**Hyakko** is a Haskell port of [docco](http://jashkenas.github.com/docco/):
the original quick-and-dirty, hundred-line-line, literate-programming-style
documentation generator. It produces HTML that displays your comments
alongside your code. Comments are passed through
[Markdown](http://daringfireball.net/projects/markdown/syntax) and code is
passed through [Kate](http://johnmacfarlane.net/highlighting-kate/) syntax
highlighting. This page is the result of running Hyakko against its own
source file.

If you install Hyakko, you can run it from the command-line:

    hyakko src/*.hs

or just specify a directory and Hyakko will search for supported files
inside the directory recursively.

Then it will generate linked HTML documentation for the named source files,
saving it into a `docs` folder. The [source for
Hyakko](https://github.com/sourrust/hyakko) available on GitHub.

To install Hyakko

    git clone git://github.com/sourrust/hyakko.git
    cd hyakko
    cabal install

or

    cabal update
    cabal install hyakko

> {-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

> module Main where

> import Text.Markdown

> import Data.Map (Map)
> import qualified Data.Map as M
> import Data.ByteString.Lazy.Char8 (ByteString)
> import qualified Data.ByteString.Lazy.Char8 as L
> import Data.Text (Text)
> import qualified Data.Text as T
> import qualified Data.Text.IO as T
> import Data.List (sort)
> import Data.Maybe (fromJust)
> import Control.Monad (filterM, (>=>), forM)
> import qualified Text.Blaze.Html as B
> import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
> import qualified Text.Highlighting.Kate as K
> import Text.Pandoc.Templates
> import Text.Regex.PCRE ((=~))
> import System.Console.CmdArgs
> import System.Directory ( getDirectoryContents
>                         , doesDirectoryExist
>                         , doesFileExist
>                         , createDirectoryIfMissing
>                         )
> import System.FilePath ( takeBaseName
>                        , takeExtension
>                        , takeFileName
>                        , (</>)
>                        )
> import Paths_hyakko (getDataFileName)

Main Documentation Generation Functions
---------------------------------------

Infix functions for easier concatenation with Text and ByteString.

> (++.) :: Text -> Text -> Text
> (++.) = T.append
> {-# INLINE (++.) #-}

> (++*) :: ByteString -> ByteString -> ByteString
> (++*) = L.append
> {-# INLINE (++*) #-}

Simpler type signatuted regex replace function.

> replace :: ByteString -> Text -> Text -> Text
> replace reg x y =
>   let str  = T.unpack x
>       (_, _, rp) = str =~ reg :: (String, String, String)
>   in y ++. (T.pack rp)

Generate the documentation for a source file by reading it in, splitting it
up into comment/code sections, highlighting them for the appropriate
language, and merging them into an HTML template.

> generateDocumentation :: Hyakko -> [FilePath] -> IO ()
> generateDocumentation opts xs = mapM_ generate xs
>   where generate :: FilePath -> IO ()
>         generate x = do
>           code <- T.readFile x
>           let sections = parse (getLanguage x) code
>           if null sections then
>             putStrLn $ "hyakko doesn't support the language extension "
>                      ++ takeExtension x
>             else do
>               let highlighted = highlight x sections
>                   y      = mapSections sections highlighted
>               generateHTML opts x y

Given a string of source code, parse out each comment and the code that
follows it, and create an individual **section** for it. Sections take the
form:

    [
      ("docsText", ...),
      ("docsHtml", ...),
      ("codeText", ...),
      ("codeHtml", ...)
    ]

> inSections :: [Text]
>            -> ByteString
>            -> [Map String Text]
> inSections xs r =
>   let sections = sectionOff "" "" xs
>   in map M.fromList sections

>   where sectionOff :: Text -> Text -> [Text] -> [[(String, Text)]]
>         sectionOff code docs [] = [ ("codeText", code)
>                                   , ("docsText", docs)
>                                   ] : []
>         sectionOff code docs (y:ys) =
>           if T.unpack y =~ r then
>             handleDocs code
>             else
>               sectionOff (code ++. y ++. "\n") docs ys

>           where handleDocs "" = handleHeaders code (newdocs docs) ys
>                 handleDocs _  = [ ("codeText", code)
>                                 , ("docsText", docs)
>                                 ] : handleHeaders "" (newdocs "") ys

>                 newdocs d = d ++. (replace r y "") ++. "\n"

If there is a header markup, only for `---` and `===`, it will get its own
line from the other documentation.

>                 handleHeaders c d zs =
>                   if T.unpack d =~ L.pack "^(---|===)+" then
>                     [ ("codeText", c)
>                     , ("docsText", d)
>                     ] : sectionOff "" "" zs
>                     else
>                       sectionOff c d zs

> parse :: Maybe (Map String ByteString) -> Text -> [Map String Text]
> parse Nothing _       = []
> parse (Just src) code =
>   inSections (newlines line (M.lookup "literate" src) True)
>              (src M.! "comment")
>   where line :: [Text]
>         line = filter ((/=) "#!" . T.take 2) $ T.lines code

Transforms a literate style language file into its normal, non-literate
style language. If it is normal, `newlines` for returns the same list of
`Text` that was passed in.

>         newlines :: [Text] -> Maybe ByteString -> Bool -> [Text]
>         newlines [] _ _            = []
>         newlines xs Nothing _      = xs
>         newlines (x:xs) lit isText =
>           let s       = src M.! "symbol"
>               r       = "^" ++* (src M.! "symbol2") ++* "\\s?"
>               r1      = L.pack "^\\s*$"
>               (x', y) = if T.unpack x =~ r then
>                      (replace r x "", False)
>                      else
>                        insert (T.unpack x =~ r1) isText
>                          ((T.pack $ L.unpack s)  ++. " " ++. x)
>           in x': newlines xs lit y

Inserts a comment symbol and a single space into the documentation line and
check if the last line was code and documentation. If the previous line was
code and the line is blank or has just whitespace, it returns a blank `Text`
datatype; otherwise it will return just the comment symbol.

>           where insert :: Bool -> Bool -> Text -> (Text, Bool)
>                 insert True True _  = (T.pack . L.unpack
>                                         $ src M.! "symbol", True)
>                 insert True False _ = ("", False)
>                 insert False _ y    = (y, True)

Highlights a single chunk of Haskell code, using **Kate**, and runs the text
of its corresponding comment through **Markdown**, using the Markdown
translator in **[Pandoc](http://johnmacfarlane.net/pandoc/)**.

> highlight :: FilePath -> [Map String Text] -> [Text]
> highlight src section =
>   let language = fromJust $ getLanguage src
>       langName = L.unpack $ language M.! "name"
>       input    = map (\x -> T.unpack $ x M.! "codeText") section
>       html     = B.toHtml . K.formatHtmlBlock K.defaultFormatOpts
>                           . K.highlightAs langName
>       htmlText = T.pack . L.unpack . renderHtml . html
>   in map htmlText input

`mapSections` is used to insert the html parts of the mapped sections of
text into the corresponding keys of `docsHtml` and `codeHtml`.

> mapSections :: [Map String Text] -> [Text] -> [Map String Text]
> mapSections section highlighted =
>   let docText s  = toHTML . T.unpack $ s M.! "docsText"
>       codeText i = highlighted !! i
>       sectLength = (length section) - 1
>       intoMap x  = let sect = section !! x
>                    in M.insert "docsHtml" (docText sect) $
>                       M.insert "codeHtml" (codeText x) sect
>   in map intoMap [0 .. sectLength]

Determine whether or not there is a `Jump to` section

> multiTemplate :: Int -> [(String, String)]
> multiTemplate 1 = []
> multiTemplate _ = [("multi", "1")]

Produces a list of anchor tags to different files in docs

    <a class="source" href="$href-link$">$file-name$</a>

> sourceTemplate :: [FilePath] -> [(String, String)]
> sourceTemplate = map source
>   where source x = ("source", concat
>           [ "<a class=\"source\" href=\""
>           , takeFileName $ destination x
>           , "\">"
>           , takeFileName x
>           , "</a>"
>           ])

Produces a list of table rows that split up code and documentation

    <tr id="section-$number$">
      <td class="docs">
        <div class="pilwrap">
          <a class="pilcrow" href="#section-$number$">λ</a>
        </div>
        $doc-html$
      </td>
      <td class="code">
        $code-html$
      </td>
    </tr>

> sectionTemplate :: [Map String Text]
>                 -> [Int]
>                 -> [(String, String)]
> sectionTemplate section = map sections
>   where sections x =
>           let x'   = x + 1
>               sect = section !! x
>           in ("section", concat
>              [ "<tr id=\"section-"
>              ,  show x'
>              ,  "\"><td class=\"docs\">"
>              , "<div class=\"pilwrap\">"
>              , "<a class=\"pilcrow\" href=\"#section-"
>              , show x'
>              , "\">&#955;</a></div>"
>              , T.unpack $ sect M.! "docsHtml"
>              , "</td><td class=\"code\">"
>              , T.unpack $ sect M.! "codeHtml"
>              , "</td></tr>"
>              ])

Once all of the code is finished highlighting, we can generate the HTML file
and write out the documentation. Pass the completed sections into the
template found in `resources/hyakko.html`

> generateHTML :: FilePath -> [Map String Text] -> IO ()
> generateHTML src section = do
>   let title = takeFileName src
>       dest  = destination src
>   source <- sources
>   html <- hyakkoTemplate $ concat
>     [ [("title", title)]
>     , multiTemplate $ length source
>     , sourceTemplate source
>     , sectionTemplate section [0 .. (length section) - 1]
>     ]
>   putStrLn $ "hyakko: " ++ src ++ " -> " ++ dest
>   T.writeFile dest html

Helpers & Setup
---------------

A list of the languages that Hyakko supports, mapping the file extension to
the name of the Pygments lexer and the symbol that indicates a comment. To
add another language to Hyakko's repertoire, add it here.

> languages :: Map String (Map String ByteString)
> languages =
>   let hashSymbol = ("symbol", "#")
>       language   = M.fromList [
>           (".hs", M.fromList [
>             ("name", "haskell"), ("symbol", "--")]),
>           (".lhs", M.fromList [
>             ("name", "haskell"), ("symbol", "--"),
>             ("literate", "True"), ("symbol2", ">")]),
>           (".coffee", M.fromList [
>             ("name", "coffee-script"), hashSymbol]),
>           (".js", M.fromList [
>             ("name", "javascript"), ("symbol", "//")]),
>           (".py", M.fromList [
>             ("name", "python"), hashSymbol]),
>           (".rb", M.fromList [
>             ("name", "ruby"), hashSymbol])
>           ]

Does the line begin with a comment?

>       hasComments symbol = "^\\s*" ++* symbol ++*  "\\s?"
>       intoMap lang = M.insert "comment"
>                               (hasComments $ lang M.! "symbol")
>                               lang

Build out the appropriate matchers and delimiters for each language.

>  in M.map intoMap language

Get the current language we're documenting, based on the extension.

> getLanguage :: FilePath -> Maybe (Map String ByteString)
> getLanguage src = M.lookup (takeExtension src) languages

Compute the destination HTML path for an input source file path. If the
source is `lib/example.hs`, the HTML will be at docs/example.html

> destination :: FilePath -> FilePath
> destination fp = "docs" </> (takeBaseName fp) ++ ".html"

Create the template that we will use to generate the Hyakko HTML page.

> hyakkoTemplate :: [(String, String)] -> IO Text
> hyakkoTemplate var = readDataFile "resources/hyakko.html" >>=
>   return . T.pack . renderTemplate var . T.unpack

The CSS styles we'd like to apply to the documentation.

> hyakkoStyles :: Maybe FilePath -> IO Text
> hyakkoStyles Nothing    = readDataFile "resources/hyakko.css"
> hyakkoStyles (Just file) = T.readFile file

Reads from resource path given in cabal package

> readDataFile :: FilePath -> IO Text
> readDataFile = getDataFileName >=> T.readFile

For each source file passed in as an argument, generate the documentation.

> sources :: [FilePath] -> IO [FilePath]
> sources file = do
>   files <- forM file $ \x -> do
>     isDir <- doesDirectoryExist x
>     if isDir then
>       unpackDirectories x
>       else
>         return [x]
>   return . sort $ concat files

Turns the directory give into a list of files including all of the files in
sub-directories.

> unpackDirectories :: FilePath -> IO [FilePath]
> unpackDirectories d = do
>   let reg = "[^(^\\.{1,2}$)]" :: ByteString
>   content <- getDirectoryContents d >>= return . filter (=~ reg)
>   let content' = map (d </>) content
>   files <- filterM doesFileExist content'
>   subdir <- filterM doesDirectoryExist content'
>   subcontent <- mapM unpackDirectories subdir >>= return . concat
>   return (files ++ subcontent)

Configuration
-------------

Data structure for command line argument parsing.

> data Hyakko =
>   Hyakko { output     :: FilePath
>          , css        :: Maybe FilePath
>          , template   :: Maybe FilePath
>          , dirOrFiles :: [FilePath]
>          } deriving (Show, Data, Typeable)

Default configuration **options**. If no arguments for these flags are
specifed, it will just use the ones in `defaultConfig`.

> defaultConfig :: Hyakko
> defaultConfig = Hyakko
>   { output     = "docs"
>   , css        = Nothing
>   , template   = Nothing
>   , dirOrFiles = [] &= args &= typ "FILES/DIRS"
>   }

Run the script.

> main :: IO ()
> main = do
>   opts <- cmdArgs defaultConfig
>   style <- hyakkoStyles $ css opts
>   source <- sources $ dirOrFiles opts
>   let dirout = output opts
>   createDirectoryIfMissing False dirout
>   T.writeFile (dirout </> "hyakko.css") style
>   generateDocumentation opts source
