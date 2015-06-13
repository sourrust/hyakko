Hyakko
======

**Hyakko** is a Haskell port of [docco](http://jashkenas.github.com/docco/):
the original quick-and-dirty documentation generate. It produces an HTML
document that displays your comments intermingled with you code. All prose
is passed through
[Markdown](http://daringfireball.net/projects/markdown/syntax), and code is
passed through [Kate](http://johnmacfarlane.net/highlighting-kate/) syntax
highlighing. This page is the result of running Hyakko against its own
[source
file](https://github.com/sourrust/hyakko/blob/master/src/Hyakko.lhs).

1. Install Hyakko with **cabal**: `cabal update; cabal install hyakko`

2. Run it agianst your code: `hyakko src/*.hs` or just `hyakko src` and
   Hyakko will search for supported files inside the directory recursively.

There is no "Step 3". This will generate an HTML page for each of the named
source files, with a menu linking to the other pages, saving the whole mess
into a `docs` folder — and is also configurable.

The [Hyakko source](https://github.com/sourrust/hyakko) is available on
GitHub, and is released under the [MIT
license](http://opensource.org/licenses/MIT).

There is a ["literate"
style](http://www.haskell.org/haskellwiki/Literate_programming) of Haskell,
only one supported at this time, but other literate styles can be added
fairly easily via a [separate languages
file](https://github.com/sourrust/hyakko/blob/master/resources/languages.json).

> {-# LANGUAGE OverloadedStrings #-}

> module Main where

> import Hyakko.Text.Markdown
> import Hyakko.Text.Templates
> import Hyakko.Types

> import Data.Aeson (decode', Value(..))
> import qualified Data.HashMap.Strict as M
> import qualified Data.ByteString.Lazy.Char8 as L
> import Data.Text (Text)
> import qualified Data.Text as T
> import qualified Data.Text.IO as T
> import Data.List (sort)
> import Data.Maybe (fromJust, isNothing)
> import Data.Monoid
> import Data.Version (showVersion)
> import Control.Applicative ((<$>))
> import Control.Monad.State.Strict
> import qualified Text.Blaze.Html as B
> import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
> import qualified Text.Highlighting.Kate as K
> import Text.Pandoc.Templates
> import Text.Regex.PCRE ((=~))
> import System.Directory ( getDirectoryContents
>                         , doesDirectoryExist
>                         , doesFileExist
>                         , createDirectoryIfMissing
>                         , copyFile
>                         )
> import System.Console.Docopt.NoTH
> import System.Environment (getArgs)
> import System.Exit (exitSuccess)
> import System.FilePath ( takeBaseName
>                        , takeExtension
>                        , takeFileName
>                        , (</>)
>                        , addTrailingPathSeparator
>                        )
> import Paths_hyakko (getDataFileName, version, getDataDir)

Main Documentation Generation Functions
---------------------------------------

Generate the documentation for our configured source file by copyinh over
static assets, reading all the source files in, splitting them up into
prose+code sections, highlighting each file in the approapiate language, and
printing them out in an HTML template.

> generateDocumentation :: Hyakko -> [FilePath] -> IO ()
> generateDocumentation _ [] =
>   putStrLn "hyakko: no files or options given (try --help)"
> generateDocumentation opts xs = do
>   dataDir <- getDataDir
>   let options  = configHyakko opts dataDir
>       dirout   = output options
>       langFile = languages options
>   style    <- hyakkoStyles options
>   langList <- decodeLanguageFile langFile
>   T.writeFile (dirout </> "hyakko.css") style
>   unless (isNothing $ layout options) $ do
>     let layoutDir = fromJust $ layout options
>     copyDirectory options $ dataDir </> "resources" </> layoutDir
>                                     </> "public"
>   forM_ xs $ \x -> do
>     code <- T.readFile x
>     let language  = getLanguage x langList
>         sections  = parse language code
>         noSects   = null sections
>     when noSects $
>       putStrLn $ "hyakko doesn't support the language extension "
>                ++ takeExtension x
>     unless noSects $ do
>       let highlighted = highlight language sections
>           y           = mapSections sections highlighted
>       generateHTML options x y

Given a string of source code, parse out eacg block of prose and the code
that follows it — by detecting which is which, line by line — then create an
individual **section** for it. Each section is Map with `docText` and
`codeText` properties, and eventuall `docsHtml` and `codeHtml` as well.

> inSections :: [Text] -> Text -> Sections
> inSections xs r =
>   let sections = sectionOff mempty mempty xs
>   in map M.fromList sections

>   where save :: Text -> Text -> [(String, Text)]
>         save code docs = [ ("codeText", code)
>                          , ("docsText", docs)
>                          ]

>         sectionOff :: Text -> Text -> [Text] -> [[(String, Text)]]
>         sectionOff code docs [] = save code docs : []
>         sectionOff code docs (y:ys) =
>           let line    = T.unpack y
>               shebang = L.pack "(^#![/]|^\\s*#\\{)"
>               r'      = L.pack $ T.unpack r
>           in if line =~ r' && (not $ line =~ shebang) then
>                handleDocs code
>                else
>                  sectionOff (code <> y <> "\n") docs ys

>           where handleDocs "" = handleHeaders code (newdocs docs) ys
>                 handleDocs _  = save code docs
>                               : handleHeaders mempty (newdocs mempty) ys

>                 newdocs d = d <> (replace r y mempty) <> "\n"

If there is a header markup, only for `---` and `===`, it will get its own
line from the other documentation.

>                 handleHeaders c d zs =
>                   if T.unpack d =~ L.pack "^(---|===)+" then
>                     save c d : sectionOff mempty mempty zs
>                     else
>                       sectionOff c d zs

The higher level interface for calling `inSections`. `parse` basically
sanitates the file — turing literate into regular source and take out
shebangs — then feed it to `inSections`, and finally return the results.

> parse :: Maybe Language -> Text -> Sections
> parse Nothing _       = []
> parse (Just src) code =
>   inSections (fromLiterate (T.lines code) $ literate src)
>              ("^\\s*" <> symbol src <> "\\s?")

Transforms a literate style language file into its normal, non-literate
style language. If it is normal, `fromLiterate` for returns the same list of
`Text` that was passed in.

>   where fromLiterate :: [Text] -> Bool -> [Text]
>         fromLiterate [] _     = []
>         fromLiterate xs False = xs
>         fromLiterate xs True  =
>           let s  = symbol src
>               r  = "^" <> (fromJust $ litSymbol src) <> "\\s?"
>               r1 = L.pack "^\\s*$"
>               fn = forM xs $ \x -> do
>                 (ys, isText) <- get
>                 let hasLitSymbol = T.unpack x =~ (L.pack $ T.unpack r)

>                 when hasLitSymbol $
>                   put (ys ++ [replace r x mempty], False)

Inserts a comment symbol and a single space into the documentation line and
check if the last line was code and documentation. If the previous line was
code and the line is blank or has just whitespace, it returns a blank `Text`
datatype; otherwise it will return just the comment symbol.

>                 unless hasLitSymbol $
>                   case (T.unpack x =~ r1, isText) of
>                     (True, True)  -> put (ys ++ [s], True)
>                     (True, False) -> put (ys ++ [mempty], False)
>                     (False, _)    -> put (ys ++ [s <> " " <> x], True)
>           in fst . snd $ runState fn (mempty, True)

Highlights the current file of code, using **Kate**, and outputs the the
highlighted html to its caller.

> highlight :: Maybe Language -> Sections -> [Text]
> highlight language' section =
>   let language = fromJust language'
>       langName = T.unpack $ name_ language
>       input    = map (T.unpack . (M.! "codeText")) section
>       html     = B.toHtml . K.formatHtmlBlock K.defaultFormatOpts
>                           . K.highlightAs langName
>       htmlText = T.pack . L.unpack . renderHtml . html
>   in map htmlText input

`mapSections` is used to insert the html parts of the mapped sections of
text into the corresponding keys of `docsHtml` and `codeHtml`.

> mapSections :: Sections -> [Text] -> Sections
> mapSections section highlighted =
>   let docText s  = toHTML . T.unpack $ s M.! "docsText"
>       codeText i = highlighted !! i
>       sectLength = (length section) - 1
>       intoMap x  = let sect = section !! x
>                    in M.insert "docsHtml" (docText sect) $
>                       M.insert "codeHtml" (codeText x) sect
>   in map intoMap [0 .. sectLength]

Once all of the code is finished highlighting, we can generate the HTML file
and write out the documentation. Pass the completed sections into the
template found in `resources/linear/hyakko.html` or
`resources/parallel/hyakko.html`.

> generateHTML :: Hyakko -> FilePath -> Sections -> IO ()
> generateHTML opts src section = do
>   let title       = takeFileName src
>       dest        = destination (output opts) src
>       maybeLayout = layout opts
>       header      = T.unpack $ (section !! 0) M.! "docsHtml"
>       isHeader    = header =~ L.pack "^<(h\\d)"
>       count       = [0 .. (length section) - 1]
>       (h, count') = if isHeader then
>         let layout' = maybe mempty id maybeLayout
>         in ( [("header", header)]
>            , (if layout' == "linear" then tail else id) count)
>         else
>           ([("header", header)], count)
>   source <- sources $ dirOrFiles opts
>   html   <- hyakkoTemplate opts . varListToJSON $ concat
>     [ [("title", if isHeader then getHeader header else title)]
>     , h
>     , cssTemplate opts
>     , multiTemplate $ length source
>     , sourceTemplate opts source
>     , sectionTemplate section maybeLayout count'
>     ]
>   putStrLn $ "hyakko: " ++ src ++ " -> " ++ dest
>   T.writeFile dest html

Small helper to yank out the header text from an html string, if there is a
header at the top of the file.

> getHeader :: String -> String
> getHeader htmlheader =
>   let reg            = L.pack ">(.+)</h\\d>"
>       [(_:header:_)] = htmlheader =~ reg
>   in header

Helpers & Setup
---------------

Simpler type signatuted regex replace function.

> replace :: Text -> Text -> Text -> Text
> replace reg x y =
>   let str        = T.unpack x
>       reg'       = L.pack $ T.unpack reg
>       (_, _, rp) = str =~ reg' :: (String, String, String)
>   in y <> (T.pack rp)

From a `languages.json` file, transform the data into useful list of
language information inside the JSON.

> decodeLanguageFile :: FilePath -> IO (Maybe Languages)
> decodeLanguageFile = L.readFile >=> return . decode'

Search a `HashMap` of languages with file extensions as keys.

> getLanguage :: FilePath -> Maybe Languages -> Maybe Language
> getLanguage _ Nothing        = Nothing
> getLanguage src (Just langs) = M.lookup (takeExtension src) langs

Compute the destination HTML path for an input source file path. If the
source is `lib/example.hs`, the HTML will be at docs/example.html

> destination :: FilePath -> FilePath -> FilePath
> destination out fp = out </> (takeBaseName fp) ++ ".html"

The function `hyakkoFile`, used to grab the contents of either the default
css and html or a custom css and html. Then move it to the output directory.

> hyakkoFile :: String -> Hyakko -> IO Text
> hyakkoFile filetype opts = do
>   let maybeFile = (if filetype == "css" then css else template) opts
>   if isNothing maybeFile then
>     readDataFile $ "resources"
>                </> (fromJust $ layout opts)
>                </> "hyakko." ++ filetype
>     else
>       T.readFile $ fromJust maybeFile


Create the template that we will use to generate the Hyakko HTML page.

> hyakkoTemplate :: Hyakko -> Value -> IO Text
> hyakkoTemplate opts var = do
>   content <- hyakkoFile "html" opts
>   return . T.pack $ renderTemplate' (T.unpack content) var

The CSS styles we'd like to apply to the documentation.

> hyakkoStyles :: Hyakko -> IO Text
> hyakkoStyles = hyakkoFile "css"

Reads from resource path given in cabal package

> readDataFile :: FilePath -> IO Text
> readDataFile = getDataFileName >=> T.readFile

For each source file passed in as an argument, generate the documentation.

> sources :: [FilePath] -> IO [FilePath]
> sources file = do
>   files <- forM file $ \x -> do
>     isDir <- doesDirectoryExist x
>     if isDir then
>       fst <$> unpackDirectories x
>       else
>         return [x]
>   return . sort $ concat files

Turns the directory give into a list of files including all of the files in
sub-directories.

> unpackDirectories :: FilePath -> IO ([FilePath], [FilePath])
> unpackDirectories d = do
>   let reg = L.pack "[^(^\\.{1,2}$)]"
>   content <-  filter (=~ reg) <$> getDirectoryContents d
>   let content' = map (d </>) content
>   files      <- filterM doesFileExist content'
>   subdir     <- filterM doesDirectoryExist content'
>   subcontent <- fmap (\x -> (concatMap fst x, concatMap snd x))
>                      (mapM unpackDirectories subdir)
>   return (files ++ fst subcontent, subdir ++ snd subcontent)

> copyDirectory :: Hyakko -> FilePath -> IO ()
> copyDirectory opts dir = do
>   (files, dirs) <- unpackDirectories dir
>   dataDir       <- getDataDir
>   let oldLocation = T.pack . addTrailingPathSeparator $ dataDir
>                       </> "resources"
>                       </> (fromJust $ layout opts)
>       dirout      = output opts
>   createDirectoryIfMissing False $ dirout </> "public"

Create all the directories needed to put future files into.

>   forM_ dirs $ \x -> do
>     let x'   = T.pack x
>         dir' = T.unpack $ T.replace oldLocation mempty x'
>     createDirectoryIfMissing False $ dirout </> dir'

Copy all the files into the recently created directories.

>   forM_ files $ \x -> do
>     let x'   = T.pack x
>         file = dirout </> (T.unpack $ T.replace oldLocation mempty x')
>     copyFile x file

Print information and exit hyakko when a flag is present. Mostly for
`version` and `help` printing.

> whenPresentPrintAndExit :: Arguments -> String -> String -> IO ()
> whenPresentPrintAndExit arguments optName string =
>   when (arguments `isPresent` longOption optName) $
>     putStrLn string >> exitSuccess

Configuration
-------------

Default configuration **options**. If no arguments for these flags are
specifed, it will just use the ones in `defaultConfig`.

> defaultConfig :: Arguments -> IO Hyakko
> defaultConfig arguments = do
>   let argOrDefault = getArgWithDefault arguments
>   languageFile <- getDataFileName $ "resources" </> "languages.json"
>   return Hyakko
>     { layout     = Just $ "parallel" `argOrDefault` longOption "layout"
>     , output     = "docs" `argOrDefault` longOption "output"
>     , css        = getArg arguments $ longOption "css"
>     , template   = getArg arguments $ longOption "template"
>     , languages  = languageFile `argOrDefault` longOption "languages"
>     , dirOrFiles = getAllArgs arguments $ argument "file"
>     }

**Configure** this particular run of hyakko. We might use a passed-in
external template, or one of the built-in **layouts**.

> configHyakko :: Hyakko -> FilePath -> Hyakko
> configHyakko oldConfig datadir =
>   if isNothing $ template oldConfig then
>     let dir    = datadir </> "resources"
>                          </> (fromJust $ layout oldConfig)
>     in oldConfig { template = Just $ dir </> "hyakko.html"
>                  , css      = Just $ dir </> "hyakko.css"
>                  }
>     else
>       oldConfig { layout = Nothing }

Using [docopt](https://github.com/docopt/docopt.hs), define a command line
interface along with the actually usage text used for the `help` flag.

> hyakkoUsage :: IO Docopt
> hyakkoUsage = parseUsageOrExit $ unlines
>   [ ""
>   , "  Usage: hyakko [options] [<file>...]\n"
>   , "  Options:\n"
>   , "    -h, --help              display this help message"
>   , "    -V, --version           display current version"
>   , "    -L, --languages <file>  use a custom languages.json"
>   , "    -l, --layout <name>     choose a built-in layout " ++
>                                  "(parallel, linear)"
>   , "    -o, --output <path>     use a custom output path"
>   , "    -c, --css <file>        use a custom css file"
>   , "    -t, --template <file>   use a custom pandoc template"
>   ]

Finally, parse and handle certain flags then hyakko does the rest.

> main :: IO ()
> main = do
>   usage'    <- hyakkoUsage
>   arguments <- getArgs
>   options'  <- parseArgsOrExit usage' arguments
>   options   <- defaultConfig options'
>
>   whenPresentPrintAndExit options' "version" $
>     "hyakko v" ++ showVersion version
>   whenPresentPrintAndExit options' "help" $ usage usage'
>
>   source <- sources $ dirOrFiles options
>   createDirectoryIfMissing False $ output options
>   generateDocumentation options source
