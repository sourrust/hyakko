-- **Hyakko** is a Haskell port of [docco](http://jashkenas.github.com/docco/):
-- the original quick-and-dirty, hundred-line-line, literate-programming-style
-- documentation generator. It produces HTML that displays your comments
-- alongside your code. Comments are passed through
-- [Markdown](http://daringfireball.net/projects/markdown/syntax) and code is
-- passed through [Pygments](http://pygments.org/) syntax highlighting.
-- This page is the result of running Hyakko against its own source file.
--
-- If you install Hyakko, you can run it from the command-line:
--
--      hyakko src/*.hs
--
-- ...will generate linked HTML documentation for the named source files, saving
-- it into a `docs` folder.
module Main where

import Text.Markdown

import qualified Data.Map as M
import Data.List (sort, groupBy)
import Text.Pandoc.Templates
import Text.Regex
import Text.Regex.Posix ((=~))
import System.Environment (getArgs)
import System.FilePath (takeBaseName, takeExtension, takeFileName)
import System.Process (system, readProcess)

-- ### Main Documentation Generation Functions

-- Make type signature more readable with these two `Callback` types.
type Callback  = IO ()
type Callback' = [M.Map String String] -> IO ()

-- Generate the documentation for a source file by reading it in, splitting it
-- up into comment/code sections, highlighting them for the appropriate language,
-- and merging them into an HTML template.
generateDocumentation :: [FilePath] -> IO ()
generateDocumentation (x:xs) = do
  code <- readFile x
  let sections = parse x code
  highlight x sections $ \y -> do
    generateHTML x y
    if null xs then return () else generateDocumentation xs

-- Given a string of source code, parse out each comment and the code that
-- follows it, and create an individual **section** for it.
-- Sections take the form:
--
--     {
--       docs_text: ...
--       docs_html: ...
--       code_text: ...
--       code_html: ...
--     }
--
inSections :: [String] -> String -> [M.Map String String]
inSections xs r =
  let s1 = groupBy (\x y ->
             and $ map (\z ->
               z =~ r) [x, y]) xs
      s2 = groupBy (\x y ->
             and $ map (\z ->
               not $ z !! 0 =~ r) [x , y]) s1
      s3 = let s = map (\x -> concat x) s2
        in if even $ length s then s else [""]:s
      replace = unlines . map (\y -> subRegex (mkRegex r) y "")
      clump (x:y:ys) = [("docsText", replace x),("codeText", unlines y)] :
        if null ys then [] else clump ys
  in [M.fromList l | l <- clump s3]

parse :: FilePath -> String -> [M.Map String String]
parse src code =
  let line     = filter (\x -> "#!" /= take 2 x) $ lines code
      language = getLanguage src
  in inSections line $ language M.! "comment"

-- Highlights a single chunk of Haskell code, using **Pygments** over stdio,
-- and runs the text of its corresponding comment through **Markdown**, using the
-- Markdown translator in **[Pandoc](http://johnmacfarlane.net/pandoc/)**.
--
-- We process the entire file in a single call to Pygments by inserting little
-- marker comments between each section and then splitting the result string
-- wherever our markers occur.
highlight :: FilePath -> [M.Map String String] -> Callback' -> IO ()
highlight src section cb = do
  let language = getLanguage src
      options  = ["-l", language M.! "name", "-f", "html", "-O", "encoding=utf-8"]
      input = concatMap (\x -> x M.! "codeText"++(language M.! "dividerText")) section

  output <- readProcess "pygmentize" options input

  let output'   = subRegex (mkRegex highlightReplace) output ""
      fragments = splitRegex (mkRegex $ language M.! "dividerHtml") output'

  cb $ map (\x -> let s = section !! x
    in M.insert "docsHtml" (toHTML $ s M.! "docsText") $
       M.insert "codeHtml" (highlightStart ++ (fragments !! x) ++
         highlightEnd) s) [0..(length section) - 1]

-- Once all of the code is finished highlighting, we can generate the HTML file
-- and write out the documentation. Pass the completed sections into the template
-- found in `resources/hyakko.html`
generateHTML :: FilePath -> [M.Map String String] -> IO ()
generateHTML src section = do
  let title = takeFileName src
      dest  = destination src
  source <- sources
  html <- hyakkoTemplate $ concat [
    [("title", title)],
    if length source > 1 then [("multi","1")] else [],
    map (\x -> ("source", unlines [
      "<a class='source' href='"++(takeFileName $ destination x)++"'>",
      "  "++takeFileName x,"</a>"])) source,
    map (\x -> ("section", unlines [
      "<tr id='section-"++show (x + 1)++"'>",
      "  <td class='docs'>",
      "    <div class='pilwrap'>",
      "      <a class='pilcrow' href='#section-"++show (x + 1)++"'>&#955;</a>",
      "    </div>",
      (section !! x) M.! "docsHtml",
      "  </td>",
      "  <td class='code'>",
      (section !! x) M.! "codeHtml",
      "  </td>",
      "</tr>" ])) [0..(length section) - 1]
    ]
  putStrLn $ "hyakko: " ++ src ++ " -> " ++ dest
  writeFile dest html

-- ### Helpers & Setup

-- A list of the languages that Hyakko supports, mapping the file extension to
-- the name of the Pygments lexer and the symbol that indicates a comment. To
-- add another language to Hyakko's repertoire, add it here.
languages :: M.Map String (M.Map String String)
languages = M.fromList [
            (".hs", M.fromList [
              ("name", "haskell"), ("symbol", "--"),
              ("comment", "^( |\t)*-- ?"),
              ("dividerText", "\n--DIVIDER\n"),
              ("dividerHtml", "\n*<span class=\"c1?\">--DIVIDER</span>\n")])
            ]

-- Get the current language we're documenting, based on the extension.
getLanguage :: FilePath -> M.Map String String
getLanguage src = languages M.! takeExtension src

-- Compute the destination HTML path for an input source file path. If the source
-- is `lib/example.hs`, the HTML will be at docs/example.html
destination :: FilePath -> FilePath
destination fp = "docs/" ++ (takeBaseName fp) ++ ".html"

-- Ensure that the destination directory exists.
ensureDirectory :: Callback -> IO ()
ensureDirectory cb = system "mkdir -p docs" >> cb

-- Create the template that we will use to generate the Hyakko HTML page.
hyakkoTemplate :: [(String, String)] -> IO String
hyakkoTemplate var = readFile "resources/hyakko.html" >>=
  return . renderTemplate var

-- The CSS styles we'd like to apply to the documentation.
hyakkoStyles :: IO String
hyakkoStyles = readFile "resources/hyakko.css"

-- The start and end of each Pygments highlight block.
highlightStart, highlightEnd :: String
highlightStart   = "<div class=\"highlight\"><pre>"
highlightEnd     = "</pre></div>"
highlightReplace = highlightStart ++ "|" ++ highlightEnd

-- For each source file passed in as an argument, generate the documentation.
sources :: IO [FilePath]
sources = getArgs >>= return . sort

-- Run the script.
main :: IO ()
main = do
  style <- hyakkoStyles
  source <- sources
  ensureDirectory $ do
    writeFile "docs/hyakko.css" style
    generateDocumentation source
