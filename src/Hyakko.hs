module Main where

import Text.Markdown

import qualified Data.Map as M
import Data.List (sort)
import Text.Pandoc.Templates
import Text.Regex
import Text.Regex.Posix ((=~))
import System.Environment (getArgs)
import System.FilePath (takeBaseName, takeExtension, takeFileName)
import System.Process (system, readProcess)

type Callback  = IO ()
type Callback' = [M.Map String String] -> IO ()

generateDocumentation :: [FilePath] -> IO ()
generateDocumentation (x:xs) = do
  code <- readFile x
  let sections = parse x code
  highlight x sections $ \y -> do
    generateHTML x y
    if null xs then return () else generateDocumentation xs

parse :: FilePath -> String -> [M.Map String String]

highlight :: FilePath -> [M.Map String String] -> Callback' -> IO ()
highlight src section cb = do
  let language = getLanguage src
      options  = ["-l", language M.! "name", "-f", "html", "-O", "encoding=utf-8"]
      input = concatMap (\x -> x M.! "codeText"++(language M.! "dividerText")) section

  output <- readProcess "pygments" options inputs

  let output'   = subRegex (mkRegex highlightReplace) options input
      fragments = splitRegex (mkRegex $ languages M.! "dividerText") output'

  cb $ map (\x -> let s = section !! x
    in M.insert "docsHtml" (toHTML $ s M.! "docsText") $
       M.insert "codeHTML" (highlightStart ++ (fragments !! x) ++
         highlightEnd) s) [0..(length sections) - 1]

generateHTML :: FilePath -> [M.Map String String] -> IO ()
generateHTML src sections = do
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

languages :: M.Map String (M.Map String String)
languages = M.fromList [
            (".hs", M.fromList [
              ("name", "haskell"), ("symbol", "--"),
              ("comment", "^( |\t)*-- ?"),
              ("dividerText", "\n--DIVIDER\n"),
              ("dividerHtml", "\n*<span class=\"c1?\">--DIVIDER</span>\n")])
            ]

getLanguage :: FilePath -> M.Map String String
getLanguage src = languages M.! takeExtension src

destination :: FilePath -> FilePath
destination fp = "docs/" ++ (takeBaseName fp) ++ ".html"

ensureDirectory :: Callback -> IO ()
ensureDirectory cb = system "mkdir -p docs" >> cb

hyakkoTemplate :: [(String, String)] -> IO String
hyakkoTemplate var = readFile "resources/hyakko.html" >>=
  return . renderTemplate var

hyakkoStyles :: IO String
hyakkoStyles = readFile "resources/hyakko.css"

highlightStart, highlightEnd :: String
highlightStart   = "<div class=\"highlight\"><pre>"
highlightEnd     = "</pre></div>"
highlightReplace = highlightStart ++ "|" ++ highlightEnd

sources :: IO [FilePath]
sources = getArgs >>= return . sort

main :: IO ()
main = do
  style <- hyakkoStyles
  source <- sources
  putStrLn style
  ensureDirectory $ do
    writeFile "docs/hyakko.css" style
    generateDocumentation source
