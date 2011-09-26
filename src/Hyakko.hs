module Main where

import Text.Markdown

import qualified Data.Map as M
import Data.List (sort)
import Text.Pandoc.Templates
import System.Environment (getArgs)
import System.FilePath (takeBaseName, takeExtension, takeFileName)
import System.Process (system)

type Callback = IO ()

generateDocumentation :: [FilePath] -> IO ()
generateDocumentation (x:xs) = do
  code <- readFile x
  let sections = parse x code
  highlight x sections $ do
    generateHTML x sections
    if null xs then return () else generateDocumentation xs

parse :: FilePath -> String -> [M.Map String String]

highlight :: FilePath -> [M.Map String String] -> Callback -> IO ()

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
              ("name", "haskell"), ("symbol", "--")])
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
highlightStart = "<div class=\"highlight\"><pre>"
highlightEnd = "</pre></div>"

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
