module Main where

import Text.Markdown

import qualified Data.Map as M
import Data.List (sort)
import Text.Pandoc.Templates
import System.Environment (getArgs)
import System.FilePath (takeBaseName)
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

languages :: M.Map String (M.Map String String)

getLanguage :: FilePath -> M.Map String String

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
  putStrLn style
  ensureDirectory $ do
    writeFile "docs/hyakko.css" style
    generateDocumentation sources
