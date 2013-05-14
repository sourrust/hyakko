{-# LANGUAGE OverloadedStrings #-}

module Hyakko.Text.Templates where

import Hyakko.Types

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T
import Data.Text (Text)
import System.FilePath (takeFileName, takeBaseName, (</>))
import Text.Regex.PCRE ((=~))

-- Determine whether or not there is a `Jump to` section.
multiTemplate :: Int -> [(String, String)]
multiTemplate 1 = []
multiTemplate _ = [("multi", "1")]

-- Produces a list of anchor tags to different files in docs. This will
-- only show up if the template support it and there are more than one
-- source file generated.
sourceTemplate :: Hyakko -> [FilePath] -> [(String, String)]
sourceTemplate opts = map source
  where source x = ("source", concat
          [ "<a class=\"source\" href=\""
          , takeFileName $ (output opts) </> (takeBaseName x) ++ ".html"
          , "\">"
          , takeFileName x
          , "</a>"
          ])

-- Depending on the layout type, `sectionTemplate` will produce the HTML
-- that will be hooked into the templates layout theme.
sectionTemplate :: Sections
                -> Maybe String
                -> [Int]
                -> [(String, String)]
sectionTemplate section layoutType count =
  let isLayout = not $ isNothing layoutType
      sections = if isLayout then layoutFn $ fromJust layoutType
                 else undefined
  in map sections count
  where layoutFn "parallel" = parallel
        layoutFn "linear"   = linear
        layoutFn _          = undefined

        -- Whenever the layout is sepecifed as **parallel**, this is the
        -- function that will generate the mapping for variable name and
        -- replacment value.
        parallel x =
          let x'   = x + 1
              sect = section !! x
              docsHtml = T.unpack $ sect M.! "docsHtml"
              codeHtml = T.unpack $ sect M.! "codeHtml"
              codeText = T.unpack $ sect M.! "codeText"
              header   = docsHtml =~ L.pack "^\\s*<(h\\d)"
              isBlank  = T.null $ replace "\\s" (T.pack codeText) ""
          in ("section", concat
             [ "<li id=\"section-"
             , show x'
             , "\"><div class=\"annotation\">"
             , "<div class=\"pilwrap"
             , if null header then "" else " for-" ++ tail header
             , "\"><a class=\"pilcrow\" href=\""
             , show x'
             , "\">&#955;</a></div>"
             , docsHtml
             , "</div>"
             , if isBlank then "" else "<div class=\"content\">"
                 ++ codeHtml ++ "</div>"
             ])
          where replace :: ByteString -> Text -> Text -> Text
                replace reg orig replacer =
                  let str  = T.unpack orig
                      (_, _, rp) = str =~ reg :: (String, String, String)
                  in T.append replacer (T.pack rp)

        -- Far simpler layout compared to **parallel**. This function gets
        -- called when the layout is marked as **linear**.
        linear x =
          let sect   = section !! x
              codeText = T.unpack $ sect M.! "codeText"
              isText = not $ null codeText
          in ("section", concat
             [ T.unpack $ sect M.! "docsHtml"
             , if isText then T.unpack $ sect M.! "codeHtml" else []
             ])

cssTemplate :: Hyakko -> [(String, String)]
cssTemplate opts =
  let maybeLayout = layout opts
      normalize   = "public" </> "stylesheets" </> "normalize.css"
      otherFile   = if isNothing maybeLayout then id else
        ([normalize] ++)
  in zip ["css", "css"] $ otherFile ["hyakko.css"]
