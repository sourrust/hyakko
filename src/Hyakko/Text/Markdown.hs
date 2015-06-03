module Hyakko.Text.Markdown (toHTML) where

import Text.Pandoc ( readMarkdown
                   , writeHtmlString
                   , def
                   )

import Data.Text (Text, pack)

-- Function for translating Markdown to HTML since `Pandoc` has several
-- different generators for other markup languages.
toHTML :: String -> Text
toHTML string =
  let parsedOutput = readMarkdown def string
      emptyText _  = empty
  in either emptyText (pack . writeHtmlString def) parsedOutput
{-# INLINE toHTML #-}
