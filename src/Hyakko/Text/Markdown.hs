module Text.Markdown (toHTML) where

import Text.Pandoc ( readMarkdown
                   , writeHtmlString
                   , def
                   )

import Data.Text (Text, pack)

-- Function for translating Markdown to HTML since `Pandoc` has several
-- different generators for other markup languages.
toHTML :: String -> Text
toHTML = pack . writeHTMLStr . parse
  where parse = readMarkdown def
        writeHTMLStr = writeHtmlString def
{-# INLINE toHTML #-}
