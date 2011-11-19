module Text.Markdown (toHTML) where

import Text.Pandoc
  ( readMarkdown
  , writeHtmlString
  , defaultParserState
  , defaultWriterOptions
  )

-- Function for translating Markdown to HTML since `Pandoc` has several
-- different generators for other markup languages.
toHTML :: String -> String
toHTML = writeHtmlString defaultWriterOptions . parse
  where parse = readMarkdown defaultParserState
{-# INLINE toHTML #-}
