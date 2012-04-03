module Text.Markdown (toHTML) where

import Text.Pandoc
  ( readMarkdown
  , writeHtmlString
  , defaultParserState
  , defaultWriterOptions
  )
import Data.ByteString.Lazy.Char8 (ByteString, pack)

-- Function for translating Markdown to HTML since `Pandoc` has several
-- different generators for other markup languages.
toHTML :: String -> ByteString
toHTML = pack . writeHtmlString defaultWriterOptions . parse
  where parse = readMarkdown defaultParserState
{-# INLINE toHTML #-}
