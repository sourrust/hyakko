module Text.Markdown (toHTML) where

import Text.Pandoc
  ( readMarkdown
  , writeHtmlString
  , ReaderOptions
  , WriterOptions
  , def
  )

import Data.ByteString.Lazy.Char8 (ByteString, pack)

readMD = readMarkdown def
writeHTMLStr = writeHtmlString def
-- Function for translating Markdown to HTML since `Pandoc` has several
-- different generators for other markup languages.
toHTML :: String -> ByteString
toHTML = pack . writeHTMLStr . parse
  where parse = readMD 
{-# INLINE toHTML #-}
