module Text.Markdown (toHTML) where

import Text.Pandoc
  ( Pandoc
  , readMarkdown
  , writeHtmlString
  , def
  )

import Data.ByteString.Lazy.Char8 (ByteString, pack)

writeHTMLStr :: Pandoc -> String
writeHTMLStr = writeHtmlString def
-- Function for translating Markdown to HTML since `Pandoc` has several
-- different generators for other markup languages.
toHTML :: String -> ByteString
toHTML = pack . writeHTMLStr . parse
  where parse = readMarkdown def
{-# INLINE toHTML #-}
