module Text.Markdown (toHTML) where

import Text.Pandoc
  ( readMarkdown
  , writeHtmlString
  , defaultParserState
  , defaultWriterOptions
  )

toHTML :: String -> String
toHTML s =
  let parse = readMarkdown defaultParserState s
  in writeHtmlString defaultWriterOptions parse
