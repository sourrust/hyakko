{-# LANGUAGE OverloadedStrings #-}

module Hyakko.Types where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

-- The `Sections` type is just an alias to keep type signatures short.
type Sections = [HashMap String Text]

-- Alias `Languages`, for the multiple different languages inside the
-- `languages.json` file.
type Languages = HashMap String Language

-- Better data type for language info — compared to the `Object` data type
-- in `Aeson`.
data Language =
  Language { name_     :: ByteString
           , symbol    :: ByteString
           , literate  :: Maybe Bool
           , litSymbol :: Maybe ByteString
           }

instance FromJSON Language where
  parseJSON (Object o) = Language
                     <$> o .:  "name"
                     <*> o .:  "symbol"
                     <*> o .:? "literate"
                     <*> o .:? "litSymbol"
  parseJSON _          = empty
