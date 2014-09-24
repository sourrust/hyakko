{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Hyakko.Types where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import System.Console.CmdArgs

-- The `Sections` type is just an alias to keep type signatures short.
type Sections = [HashMap String Text]

-- Alias `Languages`, for the multiple different languages inside the
-- `languages.json` file.
type Languages = HashMap String Language

-- Better data type for language info — compared to the `Object` data type
-- in `Aeson`.
data Language =
  Language { name_     :: Text
           , symbol    :: Text
           , literate  :: Maybe Bool
           , litSymbol :: Maybe Text
           }

instance FromJSON Language where
  parseJSON (Object o) = Language
                     <$> o .:  "name"
                     <*> o .:  "symbol"
                     <*> o .:? "literate"
                     <*> o .:? "litSymbol"
  parseJSON _          = empty

-- Data structure for command line argument parsing.
data Hyakko =
  Hyakko { layout     :: Maybe String
         , output     :: FilePath
         , css        :: Maybe FilePath
         , template   :: Maybe FilePath
         , dirOrFiles :: [FilePath]
         } deriving (Show, Data, Typeable)
