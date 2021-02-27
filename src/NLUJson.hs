{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

module NLUJson where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import GHC.Generics
import Data.Either
import Data.List

data Paragraph = Paragraph {
  text :: String,
  order :: Int
} deriving (Show, Generic)

data Entity = Entity
  { etype :: String
  , text :: String
  , mentions :: [Mention]
  , disambiguation :: Disambiguation
  , count :: Int
  } deriving (Show,Generic)

data Mention = Mention
  { text :: String
  , location :: [Int]
  } deriving (Show, Generic)

newtype Disambiguation = Disambiguation
  { subtype :: [String]
  } deriving (Show, Generic)

data Cargos = Cargos
  { title :: String
  , start :: Int
  , end :: Int
  } deriving (Show, Generic)

data Document = Document
 { title :: String,
   natureza :: String,
   sexo :: String,
   cargos :: [String],
   cargos_p :: [Cargos],
   filename :: String,
   text :: String,
   paragraphs :: [Paragraph],
   entities :: [Entity]
}  deriving (Show, Generic)

instance FromJSON Disambiguation
instance ToJSON Disambiguation

instance FromJSON Mention
instance ToJSON Mention

instance FromJSON Paragraph
instance ToJSON Paragraph

instance FromJSON Cargos
instance ToJSON Cargos

customEntity = defaultOptions {fieldLabelModifier = \x -> if x == "type" then "etype" else x }
instance FromJSON Entity where
  parseJSON = genericParseJSON  $ defaultOptions {fieldLabelModifier = \x -> if x == "etype" then "type" else x}
instance ToJSON Entity where
  toJSON = genericToJSON customEntity
  toEncoding = genericToEncoding customEntity

customDocument = defaultOptions {fieldLabelModifier = \x -> if x == "cargos-p" then "cargos_p" else x}
instance FromJSON Document where
  parseJSON =
    genericParseJSON $ defaultOptions {fieldLabelModifier = \x -> if x == "cargos_p" then "cargos-p" else x}
instance ToJSON Document where
  toJSON = genericToJSON customDocument
  toEncoding = genericToEncoding customDocument

emptyDocument :: Document
emptyDocument = Document "" "" "" [] [] "" "" [] []

readJSON :: FilePath -> IO Document
readJSON path = do
  doc <- (eitherDecode <$> B.readFile path) :: IO (Either String Document)
  return $ fromRight emptyDocument doc

strEntity :: [Entity] -> String
strEntity l = "[" ++ intercalate "," (map (C.unpack . encode) l) ++ "]"