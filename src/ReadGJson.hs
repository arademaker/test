{-# LANGUAGE DeriveGeneric #-}

module ReadGJson where

import Data.Aeson
    ( eitherDecode,
      genericParseJSON,
      defaultOptions,
      genericToEncoding,
      genericToJSON,
      FromJSON(parseJSON),
      Options(fieldLabelModifier),
      ToJSON(toJSON, toEncoding) )
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import GHC.Generics ( Generic )


 
data Properties = Properties
    { sire_ENTITY_LEVEL   :: Maybe String,
      sire_ENTITY_SUBTYPE :: String,
      sire_MENTION_CLASS  :: String,
      sire_ENTITY_CLASS   :: Maybe String,
      sire_MENTION_ROLE   :: String,
      sire_MENTION_TYPE   :: String
    } deriving (Show,Generic)

customProperties :: Options
customProperties = defaultOptions {fieldLabelModifier = aux} where
  aux x | x == "sire_MENTION_ROLE"   = "SIRE_MENTION_ROLE"
        | x == "sire_ENTITY_SUBTYPE" = "SIRE_ENTITY_SUBTYPE"
        | x == "sire_MENTION_TYPE"   = "SIRE_MENTION_TYPE"
        | x == "sire_MENTION_CLASS"  = "SIRE_MENTION_CLASS"
        | x == "sire_ENTITY_LEVEL"   = "SIRE_ENTITY_LEVEL"
        | x == "sire_ENTITY_CLASS"   = "SIRE_ENTITY_CLASS"
        | otherwise = x

instance FromJSON Properties where
  parseJSON = genericParseJSON customProperties
instance ToJSON Properties where
  toJSON = genericToJSON customProperties
  toEncoding = genericToEncoding customProperties


data Mentions = Mentions
    { menId      :: String,
      properties :: Properties,
      menType    :: String,
      menBegin   :: Int,
      menEnd     :: Int,
      inCoref    :: Bool
    } deriving (Show, Generic)

customMentions :: Options
customMentions = defaultOptions {fieldLabelModifier = aux} where
  aux x | x == "menId"    = "id"
        | x == "menType"  = "type"
        | x == "menBegin" = "begin"
        | x == "menEnd"   = "end"
        | otherwise = x

instance FromJSON Mentions where
  parseJSON = genericParseJSON customMentions
instance ToJSON Mentions where
  toJSON = genericToJSON customMentions
  toEncoding = genericToEncoding customMentions


data Tokens = Tokens
    { tokId      :: String,
      tokBegin   :: Int,
      tokEnd     :: Int,
      tokText    :: String,
      whiteSpace :: Bool
    } deriving (Show, Generic)

customTokens :: Options
customTokens = defaultOptions {fieldLabelModifier = aux} where
  aux x | x == "tokId"    = "id"
        | x == "tokBegin" = "begin"
        | x == "tokEnd"   = "end"
        | x == "tokText"  = "text"
        | otherwise = x

instance FromJSON Tokens where
  parseJSON = genericParseJSON customTokens
instance ToJSON Tokens where
  toJSON = genericToJSON customTokens
  toEncoding = genericToEncoding customTokens


data Sentences = Sentences
    { senId    :: String,
      senBegin :: Int,
      senEnd   :: Int,
      senText  :: String,
      tokens   :: [Tokens]
    } deriving (Show, Generic)

customSent :: Options
customSent = defaultOptions {fieldLabelModifier = aux} where
  aux x | x == "senId"    = "id"
        | x == "senBegin" = "begin"
        | x == "senEnd"   = "end"
        | x == "senText"  = "text"
        | otherwise = x

instance FromJSON Sentences where
  parseJSON = genericParseJSON customSent
instance ToJSON Sentences where
  toJSON = genericToJSON customSent
  toEncoding = genericToEncoding customSent


data Document = Document
    { docId         :: String,
      name          :: String,
      createdDate   :: Int,
      version       :: Int,
      docText       :: String,
      docLength     :: Int,
      language      :: String,
      status        :: String,
      modifiedDate  :: Int,
      documentSet   :: [String],
      preannotation :: [String],
      sentences     :: [Sentences],
      mentions      :: [Mentions],
      relations     :: [String],
      corefs        :: [String],
      typeResolved  :: Bool,
      userResolved  :: Bool
    } deriving (Show, Generic)

customDoc :: Options
customDoc = defaultOptions {fieldLabelModifier = aux} where
  aux x | x == "docId" = "id"
        | x == "docText" = "text"
        | otherwise = x

instance FromJSON Document where
  parseJSON = genericParseJSON customDoc
instance ToJSON Document where
  toJSON = genericToJSON customDoc
  toEncoding = genericToEncoding customDoc

readGJson :: FilePath -> IO (Either String Document)
readGJson path = (eitherDecode <$> B.readFile path) :: IO (Either String Document)

 
