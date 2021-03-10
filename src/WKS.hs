{-# LANGUAGE DeriveGeneric  #-}

module WKS where

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


 
data Properties =
  Properties
    { entity_level   :: Maybe String
    , entity_subtype :: Maybe String
    , mention_class  :: Maybe String
    , entity_class   :: Maybe String
    , mention_role   :: Maybe String
    , mention_type   :: Maybe String
    }
  deriving (Eq, Show, Generic)

customProperties :: Options
customProperties = defaultOptions {fieldLabelModifier = aux} where
  aux x | x == "mention_role"   = "SIRE_MENTION_ROLE"
        | x == "entity_subtype" = "SIRE_ENTITY_SUBTYPE"
        | x == "mention_type"   = "SIRE_MENTION_TYPE"
        | x == "mention_class"  = "SIRE_MENTION_CLASS"
        | x == "entity_level"   = "SIRE_ENTITY_LEVEL"
        | x == "entity_class"   = "SIRE_ENTITY_CLASS"
        | otherwise = x

instance FromJSON Properties where
  parseJSON = genericParseJSON customProperties
instance ToJSON Properties where
  toJSON = genericToJSON customProperties
  toEncoding = genericToEncoding customProperties


data Mentions = Mentions
    { menId      :: String,
      source     :: Maybe String,
      properties :: Properties,
      menType    :: String,
      menBegin   :: Int,
      menEnd     :: Int,
      inCoref    :: Bool
    } deriving (Eq, Show, Generic)

customMentions :: Options
customMentions = defaultOptions {fieldLabelModifier = aux} where
  aux x | x == "menId"    = "id"
        | x == "menType"  = "type"
        | x == "menBegin" = "begin"
        | x == "menEnd"   = "end"
        | otherwise = x
--instance Eq Mentions where
--  (Mentions i p t b e c) 
instance Ord Mentions where
  compare x y = compare (menBegin x) (menBegin y)
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


data Corefs = Corefs
  { corId :: Maybe String, 
    corProperties :: Maybe Properties, 
    corMentions :: Maybe [String]
  } deriving (Show, Generic)

customCorefs :: Options
customCorefs = defaultOptions {fieldLabelModifier = aux} where
  aux x | x == "corId"         = "id"
        | x == "corProperties" = "properties"
        | x == "corMentions"   = "mentions"
        | otherwise = x

instance FromJSON Corefs where
  parseJSON = genericParseJSON customCorefs
instance ToJSON Corefs where
  toJSON = genericToJSON customCorefs
  toEncoding = genericToEncoding customCorefs


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
      mentionsWKS   :: [Mentions],
      relations     :: [String],
      corefs        :: [Maybe Corefs],
      typeResolved  :: Bool,
      userResolved  :: Bool
    } deriving (Show, Generic)

customDoc :: Options
customDoc = defaultOptions {fieldLabelModifier = aux} where
  aux x | x == "docId" = "id"
        | x == "docText" = "text"
        | x == "mentionsWKS" = "mentions"
        | otherwise = x

instance FromJSON Document where
  parseJSON = genericParseJSON customDoc
instance ToJSON Document where
  toJSON = genericToJSON customDoc
  toEncoding = genericToEncoding customDoc

readGJson :: FilePath -> IO (Either String Document)
readGJson path = (eitherDecode <$> B.readFile path) :: IO (Either String Document)

 
