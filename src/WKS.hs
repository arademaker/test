{-# LANGUAGE DeriveGeneric  #-}

module WKS where

import Data.Aeson
  ( FromJSON(parseJSON)
  , Options(fieldLabelModifier)
  , ToJSON(toEncoding, toJSON)
  , defaultOptions
  , eitherDecode
  , genericParseJSON
  , genericToEncoding
  , genericToJSON
  )

import Data.Either
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import GHC.Generics ( Generic )
 
data Properties =
  Properties
    { entity_level :: Maybe String
    , entity_subtype :: Maybe String
    , mention_class :: Maybe String
    , entity_class :: Maybe String
    , mention_role :: Maybe String
    , mention_type :: Maybe String
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


data Mention =
  Mention
    { menId :: String
    , source :: Maybe String
    , properties :: Properties
    , menType :: String
    , menBegin :: Int
    , menEnd :: Int
    , inCoref :: Bool
    }
  deriving (Eq, Show, Generic)

customMention :: Options
customMention = defaultOptions {fieldLabelModifier = aux}
  where
    aux x
      | x == "menId" = "id"
      | x == "menType" = "type"
      | x == "menBegin" = "begin"
      | x == "menEnd" = "end"
      | otherwise = x

--instance Eq Mentions where
--  (Mentions i p t b e c) 

instance Ord Mention where
  compare x y = compare (menBegin x) (menBegin y)

instance FromJSON Mention where
  parseJSON = genericParseJSON customMention

instance ToJSON Mention where
  toJSON = genericToJSON customMention
  toEncoding = genericToEncoding customMention


data Token = Token
    { tokId      :: String,
      tokBegin   :: Int,
      tokEnd     :: Int,
      tokText    :: String,
      whiteSpace :: Bool
    } deriving (Show, Generic)

customToken :: Options
customToken = defaultOptions {fieldLabelModifier = aux}
  where
    aux x
      | x == "tokId" = "id"
      | x == "tokBegin" = "begin"
      | x == "tokEnd" = "end"
      | x == "tokText" = "text"
      | otherwise = x

instance FromJSON Token where
  parseJSON = genericParseJSON customToken

instance ToJSON Token where
  toJSON = genericToJSON customToken
  toEncoding = genericToEncoding customToken


data Sentence = Sentence
    { senId    :: String,
      senBegin :: Int,
      senEnd   :: Int,
      senText  :: String,
      tokens   :: [Token]
    } deriving (Show, Generic)

customSent :: Options
customSent = defaultOptions {fieldLabelModifier = aux}
  where
    aux x
      | x == "senId" = "id"
      | x == "senBegin" = "begin"
      | x == "senEnd" = "end"
      | x == "senText" = "text"
      | otherwise = x

instance FromJSON Sentence where
  parseJSON = genericParseJSON customSent

instance ToJSON Sentence where
  toJSON = genericToJSON customSent
  toEncoding = genericToEncoding customSent

data Relation =
  Relation
    { relId :: String
    , relType :: String
    , args :: [String]
    }
    deriving (Show, Generic)


instance FromJSON Relation where
  parseJSON = genericParseJSON customRelation

instance ToJSON Relation where
  toJSON = genericToJSON customCoref
  toEncoding = genericToEncoding customRelation

customRelation :: Options
customRelation = defaultOptions {fieldLabelModifier = aux}
  where
    aux x
      | x == "relId" = "id"
      | x == "relType" = "type"
      | otherwise = x  

data Coref =
  Coref
    { corId :: String
    , corMentions :: [String]
    }
  deriving (Show, Generic)

customCoref :: Options
customCoref = defaultOptions {fieldLabelModifier = aux}
  where
    aux x
      | x == "corId" = "id"
      | x == "corMentions" = "mentions"
      | otherwise = x

instance FromJSON Coref where
  parseJSON = genericParseJSON customCoref

instance ToJSON Coref where
  toJSON = genericToJSON customCoref
  toEncoding = genericToEncoding customCoref


data Document =
  Document
    { docId :: String
    , name :: String
    , createdDate :: Int
    , version :: Int
    , docText :: String
    , docLength :: Int
    , language :: String
    , status :: String
    , modifiedDate :: Int
    , documentSet :: [String]
    , preannotation :: [String]
    , sentences :: [Sentence]
    , mentions :: [Mention]
    , relations :: [Relation] 
    , corefs :: [Coref]
    , typeResolved :: Bool
    , userResolved :: Bool
    }
  deriving (Show, Generic)

customDoc :: Options
customDoc = defaultOptions {fieldLabelModifier = aux}
  where
    aux x
      | x == "docId" = "id"
      | x == "docText" = "text"
      | x == "mentionsWKS" = "mentions"
      | otherwise = x

instance FromJSON Document where
  parseJSON = genericParseJSON customDoc

instance ToJSON Document where
  toJSON = genericToJSON customDoc
  toEncoding = genericToEncoding customDoc


readJSON :: FilePath -> IO (Either String Document)
readJSON path = (eitherDecode <$> B.readFile path) :: IO (Either String Document)
