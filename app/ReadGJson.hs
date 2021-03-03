import Data.Aeson
import GHC.Generics ( Generic )


data Properties = Properties
    { SIRE_ENTITY_LEVEL :: String,
      SIRE_ENTITY_SUBTYPE :: String,
      SIRE_MENTION_CLASS :: String,
      SIRE_ENTITY_CLASS :: String,
      SIRE_MENTION_ROLE :: String,
      SIRE_MENTION_TYPE :: String
    } deriving (Show, Generic)

data Mentions = Mentions
    { id :: String,
      properties :: Properties
      type :: String,
      begin :: Int,
      end :: Int,
      inCoref :: Bool
    } deriving (Show, Generic)

data Tokens = Tokens
    { id :: String,
      begin :: Int,
      end :: Int,
      text :: String,
      whiteSpace :: Bool
    } deriving (Show, Generic)

data Sentences = Sentences
    { id :: String,
      begin :: Int,
      end :: Int,
      text :: String,
      tokens :: [Tokens]
    } deriving (Show, Generic)

data Document = Document
    { id :: String,
      name :: String,
      createdDate :: Int,
      version :: Int,
      text :: String,
      docLength :: Int,
      language :: String,
      status :: String,
      modifiedDate :: Int,
      documentSet :: [String],
      preannotation :: [String],
      sentences :: [Sentences],
      mentions :: [Mentions],
      relations :: [String],
      corefs :: [String],
      typeResolved :: Bool,
      userResolved :: Bool
    } deriving (Show, Generic)

instance FromJSON Document
instance ToJSON Document

instance FromJSON Sentences
instance ToJSON Sentences

instance FromJSON Tokens
instance ToJSON Tokens

instance FromJSON Mentions
instance ToJSON Mentions

instance FromJSON Properties
instance ToJSON Properties

