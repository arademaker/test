{-# LANGUAGE DeriveGeneric, BlockArguments #-}


module NER where

import qualified WKS as W
import qualified NLU as N
import Data.Aeson
import System.Exit
import System.Environment
import Data.Either
import Data.Maybe
import Data.List ( groupBy, intercalate, sortOn )
import Data.Ord (comparing)
import GHC.Generics
import Data.Char ( toLower )
import Data.Text ( unpack, pack)


data Annotation =
  Annotation
    { anType :: [String]
    , anBegin :: Int
    , anEnd :: Int
    , anSource :: [String]
    }
  deriving (Eq, Show)


-- receive json-WKS json-NLU and check if the texts are the same
checkTexts :: W.Document -> N.Document -> Bool
checkTexts x y = W.docText x == N.analyzed_text y

wksAnn :: W.Document -> [Annotation]
wksAnn doc = map aux (W.mentions doc)
  where
    aux m =
      Annotation
        { anType = [W.menType m]
        , anBegin = W.menBegin m
        , anEnd = W.menEnd m
        , anSource = ["WKS"]
        }

nluAnn :: N.Document -> [Annotation]
nluAnn doc = concatMap aux1 (N.entities doc)
  where
    aux1 ent = map (aux2 ent) (N.mentions ent)
    aux2 ent men =
      Annotation
        { anType = [N.etype ent]
        , anBegin = head $ N.location men
        , anEnd = N.location men !! 1
        , anSource = ["NLU"]
        }


merge1 :: Annotation -> Annotation -> Maybe Annotation
merge1 a b =
  if anBegin a == anBegin b && anEnd a == anEnd b
    then Just
           a
             { anType = anType a ++ anType b
             , anSource = anSource a ++ anSource b
             }
    else Nothing


-- receive annotations list of annotations from the WKS GT and NLU
-- return the consolidation of the annotations.

merge :: [Annotation] -> [Annotation] -> [Annotation]
merge [] ys = ys
merge xs [] = xs
merge x'@(x:xs) y'@(y:ys)
  | anBegin x < anBegin y = addNullSpace x : merge xs y'
  | anBegin x > anBegin y = addNullSpace y : merge x' ys
  | isJust res = fromJust res : merge xs ys
  | otherwise = x : y : merge xs ys
  where
    res = merge1 x y


-- Receive files WKS and NLU and return a list of diffs. The return
-- Nothing means that either the inputs are Left (error in the parser)
-- or the texts are different. Note that the return can also be Just
-- [], meaning that no annotation were available.

validation :: Either String N.Document -> Either String W.Document -> Either String [Annotation]
validation (Right docNLU) (Right docWKS) =
  if checkTexts docWKS docNLU
    then Right (merge aN aW)
    else Left "Texts are not equal!"
  where
    aN = sortOn anBegin (nluAnn docNLU)
    aW = sortOn anBegin (wksAnn docWKS)
validation (Left doc) _ = Left doc
validation _ (Left doc) = Left doc

-- bug: fix me!!
addNullSpace :: Annotation -> Annotation
addNullSpace ann =
  if anSource ann == ["WKS"]
    then ann {anType = "-" : anType ann, anSource = "-" : anSource ann}
    else ann {anType = anType ann ++ ["-"], anSource = anSource ann ++ ["-"]}

lineCSV :: Annotation -> String
lineCSV a = intercalate "," $ [show (anBegin a),show (anEnd a)] ++ anType a ++ anSource a

createCSV :: [String] -> IO ()
createCSV [fnNLU, fnWKS] = do
  nlu <- N.readJSON fnNLU
  wks <- W.readJSON fnWKS
  aux (validation nlu wks)
 where
   aux (Right as) = mapM_ (putStrLn . lineCSV) as
   aux (Left as) = putStrLn as



data Content = Content
  { pessoa_gpe :: [TypeSent]
  , pessoa_data :: [TypeSent]
  , pessoa_lei :: [TypeSent]
  , pessoa_instituicao :: [TypeSent]
  , pessoa_nan :: [TypeSent]
  , gpe_pessoa :: [TypeSent]
  , gpe_data :: [TypeSent]
  , gpe_lei :: [TypeSent]
  , gpe_instituicao :: [TypeSent]
  , gpe_nan :: [TypeSent]
  , data_pessoa :: [TypeSent]
  , data_gpe :: [TypeSent]
  , data_lei :: [TypeSent]
  , data_instituicao :: [TypeSent]
  , data_nan :: [TypeSent]
  , lei_pessoa :: [TypeSent]
  , lei_gpe :: [TypeSent]
  , lei_data :: [TypeSent]
  , lei_instituicao :: [TypeSent]
  , lei_nan :: [TypeSent]
  , instituicao_pessoa :: [TypeSent]
  , instituicao_gpe :: [TypeSent]
  , instituicao_data :: [TypeSent]
  , instituicao_lei :: [TypeSent]
  , instituicao_nan :: [TypeSent]
  , nan_pessoa :: [TypeSent]
  , nan_gpe :: [TypeSent]
  , nan_data :: [TypeSent]
  , nan_lei :: [TypeSent]
  , nan_instituicao :: [TypeSent]
  } deriving (Show, Generic)

instance FromJSON Content
instance ToJSON Content where
  toEncoding = genericToEncoding defaultOptions


data TypeSent = TypeSent
    { word :: String
    , sentence :: String
    , doc :: String
    , typeType  :: String
    } deriving (Show, Generic)

instance FromJSON TypeSent
instance ToJSON TypeSent where
  toEncoding = genericToEncoding defaultOptions


data Document = Document
  { table :: [Int]
  , content :: Content
  } deriving (Show, Generic)

instance FromJSON Document
instance ToJSON Document where
  toEncoding = genericToEncoding defaultOptions



-- create json

createDoc :: [String] -> FilePath -> IO()
createDoc as path = encodeFile path $createTable $ sortTypeSent (createTS as)
 where
  createTS (x:y:xs) = getSentens (N.readJSON x) (W.readJSON y) ++ createTS xs
  createTS _ = []



getSentens :: Either String N.Document -> Either String W.Document-> [TypeSent]
getSentens nlu wks = map (createType fileName text) $ fromRight [] (validation nlu wks)
 where [text,fileName] = getText wks
   
createType :: String -> String -> Annotation -> TypeSent
createType name text ann =
  TypeSent {
            doc = name
          , word = subStr (anBegin ann) (anEnd ann) text
          , sentence = subStr (anBegin ann - 10) (anEnd ann + 10 ) text
          , typeType = map toLower $ intercalate "_" $ anType ann
          } 

subStr :: Int -> Int -> String -> String
subStr a b text = take (b - a) (drop a text)

getText :: Either String W.Document -> [String]
getText (Right doc) = [W.docText doc, W.name doc]
getText _ = []


sortTypeSent :: [TypeSent] -> [[TypeSent]]
sortTypeSent ts = groupBy (\tsa tsb -> typeType tsa == typeType tsb) $ sortOn typeType ts

createTable :: [[TypeSent]] -> Document
createTable as = Document { table = tab, content = cont }
  where
    tab = map length as
    cont = Content {
        pessoa_gpe         = as !! 1
      , pessoa_data        = as !! 2
      , pessoa_lei         = as !! 3
      , pessoa_instituicao = as !! 4
      , pessoa_nan         = as !! 5
      , gpe_pessoa         = as !! 6
      , gpe_data           = as !! 8
      , gpe_lei            = as !! 9
      , gpe_instituicao    = as !! 10
      , gpe_nan            = as !! 11
      , data_pessoa        = as !! 12
      , data_gpe           = as !! 13
      , data_lei           = as !! 15
      , data_instituicao   = as !! 16
      , data_nan           = as !! 17
      , lei_pessoa         = as !! 18
      , lei_gpe            = as !! 19
      , lei_data           = as !! 20
      , lei_instituicao    = as !! 22
      , lei_nan            = as !! 23
      , instituicao_pessoa = as !! 24
      , instituicao_gpe    = as !! 25
      , instituicao_data   = as !! 26
      , instituicao_lei    = as !! 27
      , instituicao_nan    = as !! 29
      , nan_pessoa         = as !! 30
      , nan_gpe            = as !! 31
      , nan_data           = as !! 32
      , nan_lei            = as !! 33
      , nan_instituicao    = as !! 34
      }



-- main

msg = " Usage: \n\
      \  test-ner -c json-nlu json-wks  => csv file in the STDOUT \n "

usage = putStrLn msg

parse ["-h"]    = usage >> exitSuccess
parse ("-c":ls) = createCSV ls >> exitSuccess
parse _         = usage >> exitFailure

-- slice :: Int -> Int -> String -> String
-- slice a b = take (b - a) . drop a

main :: IO ()
main = do
  as <- getArgs
  parse as

