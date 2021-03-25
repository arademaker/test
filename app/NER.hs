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

validation :: N.Document -> W.Document -> Either String [Annotation]
validation docNLU docWKS =
  if checkTexts docWKS docNLU
    then Right (merge aN aW)
    else Left "Texts are not equal!"
  where
    aN = sortOn anBegin (nluAnn docNLU)
    aW = sortOn anBegin (wksAnn docWKS)
--validation (Left doc) _ = Left doc
--validation _ (Left doc) = Left doc

-- bug: fix me!!
addNullSpace :: Annotation -> Annotation
addNullSpace ann =
  if anSource ann == ["WKS"]
    then ann {anType = "nan" : anType ann, anSource = "nan" : anSource ann}
    else ann {anType = anType ann ++ ["nan"], anSource = anSource ann ++ ["nan"]}

lineCSV :: Annotation -> String
lineCSV a = intercalate "," $ [show (anBegin a),show (anEnd a)] ++ anType a ++ anSource a

{-
createCSV :: [String] -> IO ()
createCSV [fnNLU, fnWKS] = do
  nlu <- N.readJSON fnNLU
  wks <- W.readJSON fnWKS
  aux (validation nlu wks)
 where
   aux (Right as) = mapM_ (putStrLn . lineCSV) as
   aux (Left as) = putStrLn as
-}


data Content = Content
  { pessoa_gpe :: [TypeSent]
  , pessoa_data :: [TypeSent]
  , pessoa_lei :: [TypeSent]
  , pessoa_instituicao :: [TypeSent]
  , pessoa_nan :: [TypeSent]
  , pessoa_periodo :: [TypeSent]
  , gpe_pessoa :: [TypeSent]
  , gpe_data :: [TypeSent]
  , gpe_lei :: [TypeSent]
  , gpe_instituicao :: [TypeSent]
  , gpe_nan :: [TypeSent]
  , gpe_periodo :: [TypeSent]
  , data_pessoa :: [TypeSent]
  , data_gpe :: [TypeSent]
  , data_lei :: [TypeSent]
  , data_instituicao :: [TypeSent]
  , data_nan :: [TypeSent]
  , data_periodo :: [TypeSent]
  , lei_pessoa :: [TypeSent]
  , lei_gpe :: [TypeSent]
  , lei_data :: [TypeSent]
  , lei_instituicao :: [TypeSent]
  , lei_nan :: [TypeSent]
  , lei_periodo :: [TypeSent]
  , instituicao_pessoa :: [TypeSent]
  , instituicao_gpe :: [TypeSent]
  , instituicao_data :: [TypeSent]
  , instituicao_lei :: [TypeSent]
  , instituicao_nan :: [TypeSent]
  , instituicao_periodo :: [TypeSent]
  , nan_pessoa :: [TypeSent]
  , nan_gpe :: [TypeSent]
  , nan_data :: [TypeSent]
  , nan_lei :: [TypeSent]
  , nan_instituicao :: [TypeSent]
  , nan_periodo :: [TypeSent]
  , periodo_pessoa :: [TypeSent]
  , periodo_gpe :: [TypeSent]
  , periodo_data :: [TypeSent]
  , periodo_lei :: [TypeSent]
  , periodo_instituicao :: [TypeSent]
  , periodo_nan :: [TypeSent]
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
{-
func :: [String] -> [N.Document]
func (x:xs) = rights (aux (x:xs))
 where aux (x:xs) = 
    nlu <- N.readJSON x
    nlu : aux xs
-}



createDoc :: [N.Document] -> [W.Document] -> FilePath -> IO()
createDoc nlus wkss path = encodeFile path $ createTable $ sortTypeSent (createTS nlus wkss)
 where
  createTS (x:xs) (y:ys) = getSentens x y ++ createTS xs ys
  createTS _ _ = []



getSentens :: N.Document -> W.Document-> [TypeSent]
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

getText :: W.Document -> [String]
getText doc = [W.docText doc, W.name doc]



sortTypeSent :: [TypeSent] -> [[TypeSent]]
sortTypeSent ts = groupBy (\tsa tsb -> typeType tsa == typeType tsb) $ sortOn typeType ts

nullList :: [[TypeSent]] -> [[TypeSent]]
nullList = addNullList types 
 where
   types = ["data_data","data_gpe","data_instituicao","data_lei","data_nan","data_periodo","data_pessoa",
            "gpe_data","gpe_gpe","gpe_instituicao","gpe_lei","gpe_nan","gpe_periodo","gpe_pessoa",
            "instituicao_data","instituicao_gpe","instituicao_instituicao","instituicao_lei","instituicao_nan","instituicao_periodo","instituicao_pessoa",
            "lei_data","lei_gpe","lei_instituicao","lei_lei","lei_nan","lei_periodo","lei_pessoa",
            "nan_data","nan_gpe","nan_instituicao","nan_lei","nan_nan","nan_periodo","nan_pessoa",
            "periodo_data","periodo_gpe","periodo_instituicao","periodo_lei","nan_nan","periodo_periodo","periodo_pessoa",
            "pessoa_data","pessoa_gpe","pessoa_instituicao","pessoa_lei","pessoa_nan","pessoa_periodo","pessoa_pessoa"] 

addNullList :: [String] -> [[TypeSent]] -> [[TypeSent]]
addNullList (a:as) (t:ts) 
  | typeType (head t) == a = t : addNullList as ts
  | otherwise = [] : addNullList as (t:ts)
addNullList (a:as) [] = [] : addNullList as []
addNullList [] (t:ts) =  t:ts
addNullList [] [] = []

createTable :: [[TypeSent]] -> Document
createTable as = Document { table = tab, content = cont }
  where
    tab = map length as
    cont = Content {
        data_gpe           = as !! 1
      , data_instituicao   = as !! 2
      , data_lei           = as !! 3
      , data_nan           = as !! 4
      , data_periodo       = as !! 5
      , data_pessoa        = as !! 6
      , gpe_data           = as !! 7
      , gpe_instituicao    = as !! 9
      , gpe_lei            = as !! 10
      , gpe_nan            = as !! 11
      , gpe_periodo        = as !! 12
      , gpe_pessoa         = as !! 13
      , instituicao_data   = as !! 14
      , instituicao_gpe    = as !! 15
      , instituicao_lei    = as !! 17
      , instituicao_nan    = as !! 18
      , instituicao_periodo = as !! 19
      , instituicao_pessoa = as !! 20
      , lei_data           = as !! 21
      , lei_gpe            = as !! 22
      , lei_instituicao    = as !! 23
      , lei_nan            = as !! 25
      , lei_periodo         = as !! 26
      , lei_pessoa         = as !! 27
      , nan_data           = as !! 28
      , nan_gpe            = as !! 29
      , nan_instituicao    = as !! 30
      , nan_lei            = as !! 31
      , nan_periodo        = as !! 33
      , nan_pessoa         = as !! 34
      , periodo_data       = as !! 35
      , periodo_gpe        = as !! 36
      , periodo_instituicao = as !! 37
      , periodo_lei         = as !! 38
      , periodo_nan         = as !! 39
      , periodo_pessoa     = as !! 41
      , pessoa_data        = as !! 42
      , pessoa_gpe         = as !! 43
      , pessoa_instituicao = as !! 44
      , pessoa_lei         = as !! 45
      , pessoa_nan         = as !! 46
      , pessoa_periodo     = as !! 47
      }



-- main

msg = " Usage: \n\
      \  test-ner -c json-nlu json-wks  => csv file in the STDOUT \n "

usage = putStrLn msg

parse ["-h"]    = usage >> exitSuccess
--parse ("-c":ls) = createCSV ls >> exitSuccess
--parse ("-t":ns:ws:f) = createDoc ns ws (head f) >> exitSuccess
parse _         = usage >> exitFailure

-- slice :: Int -> Int -> String -> String
-- slice a b = take (b - a) . drop a

main :: IO ()
main = do
  as <- getArgs
  parse as

