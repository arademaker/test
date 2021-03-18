{-# LANGUAGE DeriveGeneric  #-}


module NER where

import qualified WKS as W
import qualified NLU as N
import Data.Aeson
import System.Exit
import System.Environment
import Data.Either
import Data.Maybe
import Data.List
import Data.Ord (comparing)
import GHC.Generics


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


-- Create Json

data Content = Content 
  { pessoa_pessoa :: [TypeSent]
  , pessoa_gpe :: [TypeSent]
  , pessoa_data :: [TypeSent]
  , pessoa_nan :: [TypeSent]
  , pessoa_lei :: [TypeSent]
  , pessoa_instituicao :: [TypeSent]
  , gpe_pessoa :: [TypeSent]
  , gpe_gpe :: [TypeSent]
  , gpe_data :: [TypeSent]
  , gpe_nan :: [TypeSent]
  , gpe_lei :: [TypeSent]
  , gpe_instituicao :: [TypeSent]
  , data_pessoa :: [TypeSent]
  , data_gpe :: [TypeSent]
  , data_data :: [TypeSent]
  , data_nan :: [TypeSent]
  , data_lei :: [TypeSent]
  , data_instituicao :: [TypeSent]
  , nan_pessoa :: [TypeSent]
  , nan_gpe :: [TypeSent]
  , nan_data :: [TypeSent]
  , nan_nan :: [TypeSent]
  , nan_lei :: [TypeSent]
  , nan_instituicao :: [TypeSent]
  , lei_pessoa :: [TypeSent]
  , lei_gpe :: [TypeSent]
  , lei_data :: [TypeSent]
  , lei_nan :: [TypeSent]
  , lei_lei :: [TypeSent]
  , lei_instituicao :: [TypeSent]
  , instituicao_pessoa :: [TypeSent]
  , instituicao_gpe :: [TypeSent]
  , instituicao_data :: [TypeSent]
  , instituicao_nan :: [TypeSent]
  , instituicao_lei :: [TypeSent]
  , instituicao_instituicao :: [TypeSent]
  } deriving (Show, Generic)

instance FromJSON Content
instance ToJSON Content where
  toEncoding = genericToEncoding defaultOptions


data TypeSent = TypeSent
    { word :: String
    , sentence :: String
    , doc :: String
    } deriving (Show, Generic)

instance FromJSON TypeSent
instance ToJSON TypeSent where
  toEncoding = genericToEncoding defaultOptions


data Document = Document
  { table :: [[Int]] 
  , content :: Content
  } deriving (Show, Generic)

instance FromJSON Document
instance ToJSON Document where
  toEncoding = genericToEncoding defaultOptions

createJSON :: FilePath -> Document -> IO ()
createJSON  = encodeFile

-- main

msg = " Usage: \n\
      \  test-ner -c json-nlu json-wks  => csv file in the STDOUT \n "
   
usage = putStrLn msg

parse ["-h"]    = usage >> exitSuccess
parse ("-c":ls) = createCSV ls >> exitSuccess
parse _         = usage >> exitFailure

slice :: Int -> Int -> String -> String
slice a b = take (b - a) . drop a

main :: IO ()
main = do
  as <- getArgs
  parse as

