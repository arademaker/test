{-# LANGUAGE DeriveGeneric, BlockArguments #-}


module NER where

import qualified WKS as W
import qualified NLU as N
import Data.Aeson
import System.Exit
import System.Environment
import Data.Either
import Data.Maybe
import Data.List ( groupBy, intercalate, sortOn, nub)
import Data.Ord (comparing)
import GHC.Generics
import Data.Char ( toLower )
import Data.Text ( unpack, pack)
import Control.Applicative

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

-- se continuar producao json
-- https://hackage.haskell.org/package/aeson-1.5.6.0/docs/Data-Aeson.html

-- possivel ideia de geracao HTML
-- https://mmhaskell.com/blog/2020/3/9/blaze-lightweight-html-generation

data Document =
  Document
    { header  :: [String]
    , table :: [[Int]]
    , content  :: [[AnnC]]
    } deriving (Show, Generic)
    
instance FromJSON Document
instance ToJSON Document where
  toEncoding = genericToEncoding defaultOptions

data AnnC =
  AnnC
    { mention :: String
    , context :: String
    , range    :: (Int, Int)
    , doc     :: String
    , comp :: (String, String)
    }
  deriving (Show, Generic)

instance FromJSON AnnC
instance ToJSON AnnC where
  toEncoding = genericToEncoding defaultOptions


--  m a -> m b -> (a -> b -> c) -> m c
test = do
  a <- W.readJSON "/Users/ar/work/cpdoc/dhbb-nlp/ner/wks/gt/72ba3590-8cd9-11eb-9b31-bf56d6e1e183-1.json"
  b <- N.readJSON "/Users/ar/work/cpdoc/dhbb-nlp/ner/220.json"
  return (liftA2 getSentens b a)
      

createDoc :: [N.Document] -> [W.Document] -> FilePath -> IO()
createDoc nlus wkss path = encodeFile path $ meDeUmNome $ sortTypeSent (createTS nlus wkss)
 where
  createTS (x:xs) (y:ys) = getSentens x y ++ createTS xs ys
  createTS _ _ = []



getSentens :: N.Document -> W.Document-> [AnnC]
getSentens nlu wks = map (createType fileName text) $ fromRight [] (validation nlu wks)
 where [text,fileName] = getText wks
   
createType :: String -> String -> Annotation -> AnnC
createType name text ann =
  AnnC {
            doc = name
          , mention = subStr (anBegin ann) (anEnd ann) text
          , context = subStr (anBegin ann - 15) (anEnd ann + 15) text
          , comp = (head (anType ann), last (anType ann))
          , range = (anBegin ann, anEnd ann)
          } 

subStr :: Int -> Int -> String -> String
subStr a b text = take (b - a) (drop a text)

getText :: W.Document -> [String]
getText doc = [W.docText doc, W.name doc]

meDeUmNome :: [[AnnC]] -> Document
meDeUmNome annc = createTable tipos (addNullList tipos annc)
 where
   tipos = getType annc 


sortTypeSent :: [AnnC] -> [[AnnC]]
sortTypeSent m = groupBy (\ma mb -> comp ma == comp mb) $ sortOn comp m

getType :: [[AnnC]]->[String]
getType m = nub $ map aux m
 where 
   aux (a:as) = fst (comp a)

addNullList :: [String] -> [[AnnC]] -> [[AnnC]]
addNullList (a:as) (t:ts) 
  | fst (comp (head t)) == a = t : addNullList as ts
  | otherwise = [] : addNullList as (t:ts)
addNullList (a:as) [] = [] : addNullList as [] <- 
addNullList [] (t:ts) =  t:ts
addNullList [] [] = []

tableInt :: [[AnnC]] -> Int -> [[Int]] -> [[Int]]
tableInt [] n l
  | length l == n && length (last l) == n = l
  | length (last l) == n                  = tableInt [] n $ l ++ [[0]]
  | otherwise                             = tableInt [] n $ (init l) ++ [last l ++ [0]]
tableInt (x:xs) n l
  | length (last l) == n                  = tableInt xs n $ l ++ [[length x]]
  | otherwise                             = tableInt xs n $ (init l) ++ [last l ++ [length x]]

removeDiag :: [[AnnC]] -> Int -> [[AnnC]] -> [[AnnC]]
removeDiag [] _ l = l
removeDiag (x:xs) n l
 | length (last l) == n && (fst . comp (head x) == snd . comp (head x)) = removeDiag xs n $ l ++ [[]]
 | length (last l) == n = removeDiag xs n $ l ++ [x]
--  | (fst . comp (head x)) == (snd . comp (head x)) = removeDiag xs n $ (init l) ++ [last l ++ []]
--  | otherwise = removeDiag xs n $ (init l) ++ [last l ++ x]
 | otherwise = 

createTable :: [String] -> [[AnnC]] -> Document
createTable types cont = Document {header = types, table = (tableInt cont (length types) [[]]), content = cont}

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
