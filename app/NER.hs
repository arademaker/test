{-# LANGUAGE DeriveGeneric, BlockArguments #-}


module NER where

import qualified WKS as W
import qualified NLU as N
import qualified Data.ByteString.Lazy as B (readFile)
import Data.Aeson
import System.FilePath.Posix
import System.Exit
import System.Environment
import Data.Either
import Data.Maybe
import Data.List 
import Data.Ord (comparing)
import GHC.Generics
import Data.Char ( toLower )
import Data.Text ( unpack, pack )
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

-- json for table structure 
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
    , rangeInSent :: [Int]
    }
  deriving (Show, Generic)

instance FromJSON AnnC
instance ToJSON AnnC where
  toEncoding = genericToEncoding defaultOptions


-- json reader with names of NLU and WKS jsons
data Documents = 
  Documents { docId :: FilePath
            , name :: FilePath
            , createdDate :: Int
            , text :: String
            , status :: String
            , modifiedDate :: Double
            } deriving (Show, Generic)

customDocuments :: Options
customDocuments = defaultOptions {fieldLabelModifier = aux}
  where
    aux x | x == "docId" = "id"
          | otherwise = x

instance FromJSON Documents
  where 
    parseJSON = genericParseJSON customDocuments

instance ToJSON Documents 
  where
    toJSON = genericToJSON customDocuments
    toEncoding = genericToEncoding customDocuments


readJSON :: FilePath -> IO (Either String [Documents])
readJSON path = fmap eitherDecode (B.readFile path) :: IO (Either String [Documents])


-- Receive filepaths of NLU and return NLU.Document
pathToDocNLU :: [FilePath] -> IO [N.Document]
pathToDocNLU = mapM $ fmap (\(Right x) -> x) . N.readJSON

-- Receive filepaths of WKS and return WKS.Document
pathToDocWKS :: [FilePath] -> IO [W.Document]
pathToDocWKS = mapM $ fmap (\(Right x) -> x) . W.readJSON

-- Receive json with names of the NLU and WKS jsons, location of the NLU jsons, 
-- output json writing site and return json with the compression matrix.
createJson :: FilePath -> FilePath -> FilePath -> IO()
createJson jsonFile inDir outDir = do
  jsonDoc <- readJSON jsonFile
  nlu <- pathToDocNLU $ map (\x -> replaceDirectory x inDir) 
                            (map (\x -> replaceExtension x "json") 
                                 (map name 
                                      (head (rights [jsonDoc]))))
  wks <- pathToDocWKS $ map (\x -> replaceDirectory x (combine (takeDirectory jsonFile) "gt")) 
                            (map (\x -> replaceExtension x ".json") 
                                 (map docId
                                      (head (rights [jsonDoc]))))
  createDoc nlu wks outDir

-- Receive a list of NLU document, a list of WKS document, a file and return a json file
createDoc :: [N.Document] -> [W.Document] -> FilePath -> IO()
createDoc nlus wkss path = encodeFile path $ constructorDoc $ sortTypeSent (createTS nlus wkss)
 where
  createTS (x:xs) (y:ys) = getSentens x y ++ createTS xs ys
  createTS _ _ = []


-- Apply validation in 2 Documents and return a AnnC
getSentens :: N.Document -> W.Document-> [AnnC]
getSentens nlu wks = map (createType (head fileName) (head text) sentences) $ fromRight [] (validation nlu wks)
 where [text, fileName, sentences] = getText wks

-- Receive sentences and range of marking, return sentence that is 
-- the mark and the range of the mark within the sentence
returnSentence :: [String] -> Int -> Int -> Int -> (String, (Int, Int))
returnSentence [] _ _ _ = ("Error - sentences list null", (0, 0))   -- Not the best place for a mistake
returnSentence sents begin end cont
  | end > cont + length (head sents) = returnSentence (tail sents) begin end (cont + 1 + (length (head sents)))
  | begin >= cont = ((head sents), (begin - cont, end - cont))
  | otherwise = ("Error - Marking in 2 or more sentences", (0, 0))   -- Not the best place for a mistake

-- Create AnnC, type for html
createType :: String -> String -> [String] -> Annotation -> AnnC
createType name text sentences ann =
  let sentAndRang = returnSentence sentences (anBegin ann) (anEnd ann) (0)
  in
    AnnC {
              doc = name
            , mention = subStr (anBegin ann) (anEnd ann) text
            , context = (fst sentAndRang)
            , comp = (head (anType ann), last (anType ann))
            , range = (anBegin ann, anEnd ann)
            , rangeInSent = [fst (snd sentAndRang), snd (snd sentAndRang)]
            } 

subStr :: Int -> Int -> String -> String
subStr a b text = take (b - a) (drop a text)

-- Get text, file name and sentences 
getText :: W.Document -> [[String]]
getText docW = [[W.docText docW], [W.name docW], map W.senText (W.sentences docW)]

-- apply the builders
constructorDoc :: [[AnnC]] -> Document
constructorDoc annc = createTable tipos (addNullList (combi tipos) annc)
 where
   tipos = getType annc 

-- Groups the same cell phone markings 
sortTypeSent :: [AnnC] -> [[AnnC]]
sortTypeSent m = groupBy (\ma mb -> comp ma == comp mb) $ sortOn comp m

-- takes the types of annotations 
getType :: [[AnnC]] -> [String]
getType m = nub $ map aux m
 where 
   aux (a:as) = fst (comp a)

-- All possible cells (making a combination) 
combi :: [String] -> [[String]]
combi s = sort [ [x,y] | x<-s, y<-s ]

-- Adds empty cells 
addNullList :: [[String]] -> [[AnnC]] -> [[AnnC]]
addNullList (s:xs) (a:xa)
 | fst (comp (head a)) == s !! 0 && snd (comp (head a)) == s !! 1 = a : addNullList xs xa
 | otherwise = [] : addNullList xs (a:xa)
addNullList (s:xs) [] = [] : addNullList xs []
addNullList s [] = []
addNullList [] _ = []

-- Create table integers
tableInt :: [[AnnC]] -> Int -> [[Int]] -> [[Int]]
tableInt (a:as) n l
 | length (last l) == n = tableInt as n (l ++ [[length a]])
 | otherwise = tableInt as n $ (init l) ++ [last l ++ [length a]]
tableInt [] _ l = l

-- Removes diagonal markings 
removeDiag :: [[AnnC]] -> [[AnnC]] -> [[AnnC]]
removeDiag (a:as) l
 | null a  = removeDiag as (l ++ [[]])
 | fst (comp (head a)) == snd (comp (head a)) = removeDiag as (l ++ [[]])
 | otherwise = removeDiag as (l ++ [a])
removeDiag [] l = l

-- Groups the data to create the document 
createTable :: [String] -> [[AnnC]] -> Document
createTable types cont = Document { header = types
                                  , table = (tableInt cont (length types) [[]])
                                  , content = (removeDiag cont [])}

-- main

msg = " Usage:\n\n\
\  test-ner -t JSON_FILE NLU_PATH OUT_JSON\n\
\     JSON_FILE: json with names of the NLU and WKS jsons\n\
\     NLU_PATH: location of the NLU jsons\n\
\     OUT_JSON: output json writing site"

usage = putStrLn msg

parse ["-h"]    = usage >> exitSuccess
parse ("-t":jsonFile:inDir:outDir) = createJson jsonFile inDir (head outDir) >> exitSuccess
parse _         = usage >> exitFailure

main :: IO ()
main = do
  as <- getArgs
  parse as
