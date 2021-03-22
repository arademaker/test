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
  { pessoa_gpe :: [TypeSent]
  , pessoa_data :: [TypeSent]
  , pessoa_nan :: [TypeSent]
  , pessoa_lei :: [TypeSent]
  , pessoa_instituicao :: [TypeSent]
  , gpe_pessoa :: [TypeSent]
  , gpe_data :: [TypeSent]
  , gpe_nan :: [TypeSent]
  , gpe_lei :: [TypeSent]
  , gpe_instituicao :: [TypeSent]
  , data_pessoa :: [TypeSent]
  , data_gpe :: [TypeSent]
  , data_nan :: [TypeSent]
  , data_lei :: [TypeSent]
  , data_instituicao :: [TypeSent]
  , nan_pessoa :: [TypeSent]
  , nan_gpe :: [TypeSent]
  , nan_data :: [TypeSent]
  , nan_lei :: [TypeSent]
  , nan_instituicao :: [TypeSent]
  , lei_pessoa :: [TypeSent]
  , lei_gpe :: [TypeSent]
  , lei_data :: [TypeSent]
  , lei_nan :: [TypeSent]
  , lei_instituicao :: [TypeSent]
  , instituicao_pessoa :: [TypeSent]
  , instituicao_gpe :: [TypeSent]
  , instituicao_data :: [TypeSent]
  , instituicao_nan :: [TypeSent]
  , instituicao_lei :: [TypeSent]
  } deriving (Show, Generic)

instance FromJSON Content
instance ToJSON Content where
  toEncoding = genericToEncoding defaultOptions


data TypeSent = TypeSent
    { word :: String
    , sentence :: String
    , doc :: String
    , type_type  :: String
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
createJSON = encodeFile

-- create json

createDoc :: [FilePath] -> Document
createDoc paths = 
types = createTypeSent paths
Document {
  table = "..."
  content = aux 
    where
      aux 
}

createTypeSent :: [FilePath] -> [TypeSent]
createTypeSent (x:y:xs) = getSentens x y : createTypeSent xs
createTypeSent _ = []

getSentens :: FilePath -> FilePath -> [TypeSent]
getSentens 
nlu <- readJSON pathNLU
wks <- readJSON pathWKS
(Right docWKS) <- readJSON pathWKS
text = W.docText docWKS
fileName = W.name docWKS
return func fileName text validation nlu wks 
where 
  func String -> String -> [Annotation] -> [TypeSent]
  func name text anns = map (aux name text) anns
    where
      aux :: String -> String -> Annotation -> TypeSent
      aux name text ann =
        TypeSent {
          doc = name
        , word = [anBegin:anEnd] text
        , sentence = [anBegin - 10:anEnd + 10] text
        , type_type = anType ann
        }



    p_g = []
  , p_d = []
  , p_n = []
  , p_l = []
  , p_i = []
  , g_p = []
  , g_d = []
  , g_n = []
  , g_l = []
  , g_i = []
  , d_p = []
  , d_g = []
  , d_n = []
  , d_l = []
  , d_i = []
  , n_p = []
  , n_g = []
  , n_d = []
  , n_l = []
  , n_i = []
  , l_p = []
  , l_g = []
  , l_d = []
  , l_n = []
  , l_i = []
  , i_p = []
  , i_g = []
  , i_d = []
  , i_n = []
  , i_l = []

map aux anns 
  where aux 
    | anType i == "pessoa_gpe" = i ++ p_g
    | anType i == "pessoa_data" = i ++ p_d 
    | anType i == "pessoa_nan" = i ++ p_n 
    | anType i == "pessoa_lei" = i ++ p_l 
    | anType i == "pessoa_instituicao" = i ++ p_i 
    | anType i == "gpe_pessoa" = i ++ g_p 
    | anType i == "gpe_data" = i ++ g_d 
    | anType i == "gpe_nan" = i ++ g_n 
    | anType i == "gpe_lei" = i ++ g_l 
    | anType i == "gpe_instituicao" = i ++ g_i 
    | anType i == "data_pessoa" = i ++ d_p 
    | anType i == "data_gpe" = i ++ d_g 
    | anType i == "data_nan" = i ++ d_n 
    | anType i == "data_lei" = i ++ d_l 
    | anType i == "data_instituicao" = i ++ d_i 
    | anType i == "nan_pessoa" = i ++ n_p 
    | anType i == "nan_gpe" = i ++ n_g 
    | anType i == "nan_data" = i ++ n_d 
    | anType i == "nan_lei" = i ++ n_l 
    | anType i == "nan_instituicao" = i ++ n_i 
    | anType i == "lei_pessoa" = i ++ l_p 
    | anType i == "lei_gpe" = i ++ l_g 
    | anType i == "lei_data" = i ++ l_d 
    | anType i == "lei_nan" = i ++ l_n 
    | anType i == "lei_instituicao" = i ++ l_i 
    | anType i == "instituicao_pessoa" = i ++ i_p 
    | anType i == "instituicao_gpe" = i ++ i_g 
    | anType i == "instituicao_data" = i ++ i_d 
    | anType i == "instituicao_nan" = i ++ i_n 
    | anType i == "instituicao_lei" = i ++ i_l 

table = [[p_g,p_d,p_n,p_l,p_i],[g_p,g_d,g_n,g_l,g_i],[d_p,d_g,d_n,d_l,d_i],
         [n_p,n_g,n_d,n_l,n_i],[l_p,l_g,l_d,l_n,l_i],[i_p,i_g,i_d,i_n,i_l]]


(table !! aux head type_type) !! aux last type_type = typeSent




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

