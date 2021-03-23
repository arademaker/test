{-# LANGUAGE BlockArguments #-}
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
import Data.Char


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
  { table :: [[Int]] 
  , content :: Content
  } deriving (Show, Generic)

instance FromJSON Document
instance ToJSON Document where
  toEncoding = genericToEncoding defaultOptions

createJSON :: FilePath -> Document -> IO ()
createJSON = encodeFile

-- create json
createDoc :: [FilePath] -> FilePath -> IO()
createDoc as path = createJSON path (createTable . createTS as)


createTS :: [FilePath] -> [TypeSent]
createTS (x:y:xs) = getSentens x y : createTS xs
createTS _ = [] 
  

loweredString :: String -> String
lowerString str = [ toLower loweredString | loweredString <- str]

getSentens :: FilePath -> FilePath -> [TypeSent]
getSentens pathNLU pathWKS = do
  nlu <- N.readJSON pathNLU
  wks <- W.readJSON pathWKS
  (Right docWKS) <- W.readJSON pathWKS
  text <- W.docText docWKS
  fileName <- W.name docWKS
  return (func fileName text validation nlu wks) 
    where 
      -- func :: String -> String -> [Annotation] -> [TypeSent] 
      func name text anns = concatMap (aux name text) anns 
        where 
          -- aux :: String -> String -> Annotation -> TypeSent 
          aux name text ann = 
            TypeSent { 
                doc = name 
              , word = (take (anEnd - anBegin)) . (drop anBegin) text
              , sentence = (take (10 + anEnd - anBegin)) . (drop (anBegin - 10 )) text
              , typeType = intercalate "_" loweredString $ anType ann                  
              }


sortTypeSent :: [TypeSent] -> [[TypeSent]]
sortTypeSent as
  let   p_p = [] 
        p_g = [] 
        p_d = []
        p_n = []
        p_l = []
        p_i = []
        g_p = []
        g_g = []
        g_d = []
        g_n = []
        g_l = []
        g_i = []
        d_p = []
        d_g = []
        d_d = []
        d_n = []
        d_l = []
        d_i = []
        l_p = []
        l_g = []
        l_d = []
        l_n = []
        l_l = []
        l_i = []
        i_p = []
        i_g = []
        i_d = []
        i_n = []
        i_l = []
        i_i = []
        n_p = []
        n_g = []
        n_d = []
        n_l = []
        n_i = []
        n_n = []
  in 
    map aux as 
      where 
        aux a 
          | typeType a == "pessoa_pessoa" = a ++ p_p
          | typeType a == "pessoa_gpe" = a ++ p_g
          | typeType a == "pessoa_data" = a ++ p_d 
          | typeType a == "pessoa_lei" = a ++ p_l 
          | typeType a == "pessoa_instituicao" = a ++ p_i 
          | typeType a == "pessoa_nan" = a ++ p_n 
          | typeType a == "gpe_pessoa" = a ++ g_p 
          | typeType a == "gpe_gpe" = a ++ g_g
          | typeType a == "gpe_data" = a ++ g_d 
          | typeType a == "gpe_lei" = a ++ g_l 
          | typeType a == "gpe_instituicao" = a ++ g_i 
          | typeType a == "gpe_nan" = a ++ g_n 
          | typeType a == "data_pessoa" = a ++ d_p 
          | typeType a == "data_gpe" = a ++ d_g 
          | typeType a == "data_data" = a ++ d_d
          | typeType a == "data_lei" = a ++ d_l 
          | typeType a == "data_instituicao" = a ++ d_i 
          | typeType a == "data_nan" = a ++ d_n 
          | typeType a == "lei_pessoa" = a ++ l_p 
          | typeType a == "lei_gpe" = a ++ l_g 
          | typeType a == "lei_data" = a ++ l_d 
          | typeType a == "lei_lei" = a ++ l_l
          | typeType a == "lei_instituicao" = a ++ l_i 
          | typeType a == "lei_nan" = a ++ l_n 
          | typeType a == "instituicao_pessoa" = a ++ i_p 
          | typeType a == "instituicao_gpe" = a ++ i_g 
          | typeType a == "instituicao_data" = a ++ i_d 
          | typeType a == "instituicao_lei" = a ++ i_l 
          | typeType a == "instituicao_instituicao" = a ++ i_i
          | typeType a == "instituicao_nan" = a ++ i_n 
          | typeType a == "nan_pessoa" = a ++ n_p 
          | typeType a == "nan_gpe" = a ++ n_g 
          | typeType a == "nan_data" = a ++ n_d 
          | typeType a == "nan_lei" = a ++ n_l 
          | typeType a == "nan_instituicao" = a ++ n_i 
          | otherwise = return
    
    return [p_p, p_g, p_d, p_l, p_i, p_n, 
            g_p, g_g, g_d, g_l, g_i, g_n,
            d_p, d_g, d_d, d_l, d_i, d_n,
            l_p, l_g, l_d, l_l, l_i, l_n,
            i_p, i_g, i_d, i_l, i_i, i_n,
            n_p, n_g, n_d, n_l, n_i, n_n]

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

