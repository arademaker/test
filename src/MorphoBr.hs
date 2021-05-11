{-# LANGUAGE OverloadedStrings #-}

module MorphoBr where

import Data.Either
import System.IO
import qualified Data.Map as M
import System.Directory
import System.FilePath.Posix
import qualified Data.Text as T
import qualified Data.Text.IO as TO

lines2pairs :: [T.Text] -> [(T.Text,[T.Text])]
lines2pairs =
  map (\s -> let p = T.breakOn "\t" s in (fst p, [snd p]))


readF1 :: FilePath -> IO (M.Map T.Text [T.Text])
readF1 fn = do
  content <- TO.readFile fn
  return $ M.fromListWith (++) $ lines2pairs (T.lines content)
 
readD :: FilePath -> IO (M.Map T.Text [T.Text])
readD path = do
  lfiles <- listDirectory path
  dicts  <- mapM (readF1 . combine path) lfiles
  return (foldr M.union M.empty dicts)

toStr :: Morpho -> String
toStr a = (lemma a)++"+"++(cat a)++"+"++(verb a)++"+"++(person a)++"+"++(grau a)++"+"++(gender a)++"+"++(number a)

aux :: T.Text -> [Morpho]
aux l = do
  let (x:xs) = map  T.unpack (T.splitOn "+" l)
  getMorpho (xs) (Morpho {lemma=x,cat="",verb="",person="",gender="",number="",grau=""})
 

unificate :: [T.Text] -> [T.Text]
unificate ls = map T.pack (foldl func2 [] (map aux ls))
 where 
   func2 a b 
    | (lemma a == lemma b) && 
       (cat a == cat b) && 
       (verb a == verb b) && 
       (person a == person b) && 
       (gender a == gender b) && 
       (grau a == grau b) = 
         return $ toStr a{number = ""}
    | (lemma a == lemma b) && 
       (cat a == cat b) && 
       (verb a == verb b) && 
       (person a == person b) && 
       (number a == number b) &&
       (grau a == grau b) = 
         return $ toStr a{gender = ""} 
    | otherwise = return $ toStr a

func :: [FilePath] -> IO [M.Map T.Text [T.Text]]
func path = do 
  m <- mapM readD path
  return (map (M.map unificate) m)


getMorpho :: [String] -> Morpho -> Morpho
getMorpho (x:xs) m
 | x == "V" = getMorpho xs m {cat = x} 
 | x == "A" = getMorpho xs m {cat = x}
 | x == "ADV" = getMorpho xs m {cat = x}
 | x == "DIM" = getMorpho xs m {grau = x} 
 | x == "M" = getMorpho xs m {gender = x}
 | x == "F" = getMorpho xs m {gender = x}
 | x == "SG" = getMorpho xs m {number = x}
 | x == "PL" = getMorpho xs m {number = x}
 | x == "FUT" = getMorpho xs m {verb = x}
 | x == "PRF" = getMorpho xs m {verb = x} 
 | x == "PRS" = getMorpho xs m {verb = x} 
 | x == "IMPF" = getMorpho xs m {verb = x} 
 | x == "PQP" = getMorpho xs m {verb = x}
 | x == "SBJF" = getMorpho xs m {verb = x} 
 | x == "SBJP" = getMorpho xs m {verb = x} 
 | x == "SBJR" = getMorpho xs m {verb = x} 
 | x == "3" = getMorpho xs m {person = x} 
 | x == "2" = getMorpho xs m {person = x} 
 | x == "1" = getMorpho xs m {person = x} 
 | otherwise = getMorpho xs m
getMorpho [] m = m

data Morpho =
  Morpho
  {lemma :: String
  ,cat :: String
  ,gender :: String
  ,number :: String
  ,verb :: String
  ,person :: String
  ,mood :: String
  ,tense :: String
  ,verbForm :: String
  ,voice :: String
  ,grau :: String
  } deriving (Show)


