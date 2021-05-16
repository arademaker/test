{-# LANGUAGE OverloadedStrings #-}

module MorphoBr where

import Data.Either
import System.IO
import qualified Data.Map as M
import System.Directory
import System.FilePath.Posix
import qualified Data.Text as T
import qualified Data.Text.IO as TO
import Data.List (groupBy, intercalate)



lines2pairs :: [T.Text] -> [(T.Text, [T.Text])]
lines2pairs =
  map (\s -> let p = T.splitOn "\t" s in (head p, tail p))

inter :: [T.Text] -> [T.Text] -> [T.Text] 
inter (x:xs) (y:ys)
 | x == y = x : inter xs ys
 | otherwise = inter xs ys
inter [] (y:ys) = []
inter (x:xs) [] = []
inter [] [] = []

conc :: [T.Text] -> String
conc xs = intercalate "+" (map T.unpack xs) 

aux :: [[T.Text]]-> T.Text 
aux ls =  T.pack $ conc $ foldl1 inter ls

simplify :: [T.Text] -> [T.Text]
simplify ms = 
  map aux $ groupBy (\a b -> (head a) == (head b)) (map (T.splitOn (T.pack "+")) ms)

readF1 :: FilePath -> IO (M.Map T.Text [T.Text])
readF1 fn = do
  content <- TO.readFile fn
  return $ M.map simplify $ M.fromListWith (++) $ lines2pairs (T.lines content)

readD path = do
  lfiles <- listDirectory path
  dicts  <- mapM (readF1 . combine path) lfiles
  return (foldr M.union M.empty dicts)