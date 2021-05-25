{-# LANGUAGE OverloadedStrings #-}

module MorphoBr where

import Data.Either
import System.IO
import qualified Data.Map as M
import System.Directory
import System.FilePath.Posix
import qualified Data.Text as T
import qualified Data.Text.IO as TO
import Data.List (groupBy, intercalate, sort)
import Data.Maybe ( fromJust, isNothing )

member :: T.Text  -> [T.Text] -> Bool
member x [] = False
member x (y:ys) | x==y = True
                | otherwise = member x ys

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


simplify :: [T.Text] -> [T.Text]
simplify ms =
  map aux $ groupBy (\a b -> (head a) == (head b)) (map (T.splitOn (T.pack "+")) (sort ms))
   where 
     aux ls =  T.pack $ conc $ foldl1 inter ls

readF1 :: FilePath -> IO (M.Map T.Text [T.Text])
readF1 fn = do
  content <- TO.readFile fn
  return $ M.fromListWith (++) $ lines2pairs (T.lines content)

getDict :: [FilePath] -> [FilePath]
getDict (x:xs)
 | takeExtension x == ".dict" = x:getDict xs
 | otherwise = []
getDict [] = []

getTXT :: [FilePath] -> [FilePath]
getTXT (x:xs)
 | takeExtension x == ".txt" = x:getTXT xs
 | otherwise = []
getTXT [] = []

check :: M.Map T.Text [T.Text] -> [T.Text] -> T.Text
check m xs
 | isNothing $ M.lookup (head xs) m  = T.pack ""
 | otherwise = T.pack "ok"

createMap :: FilePath -> [FilePath] -> IO (M.Map T.Text [T.Text])
createMap dir paths = do
  dicts <- mapM (readF1 . combine dir) paths 
  return (M.map simplify $ foldr M.union M.empty dicts)


readD :: FilePath -> IO [[()]]
readD path = do
  lfiles <- listDirectory path
  m <- createMap path (getDict lfiles)
  mapM (aux m path) (getTXT lfiles)
   where
     aux m path f = do
       content <- TO.readFile $ combine path f
       print(M.size m)
       mapM (print . check m . (T.splitOn "\t")) (T.lines content)
       

