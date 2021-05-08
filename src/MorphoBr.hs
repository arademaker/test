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
 
readD path = do
  lfiles <- listDirectory path
  dicts  <- mapM (readF1 . combine path) lfiles
  return (foldr M.union M.empty dicts)
