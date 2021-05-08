{-# LANGUAGE OverloadedStrings #-}

module MorphoBr where

import Data.Either
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import System.IO
import qualified Data.Map as M
import qualified Data.List.Split as S
import System.Directory
import System.FilePath.Posix
import qualified Data.Text as T
import qualified Data.Text.IO as TO

-- read MorphoBr file
readF fn = do
  hdl <- openFile fn ReadMode
  hasLine <- hIsEOF hdl
  firstLine <-
    if not hasLine
      then hGetLine hdl
      else return "empty"
  hClose hdl
  putStrLn "done!"


lines2pairs :: [T.Text] -> [(T.Text,[T.Text])]
lines2pairs =
  Prelude.map (\s -> let p = T.breakOn "\t:" s in (fst p, [snd p]))


readF1 :: FilePath -> IO (M.Map T.Text [T.Text])
readF1 fn = do
  content <- TO.readFile fn
  return $ M.fromListWith (++) $ lines2pairs (T.lines content)
 
readD path = do
  lfiles <- listDirectory path
  dicts  <- mapM (readF1 . combine path) lfiles
  return (Prelude.foldr M.union (M.empty :: M.Map T.Text [T.Text]) dicts)
