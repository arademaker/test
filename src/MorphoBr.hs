
module MorphoBr where

import Data.Either
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import System.IO
import Data.Map as M
import Data.List.Split as S
import System.Directory
import System.FilePath.Posix

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

lines2pairs :: [String] -> [(String,[String])]
lines2pairs =
  Prelude.map
    (\l ->
       let lst = S.splitOn "\t" l
        in (head lst, [lst !! 1]))


readF1 :: FilePath -> IO (Map String [String])
readF1 fn = do
  content <- readFile fn
  return $ M.fromListWith (++) $ lines2pairs (lines content)
 
readD path = do
  lfiles <- listDirectory path
  dicts  <- mapM (readF1 . combine path) lfiles
  return (Prelude.foldr M.union (M.empty :: Map String [String]) dicts)
