
module Sentence where

import System.Environment ( getArgs )
import System.Exit ( ExitCode(ExitSuccess), exitWith )
import Data.List ( intercalate )
import Text.ParserCombinators.ReadP ( char, many, munch, readP_to_S, string, ReadP )
import Data.Char ( isDigit, isSpace )
import System.FilePath ( takeExtension )
import Text.Regex.TDFA ( (=~) )

msg =
  " Usage: \n\
  \  test-sentence -d offset-file raw-file   => descompact offset into stdout \n\
  \  test-sentence -c sent-file  => compact sent into stdout\n "

usage = putStrLn msg
exit = exitWith ExitSuccess

parse ["-h"]    = usage >> exit
parse ("-c":ls) = compact ls >> exit
parse ("-d":ls) = descompact ls >> exit
parse ls        = usage >> exit


-- to compact a file with one sentence per line, we need to read the
-- file into a list of strings. Calculate the offsets and save the
-- offsets into a file.

offsets ls = tail $ reverse $ foldl aux [(0,-1)] ls
  where
    aux ps s = (last + 1, last + 1 + length s) : ps 
      where
        last = snd $ head ps


compact :: [FilePath] -> IO ()
compact ls = do
  content <- readFile $ head ls
  let os = offsets $ lines content
  let ss = map (\x -> show (fst x) ++ " " ++ show (snd x)) os
  putStr $ intercalate "\n" ss ++ "\n"


-- parser to offset in a string, not all parsers need to be
-- complicated. Using https://hackage.haskell.org/package/regex-tdfa
-- just because hs-conllu already depend on it.

digits :: String -> [[Int]]
digits c = map (map read . tail) ms
  where
    ms = (=~) c "([0-9]+)[ ]+([0-9]+)[ ]*\n" :: [[String]]


slice :: Int -> Int -> String -> String
slice a b = take (b - a) . drop a

getSubStrings :: [[Int]] -> String -> [String]
getSubStrings [] c = []
getSubStrings (p:ps) c = as : getSubStrings ps c
  where
    as = slice (head p) (p!!1) c

-- split raw text into sentences based on offsets
descompact :: [FilePath] -> IO ()
descompact ls = do
    content <- readFile $ head ls
    raw <- readFile $ ls!!1
    let ds = digits content
    let ss = getSubStrings ds raw
    putStr $ intercalate "\n" ss ++ "\n"

main :: IO ()
main = getArgs >>= parse
