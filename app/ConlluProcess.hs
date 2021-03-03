module ConlluProcess where

import Conllu.IO
import Conllu.Type
import Data.Maybe
import NLU
import Text.Regex.TDFA
import System.Environment
import System.Exit


msg =
  " Usage: \n\
  \  test-conllu -m JSON-file CONLLU-file CoNLLU-file\n\
  \  test-conllu -c CONLLU-file  => NER and POS check to stdout\n"

usage = putStrLn msg

parse ["-h"]    = usage >> exitSuccess
parse ("-m":ls) = merge ls >> exitSuccess
parse ("-c":ls) = check ls >> exitSuccess
parse ls        = usage >> exitFailure


type Range = (Int,Int)

readRange :: String -> Range -- Read _misc string to range
readRange t = (read a, read b) where
  (_,_,_,[a,b]) = (=~) t "TokenRange=([0-9]+):([0-9]+)" :: (String,String,String,[String])

cwRange :: CW AW -> Range -- Take range of element
cwRange w = maybe (-1,0) readRange $ _misc w 

sentRange :: Sent -> Range -- Take tange of sentence
sentRange s = (fst $ cwRange $ head w, snd $ cwRange $ last w) where w = _words s

mentionRange :: Mention -> Range -- Rake ranges of a mention
mentionRange (Mention t (b:e)) =  (b,head e)

entRanges :: Entity -> [Range] -- Take ranges of entity
entRanges e = map mentionRange $ mentions e

isSubrange :: Range -> Range -> Bool -- Verify if range is subrange of other
isSubrange (b1,e1) (b2,e2) = b1 >= b2 && e1 <= e2

entINsent :: Entity -> Sent -> Bool -- Verify if entity belongs to sent
entINsent e s = any (`isSubrange` sentRange s) (entRanges e)

metaUpdate :: Sent -> [Entity] -> Sent -- Update Sent metadata with entities
metaUpdate s e = Sent (_meta s ++ [("entitys",entTOstr e)]) (_words s)

isMember :: (Foldable t, Eq a) => a -> t a -> Bool
isMember n = foldr (\x -> (||) (n==x)) False

cwHead :: CW AW -> ID 
cwHead n = maybe (SID 0) _head $ _rel n

merge :: [FilePath] -> IO ()
merge [pj,pc,po] = do
  js <- readJSON pj
  sents <- readConlluFile pc
  let ents = entities js
  let ncl = map (\s -> metaUpdate s $ filter (`entINsent` s) ents) sents
  writeConlluFile po ncl

entCheck:: Entity -> [CW AW] -> Bool
entCheck e l = res where
  er = head $ entRanges e
  nl = filter (\c -> isSubrange (cwRange c) er) l
  nodes = map _id nl
  roots = filter (\c -> isMember (cwHead c) nodes) nl
  res = length roots <= 1

sentCheck :: Sent -> [Entity]
sentCheck s = filter (`entCheck` w) ent where
  ent = strTOent $ snd $ last $ _meta s
  w = _words s

check :: [FilePath] -> IO ()
check (p:_) = do
  c <- readConlluFile p
  print $ map sentCheck c

main :: IO ()
main = getArgs >>= parse
