module ConlluProcess where

import Conllu.IO
import Conllu.Type
import Data.Maybe
import NLUJson
import Text.Regex.TDFA
import System.Environment
import System.Exit


msg =
  " Usage: \n\
  \  test-ConlluProcess -m JSON-file CONLLU-file out-CONLLU-file => merged CONLLU file\n\
  \  test-sentence -c CONLLU-file                                => NER and POS check to stdout\n"

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
cwRange w = readRange $ fromJust $ _misc w

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

merge :: [FilePath] -> IO ()
merge [pj,pc,po] = do
  js <- readJSON pj
  sents <- readConlluFile pc
  let ents = entities js
  let ncl = map (\s -> metaUpdate s $ filter (`entINsent` s) ents) sents
  writeConlluFile po ncl

entCheck:: Entity -> [CW AW] -> IO()
entCheck e l = print 0

sentCheck :: Sent -> IO()
sentCheck s = mapM_ (`entCheck` w) ent where
  ent = strTOent $ snd $ last $ _meta s
  w = _words s

check :: [FilePath] -> IO ()
check (p:_) = do
  c <- readConlluFile p
  mapM_ sentCheck c

main :: IO ()
main = getArgs >>= parse