module ConlluProcess where

import Text.Regex.TDFA ( (=~) )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import Conllu.IO ( readConlluFile, writeConlluFile )
import Conllu.Type
import NLU


type Range = (Int,Int)

-- corrigir !!!
readRange :: String -> Maybe Range 
readRange t = Just (read a, read b)
  where
    (_,_,_,[a,b]) = (=~) t "TokenRange=([0-9]+):([0-9]+)" :: (String,String,String,[String])

-- Take range of element
cwRange :: CW AW -> Range 
cwRange w = maybe (-1,0) readRange $ _misc w 

-- Take tange of sentence
sentRange :: Sent -> Range 
sentRange s = (fst $ cwRange $ head w, snd $ cwRange $ last w)
  where
    w = _words s

-- Rake ranges of a mention
mentionRange :: Mention -> Range 
mentionRange (Mention _ [b,e] _) = (b,e)

entRanges :: Entity -> [Range] -- Take ranges of entity
entRanges e = map mentionRange $ mentions e

isSubrange :: Range -> Range -> Bool -- Verify if range is subrange of other
isSubrange (b1,e1) (b2,e2) = b1 >= b2 && e1 <= e2

-- Verify if entity belongs to sent
entINsent :: Entity -> Sent -> Bool 
entINsent e s = any (`isSubrange` sentRange s) (entRanges e)

metaUpdate :: Sent -> [Entity] -> Sent -- Update Sent metadata with entities
metaUpdate s e = Sent (_meta s ++ [("entitys",entTOstr e)]) (_words s)

isMember :: (Foldable t, Eq a) => a -> t a -> Bool
isMember n = foldr (\x -> (||) (n==x)) False

cwHead :: CW AW -> ID 
cwHead n = maybe (SID 0) _head $ _rel n

addJson (Left _) _ _ = putStrLn "JSON inválido"
addJson (Right js) sents outpath = writeConlluFile outpath ncl
  where
    ncl = map (\s -> metaUpdate s $ filter (`entINsent` s) (entities js)) sents

merge :: [FilePath] -> IO ()
merge [jspath, clpath, outpath] = do
  js <- readJSON jspath
  sents <- readConlluFile clpath
  addJson js sents outpath

-- como fazer...

entCheck:: Entity -> [CW AW] -> Bool
entCheck e l = res where
  er = head $ entRanges e
  nl = filter (\c -> isSubrange (cwRange c) er) l
  nodes = map _id nl
  roots = filter (\c -> not $ isMember (cwHead c) nodes) nl
  res = length roots > 1

sentCheck :: Sent -> [Entity]
sentCheck s = filter (`entCheck` w) ent where
  ent = strTOent $ snd $ last $ _meta s
  w = _words s

check :: [FilePath] -> IO ()
check (p:_) = do
  c <- readConlluFile p
  print $ foldl (\r s -> r ++ sentCheck s) [] c


-- main interface

msg =
  " Usage: \n\
  \  test-conllu -m JSON-file CONLLU-file CoNLLU-file\n\
  \  test-conllu -c CONLLU-file  => NER and POS check to stdout\n"

help = putStrLn msg

parse ["-h"]    = help >> exitSuccess
parse ("-m":ls) = merge ls >> exitSuccess
parse ("-c":ls) = check ls >> exitSuccess
parse ls        = help >> exitFailure

    
main :: IO ()
main = getArgs >>= parse
