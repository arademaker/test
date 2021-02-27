module NLUJson where

import Conllu.Type
import Data.Maybe
import DHBB_JSON
import Text.Regex.TDFA

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
metaUpdate s e = Sent (_meta s ++ [("entitys",strEntity e)]) (_words s)