module JsonConlluTools where

import qualified Data.ByteString.Lazy.Char8 as C
import Text.Regex.TDFA
import Data.Maybe
import Data.List
import Data.Aeson
import Data.Either
import Conllu.Type
import NLU

type Range = (Int,Int)

isSubrange :: Range -> Range -> Bool
isSubrange (b1,e1) (b2,e2) = b1 >= b2 && e1 <= e2

-- isMember :: (Foldable t, Eq a) => a -> t a -> Bool
-- isMember n = foldr (\x -> (||) (n==x)) False

-- NLU.Json manipulation

-- mentionRange :: Mention -> Range 
-- mentionRange (Mention _ [b,e] _) = (b,e)

cEntRange :: CleanEntity -> Range
cEntRange (CleanEntity _ (b:e:_) _) = (b,e)

cEntTOstr :: [CleanEntity] -> String
cEntTOstr l = "[" ++ intercalate "," (map (C.unpack . encode) l) ++ "]"

-- strTOent :: String -> [Entity]
-- strTOent s = fromRight [] (eitherDecode (C.pack s) :: Either String [Entity])

cleanEnts :: [Entity] -> [CleanEntity]
cleanEnts = map aux
  where
    aux (Entity _ t (m:_) _) = CleanEntity t (location m) (confidence m)

-- Conllu manipulation

readRange :: String -> Maybe Range
readRange t = if null l then Nothing else aux l
  where
    (_,_,_,l) = (=~) t "TokenRange=([0-9]+):([0-9]+)" :: (String,String,String,[String])
    aux (a:b:_) = Just (read a, read b)
    
cwRange :: CW AW -> Maybe Range 
cwRange w = (>>=) (_misc w) readRange


sentRange :: Sent -> Maybe Range
sentRange s = if length l < 2 then Nothing else aux l
  where
    w = _words s
    l = catMaybes [cwRange $ head w, cwRange $ last w]
    aux (a:b:_) = Just (fst a, snd b)

-- cwHead :: CW AW -> ID 
-- cwHead n = maybe (SID 0) _head $ _rel n