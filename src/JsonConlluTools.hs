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

isMember :: (Foldable t, Eq a) => a -> t a -> Bool
isMember n = foldr (\x -> (||) (n==x)) False

-- NLU.Json manipulation

mentionRange :: Mention -> Range 
mentionRange (Mention _ [b,e] _) = (b,e)

entRanges :: Entity -> [Range]
entRanges e = map mentionRange $ mentions e

entTOstr :: [Entity] -> String
entTOstr l = "[" ++ intercalate "," (map (C.unpack . encode) l) ++ "]"

strTOent :: String -> [Entity]
strTOent s = fromRight [] (eitherDecode (C.pack s) :: Either String [Entity])

-- Conllu manipulation

readRange :: String -> Maybe Range
readRange t = mr
  where
    (_,_,_,l) = (=~) t "TokenRange=([0-9]+):([0-9]+)" :: (String,String,String,[String])
    mr = if length l < 2 then Nothing else Just (read $ l!!0, read $ l!!1)
    
cwRange :: CW AW -> Maybe Range 
cwRange w = readRange $ fromMaybe "" $ _misc w 

sentRange :: Sent -> Maybe Range
sentRange s = mr where
    w = _words s
    l = catMaybes [cwRange $ head w, cwRange $ last w]
    mr = if length l < 2 then Nothing else Just (fst $ l!!0, snd $ l!!1)

-- cwHead :: CW AW -> ID 
-- cwHead n = maybe (SID 0) _head $ _rel n