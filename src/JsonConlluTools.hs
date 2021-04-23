module JsonConlluTools where

import qualified Data.ByteString.Lazy.UTF8 as C
import Text.Regex.TDFA ( (=~) )
import Data.List ( intercalate )
import Data.Aeson ( eitherDecode, encode )
import Control.Applicative ( Applicative(liftA2) )
import Data.Maybe ( mapMaybe )
import Conllu.Type
import NLU

type Range = (Int,Int)

-- Verify if first Range is contained in second Range
isSubrange :: Range -> Range -> Bool
isSubrange (b1, e1) (b2, e2) = b1 >= b2 && e1 <= e2

-- Verify if element is in transversible object
isMember :: (Foldable t, Eq a) => a -> t a -> Bool
isMember n = foldr (\x -> (||) (n==x)) False


-- NLU.Json manipulation

entRange :: Entity -> Range
entRange (Entity _ _ ((Mention _ [a,b] _):_) _) = (a,b)

-- Show list of CleanMention into String form
cMenTOstr :: [CleanMention] -> String
cMenTOstr l = "[" ++ intercalate "," (map (C.toString . encode) l) ++ "]"

-- Read String to list of CleanMention
strTOcMen :: String -> Either String [CleanMention]
strTOcMen s = eitherDecode (C.fromString s) :: Either String [CleanMention]


-- Conllu manipulation

-- Read Ranges from Misc of Conllu format
readRange :: String -> Maybe Range
readRange t = if length l < 2 then Nothing else aux l
  where
    (_,_,_,l) = (=~) t "TokenRange=([0-9]+):([0-9]+)" :: (String,String,String,[String])
    aux (a:b:_) = Just (read a, read b)

-- Extract range of token      
cwRange :: CW AW -> Maybe Range 
cwRange w = (>>=) (_misc w) readRange

-- Extract range of Sent
sentRange :: Sent -> Maybe Range
sentRange (Sent _ w) = if length l < 2 then Nothing else Just (fst $ head l,snd $ last l)
  where
    l = mapMaybe cwRange w