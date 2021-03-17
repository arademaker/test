module JsonConlluTools where

import qualified Data.ByteString.Lazy.Char8 as C
import Text.Regex.TDFA ( (=~) )
import Data.List ( intercalate )
import Data.Aeson ( eitherDecode, encode )
import Control.Applicative ( Applicative(liftA2) )
import Conllu.Type
import NLU

type Range = (Int,Int)

isSubrange :: Range -> Range -> Bool
isSubrange (b1,e1) (b2,e2) = b1 >= b2 && e1 <= e2

isMember :: (Foldable t, Eq a) => a -> t a -> Bool
isMember n = foldr (\x -> (||) (n==x)) False


-- NLU.Json manipulation

entRange :: Entity -> Range
entRange (Entity _ _ ((Mention _ [a,b] _):_) _) = (a,b)

cMenTOstr :: [CleanMention] -> String
cMenTOstr l = "[" ++ intercalate "," (map (C.unpack . encode) l) ++ "]"

strTOcMen :: String -> Either String [CleanMention]
strTOcMen s = eitherDecode (C.pack s) :: Either String [CleanMention]


-- Conllu manipulation
readRange :: String -> Maybe Range
readRange t = if length l < 2 then Nothing else aux l
  where
    (_,_,_,l) = (=~) t "TokenRange=([0-9]+):([0-9]+)" :: (String,String,String,[String])
    aux (a:b:_) = Just (read a, read b)
    
cwRange :: CW AW -> Maybe Range 
cwRange w = (>>=) (_misc w) readRange

sentRange :: Sent -> Maybe Range
sentRange (Sent _ w) = liftA2 (\x y -> (fst x,snd y)) b e
  where
    [b,e] = [cwRange $ head w, cwRange $ last w]