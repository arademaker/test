module JsonConlluTools where

import qualified Data.ByteString.Lazy.Char8 as C
import Text.Regex.TDFA
import Data.Maybe
import Data.List
import Data.Aeson
import Data.Either
import Control.Applicative
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

strTOcEnts :: String -> Either String [CleanEntity]
strTOcEnts s = eitherDecode (C.pack s) :: Either String [CleanEntity]

cleanEnts :: [Entity] -> [CleanEntity]
cleanEnts = map (\(Entity _ t (m:_) _) -> CleanEntity t (location m) (confidence m))


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


-- cwHead :: CW AW -> ID 
-- cwHead n = maybe (SID 0) _head $ _rel n