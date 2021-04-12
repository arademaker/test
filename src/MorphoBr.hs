
module MorphoBr where

import Data.Either
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Char (ord)
import Data.Trie
import Data.List

packStr, packStr', packStr'' :: String -> B.ByteString
packStr   = B.pack . map (fromIntegral . ord)
packStr'  = C.pack
packStr'' = encodeUtf8 . T.pack

toBS :: [(String,String)] -> [(B.ByteString,String)]
toBS = map (\(a,b) -> (packStr a, b))

getKey :: [(B.ByteString,String)] -> [(B.ByteString,[String ])]
getKey l = map aux (groupBy (\la lb -> fst la == fst lb) l) 
 where aux (x:xs) 
        | length (x:xs) == 1 = (fst x, [snd x])
        | otherwise = (fst x, map snd (x:xs))

struc :: [(String,String)] -> [(B.ByteString,[String])]
struc (x:xs) = getKey $ toBS (x:xs)
struc _ = []

getPairs :: String -> [(String,String )] 
getPairs verbs = map (aux . words) (lines verbs)
 where aux l = (head l, last l)

merge :: [(String,String )] -> [(String,String )] -> [(String,String )] 
merge (x:xs) (y:ys) = if fst x < fst y
                        then x: merge xs (y:ys)
                        else y: merge (x:xs) ys
merge [] xs = xs
merge xs [] = xs

createList :: [String] -> [(String,String)]
createList words = aux (map getPairs words)
 where aux xs = foldl merge [] xs

createTreeList :: [String] -> [(B.ByteString,[String])]
createTreeList = struc . createList 

createTree :: [String] -> Trie [String]
createTree = fromList . createTreeList



--fromList cria a arvores a partir de uma lista
--para pesquisar na trie: lookup
    
-- <$> writeFile path (show createTree)
