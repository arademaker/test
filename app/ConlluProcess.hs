module ConlluProcess where

-- Provavelmente não precisarei de todos esses imports. Tirarei quando o codigo estiver pronto
import Conllu.IO
import Conllu.DeprelTagset
import Conllu.Diff
import Conllu.Parse
import Conllu.Print
import Conllu.Type
import Conllu.UposTagset
import Conllu.Utils
import System.Environment
import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe
import DHBB_JSON
import Data.Either

type Range = (Int,Int)

range :: ReadP Range -- Range parser
range = do
    _ <- manyTill get (string "TokenRange=")
    b <- munch isDigit
    char ':'
    e <- munch isDigit
    return (read b, read e)

cwRange :: CW AW -> Range -- Take range of element
cwRange w = fst $ head $ readP_to_S range $ fromJust $ _misc w

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



-- To Be Continued..


main = putStrLn "Breve módulo de manipulação conllu"


------ Older implementation (might rescue some functions)

-- sentFind :: [Sent] -> (Int, Int) -> Maybe Sent -- Find sentence that contains given range
-- sentFind l t = do
--     let ranges = filter (isSubrange t . sentRange) l
--     return $ if null ranges then Nothing else Just $ head ranges

-- isSubTree :: [CW AW] -> Bool -- Check if a list of words is a unique subtree
-- isSubTree w = do
--     let sid = map _id w
--         heads = map (_head . fromJust . _rel) w
--         outsiders = filter (`notElem` sid) heads
--     return $ length outsiders <= 1 

-- verify :: Sent -> (Int,Int) -> Bool -- Check if an entity token range is consistent with a sentence subtree
-- verify entSent entRange = do
--     let content = filter (\e -> isSubrange (cwRange e) entRange) $ _words entSent
--     return isSubTree content

-- readCONLLU conlluPath begin end= do
--     x <- readConllu conlluPath
--     let sents = head x
--         entRange = (read begin, read end)
--         entSent = sentFind sents entRange
--         consistence = verify (fromJust entSent) entRange
--     putStrLn $ if consistence then "Esse arquivo é consistente" else "Esse arquivo NÃO é consistente"

-- data SintaticTree = SintaticTree{
--     _nid :: ID,
--     _range :: Range,
--     _subt :: [SintaticTree]
-- } deriving (Show)

-- data RangeTree = RangeTree{
--     _r :: Range,
--     _sub :: [RangeTree]
-- } deriving (Show)

-- cwHead :: CW AW -> ID
-- cwHead = _head . fromJust . _rel

-- nodePath :: CW AW -> SintaticTree -> [Int]
-- nodePath c (SintaticTree id r st) | 

-- insertNode :: CW AW -> SintaticTree -> SintaticTree


-- putNode :: CW AW -> SintaticTree -> SintaticTree
-- putNode c t | _nid t == cwHead c = SintaticTree (_nid t) (_range t) (SintaticTree (_id c) (cwRange c) []:_subt t)
--             | otherwise = SintaticTree (_nid t) (_range t) (map (putNode c) $ _subt t)

-- sidINtree :: ID -> SintaticTree -> Bool
-- sidINtree i (SintaticTree id r subt) | id == i = True
--                                      | null subt = False
--                                      | otherwise = any (sidINtree i) subt

-- auxTree :: [CW AW] -> SintaticTree -> SintaticTree
-- auxTree w t | null w = t
--             | otherwise = auxTree nw nt where
--                 nt = foldl (flip putNode) t w
--                 nw = filter (\c -> not (sidINtree (_id c) nt)) w

-- rangeMerge :: [Range] -> Range
-- rangeMerge = foldl1 (\(b1,e1) (b2,e2) -> (min b1 b2, max e1 e2))

-- rangeUpdate :: SintaticTree -> SintaticTree
-- rangeUpdate (SintaticTree id r st) = if null st then SintaticTree id r st
--                                      else SintaticTree id (rangeMerge $ r:map _range nst) nst
--                                      where nst = map rangeUpdate st

-- sentTree :: Sent -> SintaticTree
-- sentTree s = auxTree (_words s) (SintaticTree (SID 0) (0,0) [])



-- flatten :: SintaticTree -> Range
-- flatten t | null $ _subt t = _range t
--           | otherwise = rangeMerge (_range t) (foldl (\r st -> rangeMerge r (flatten st)) (0,0) (_subt t))

-- sintaticTOrange :: SintaticTree -> RangeTree
-- sintaticTOrange t = RangeTree (flatten t) (map sintaticTOrange $ _subt t)

-- insertTree :: SintaticTree -> CW AW -> SintaticTree
-- insertTree t w | _nid t == _head (fromJust $ _rel w) = SintaticTree
--                     (_nid t)
--                     (_range t)
--                     (SintaticTree (_id w) (cwRange w) [] :_subt t)
--                | otherwise = SintaticTree
--                     (_nid t)
--                     (_range t)
--                     (map (`insertTree` w) $ _subt t)

-- sentTree :: Sent -> SintaticTree


-- docTree :: Doc -> SintaticTree
-- docTree doc = SintaticTree (SID 0) (fst $ sentRange $ head doc, snd $ sentRange $ last doc) $ map sentTree doc