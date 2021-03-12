module ConlluProcess where


import Data.Maybe
import Data.Either
import Control.Applicative
import System.Environment 
import System.Exit
import Conllu.IO
import Conllu.Type
import NLU
import JsonConlluTools




-- Verify if CleanEntity belongs to Sent
cEntINsent :: CleanEntity -> Sent -> Maybe Bool 
cEntINsent ce s = out where
  sRange = catMaybes [sentRange s]
  out = if null sRange then Nothing else Just $ isSubrange (cEntRange ce) (head sRange)

-- Update sent metadata with list of CleanEntity
metaUpdate :: Sent -> [CleanEntity] -> Sent
metaUpdate s e = Sent (_meta s ++ [("entities",cEntTOstr e)]) (_words s)

entFilter :: [CleanEntity] -> Sent -> [CleanEntity]
entFilter [] _ = []
entFilter (x:xs) s
  | cEntINsent x s == Just False = entFilter xs s
  | otherwise = x:entFilter xs s

addJson :: Either String Document -> Doc -> FilePath -> IO ()
addJson (Left s) _ _ = putStrLn $ "JSON INVÁLIDO: \n" ++ s
addJson (Right js) sents outpath 
  | isNothing $ sentRange $ head sents = putStrLn "CONLLU INVÁLIDO: \n Ranges de sentenças não encontrados"
  | otherwise = writeConlluFile outpath outConll
  where
    outConll = map (\s -> metaUpdate s $ entFilter (cleanEnts $ entities js) s) sents

merge :: [FilePath] -> IO ()
merge [jspath, clpath, outpath] = do
  esd <- readJSON jspath
  d <- readConlluFile clpath
  addJson esd d outpath

-- -- como fazer...




treeCheck :: [ID] -> [ID] -> Bool
treeCheck nodes heads = length roots < 2
  where
    roots = filter (\i -> not $ isMember i nodes) heads

-- Filter tokens from entity (nothing if original tokens have no range)
entTokens :: CleanEntity -> [CW AW] -> Either String [CW AW]
entTokens e l = if length l /= length ranges then invRanges else nl
  where
    invRanges = Left "Conllu inválido: \n Ranges dos tokens não encontrados"
    er = cEntRange e
    ranges = mapMaybe cwRange l
    nl = Right $ foldl (\l (c,r) -> if isSubrange r er then c:l else l) [] (zip l ranges)
    
headCheck :: [CW AW] -> Either String [ID]
headCheck ls
  | length rel /= length ls = Left "Conllu inválido: \n Heads dos tokens não encontrados"
  | otherwise = Right $ map _head rel
  where
    rel = mapMaybe _rel ls

cEntCheck :: CleanEntity -> [CW AW] -> Either String Bool
cEntCheck e l = liftA2 aux tokens heads
  where
    tokens = entTokens e l
    heads = headCheck l
    aux cws heads = treeCheck (map _id cws) heads

jsonCheck :: [CleanEntity] -> [CW AW] -> Either String [CleanEntity]
jsonCheck es cs
  | null l = Right $ foldl (\l (c,b) -> if b then c:l else l) [] cws
  | otherwise = Left $ "Conllu inválido: \n Erro no JSON: " ++ head l
  where
    el = map (`cEntCheck` cs) es
    l = lefts el
    cws = zip es $ rights el

sentCheck :: Sent -> Either String [CleanEntity]
sentCheck s = (>>=) ents (`jsonCheck` _words s)
  where
    ents = strTOcEnts $ snd $ last $ _meta s

check :: [FilePath] -> IO ()
check (p:_) = do
  clu <- readConlluFile p
  let cs = map sentCheck clu
      l = lefts cs
      r = rights cs
  print l

-- sentCheck :: Sent -> Either String [CleanEntity]
-- sentCheck s = (>>=) ent f
--   where
--     ent = strTOcEnts $ snd $ last $ _meta s
--     w = _words s
--     f e = Right $ filter (`cEntCheck` w) e

-- cluCheck :: [Either String [CleanEntity]] -> IO ()
-- cluCheck xs | not $ null ls = putStrLn $ "Conllu inválido: \n" ++ head ls
--             | otherwise = print $ concat $ rights xs
--               where
--                 ls = lefts xs

-- check :: [FilePath] -> IO ()
-- check (p:_) = do
--   clu <- readConlluFile p
--   cluCheck $ map sentCheck clu

-- -- main interface

msg =
  " Usage: \n\
  \  test-conllu -m JSON-file CONLLU-file CONLLU-file\n\
  \  test-conllu -c CONLLU-file  => NER and POS check to stdout\n"

help = putStrLn msg

parse ["-h"]    = help >> exitSuccess
parse ("-m":ls) = merge ls >> exitSuccess
parse ("-c":ls) = check ls >> exitSuccess
parse ls        = help >> exitFailure
    
main :: IO ()
main = getArgs >>= parse