module ConlluProcess where


import Data.Maybe
import Data.Either
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


-- cEntCheck :: CleanEntity -> [CW AW]








-- cEntCheck :: CleanEntity -> [CW AW] -> Bool

-- cEntCheck e l = res where
--   er = cEntRange e
--   nl = filter (\c -> isSubrange (cwRange c) er) l

-- cEntCheck e l = res where
--   er = head $ entRanges e
--   nl = filter (\c -> isSubrange (cwRange c) er) l
--   nodes = map _id nl
--   roots = filter (\c -> not $ isMember (cwHead c) nodes) nl
--   res = length roots > 1

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
-- parse ("-c":ls) = check ls >> exitSuccess
parse ls        = help >> exitFailure
    
main :: IO ()
main = getArgs >>= parse