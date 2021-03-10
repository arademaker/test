module ConlluProcess where


import Data.Maybe
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
metaUpdate s e = Sent (_meta s ++ [("entitys",cEntTOstr e)]) (_words s)

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

-- entCheck:: Entity -> [CW AW] -> Bool
-- entCheck e l = res where
--   er = head $ entRanges e
--   nl = filter (\c -> isSubrange (cwRange c) er) l
--   nodes = map _id nl
--   roots = filter (\c -> not $ isMember (cwHead c) nodes) nl
--   res = length roots > 1

-- sentCheck :: Sent -> [Entity]
-- sentCheck s = filter (`entCheck` w) ent where
--   ent = strTOent $ snd $ last $ _meta s
--   w = _words s

-- check :: [FilePath] -> IO ()
-- check (p:_) = do
--   c <- readConlluFile p
--   print $ foldl (\r s -> r ++ sentCheck s) [] c


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