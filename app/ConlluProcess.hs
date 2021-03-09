module ConlluProcess where


import System.Environment 
import System.Exit
import Conllu.IO
import Conllu.Type
import NLU
import JsonConlluTools




-- -- Verify if entity belongs to sent
-- entINsent :: Entity -> Sent -> Bool 
-- entINsent e s = any (`isSubrange` sentRange s) (entRanges e)

-- metaUpdate :: Sent -> [Entity] -> Sent -- Update Sent metadata with entities
-- metaUpdate s e = Sent (_meta s ++ [("entitys",entTOstr e)]) (_words s)






-- addJson :: Either String Document -> Doc -> FilePath -> IO ()
-- addJson (Left _) _ _ = putStrLn "JSON inválido"
-- addJson (Right js) sents outpath = writeConlluFile outpath outConll
--   where
--     outConll = map (\s -> metaUpdate s $ filter (`entINsent` s) (entities js)) sents

-- merge :: [FilePath] -> IO ()
-- merge [jspath, clpath, outpath] = do
--   js <- readJSON jspath
--   sents <- readConlluFile clpath
--   addJson js sents outpath

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
-- parse ("-m":ls) = merge ls >> exitSuccess
-- parse ("-c":ls) = check ls >> exitSuccess
parse ls        = help >> exitFailure
    
main :: IO ()
main = getArgs >>= parse