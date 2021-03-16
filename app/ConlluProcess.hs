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


-- File Merge Section





-- Verify if CleanEntity belongs to Sent
entINsent :: Entity -> Sent -> Maybe Bool 
entINsent e s = out where
  sRange = catMaybes [sentRange s]
  out = if null sRange then Nothing else Just $ isSubrange (entRange e) (head sRange)




prevRange _ (_,Nothing) = False
prevRange (x,y) (i,Just (a,b)) = b < x

outRange _ (_,Nothing) = False
outRange (x,y) (i,Just (a,b)) = a > y

aux [] _ = []
aux (x:xs) er | prevRange er x = aux xs er
              | outRange er x = []
              | otherwise = fst x : aux xs er


entClean :: Entity -> Sent -> CleanMention
entClean e s = CleanMention (etext e) (location $ head $ mentions e) t_range
  where
    w = _words s
    t = zip (map _id w) (map cwRange w)
    er = entRange e
    nodes = aux t er
    t_range = map (\(SID x) -> x) [head nodes,last nodes]



-- Update sent metadata with list of CleanEntity
metaUpdate :: Sent -> [Entity] -> Sent
metaUpdate s e = Sent (_meta s ++ [("entities",cMenTOstr $ map (`entClean` s) e)]) (_words s)

-- Filter CleanEntity list with the ones that belong to the Sent given
entFilter :: [Entity] -> Sent -> [Entity]
entFilter [] _ = []
entFilter (x:xs) s
  | entINsent x s == Just False = entFilter xs s
  | otherwise = x:entFilter xs s

-- Recieves the NLU.Document (or an reading error), a Conllu.Doc and a out_file path
-- to print the error or create the out conllu file with the cleanentities in the metadata
addJson :: Either String Document -> Doc -> FilePath -> IO ()
addJson (Left s) _ _ = putStrLn $ "JSON INVÁLIDO: \n" ++ s
addJson (Right js) sents outpath 
  | null $ mapMaybe sentRange sents = putStrLn "CONLLU INVÁLIDO: \n Ranges de sentenças não encontrados"
  | otherwise = writeConlluFile outpath outConll
  where
    outConll = map (\s -> metaUpdate s $ entFilter (entities js) s) sents

-- Recieves the filepaths, opens the files and calls addJson
merge :: [FilePath] -> IO ()
merge [jspath, clpath, outpath] = do
  esd <- readJSON jspath
  d <- readConlluFile clpath
  addJson esd d outpath
merge _ = help >> exitFailure


{-



-- Check section


-- Recieves nodes IDs list, nodes heads list and verifies tree consistance
treeCheck :: [ID] -> [ID] -> Bool
treeCheck nodes heads = length roots < 2
  where
    roots = filter (\i -> not $ isMember i nodes) heads

-- Filter tokens that belong to entity (returning the error if there are no tokens in the conllu)
entTokens :: CleanEntity -> [CW AW] -> Either String [CW AW]
entTokens e l = if null ranges then invRanges else nl
  where
    invRanges = Left "Conllu inválido: \n Ranges dos tokens não encontrados"
    er = cEntRange e
    ranges = mapMaybe cwRange l
    nl = Right $ foldl (\l (c,r) -> if isSubrange r er then c:l else l) [] (zip l ranges)
    
-- Recieve nodes list to produce the heads list, returning an error if they are Nothing
-- (_rel, that contains the head, are maybe objects at the conllu structure)
headCheck :: [CW AW] -> Either String [ID]
headCheck ls
  | null rel = Left "Conllu inválido: \n Heads dos tokens não encontrados"
  | otherwise = Right $ map _head rel
  where
    rel = mapMaybe _rel ls

-- Recieves CleanEntity and the nodes list to check its consistence (previous errors can be spread)
cEntCheck :: CleanEntity -> [CW AW] -> Either String Bool
cEntCheck e l = liftA2 aux tokens heads
  where
    tokens = entTokens e l
    heads = headCheck l
    aux cws heads = treeCheck (map _id cws) heads

-- Takes list of CleanEntities and nodes list to produce a list of the unconsistent CleanEntities
-- (spreads the possible errors and returns one if Json is not valid)
jsonCheck :: [CleanEntity] -> [CW AW] -> Either String [CleanEntity]
jsonCheck es cs
  | null l = Right $ foldl (\l (c,b) -> if b then c:l else l) [] cws
  | otherwise = Left $ "Conllu inválido: \n Erro no JSON: " ++ head l
  where
    el = map (`cEntCheck` cs) es
    l = lefts el
    cws = zip es $ rights el

-- Take Conllu.Sent to map it to a list of cleanEntities that are inconsistant
-- (spreads the possible errors as strings)
sentCheck :: Sent -> Either String [CleanEntity]
sentCheck s = (>>=) ents (`jsonCheck` _words s)
  where
    ents = strTOcEnts $ snd $ last $ _meta s

-- Recives the filepath, reads the file and map sentCheck
check :: [FilePath] -> IO ()
check (p:_) = do
  clu <- readConlluFile p
  let cs = map sentCheck clu
      l = lefts cs
      r = concat $ rights cs
  putStrLn $ if null l
    then (if null r then "No inconsistences" else show r)
    else head l

-}

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