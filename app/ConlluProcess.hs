module ConlluProcess where


import Data.Maybe 
import Data.Either 
import Control.Applicative
import System.Environment 
import System.Exit 
import Data.List
import Conllu.IO
import Text.Regex.TDFA
import System.FilePath
import Conllu.Type
import NLU
import JsonConlluTools
import Data.Char


-- File Merge Section






-- Sent Ranges list of Conllu Document
sRanges :: Doc -> [Range]
sRanges doc = tail $ foldl (\l sent -> l ++ [(cur_char l, cur_char l + length sent)]) [(-1,-1)] s
  where
    s = map (snd . last . _meta) doc
    cur_char l = snd (last l) + 1

putLen :: CW AW -> Maybe Int -> CW AW
putLen c Nothing = c
putLen (CW id form lemma upos xpos feats rel deps misc) (Just x)
  | isNothing misc = CW id form lemma upos xpos feats rel deps
                        (Just $ "TokenRange="++show x++":"++show (x+length (fromJust form)))
  | otherwise = CW id form lemma upos xpos feats rel deps
                   ((<$>) (++"|TokenRange="++show x++":"++show (x+length (fromJust form))) misc)


aux :: String -> [CW AW] -> Int -> [CW AW]
aux str (t:ts) begin | null (t:ts) = []
                     | (t:ts) == (t:[]) = [putLen t pos]
                     | foldl (\b c -> and [b, (not $ or [isAlphaNum c, isPunctuation c])]) True str = (t:ts)
                     | b = (putLen t pos):(aux (drop tam nstr) ts ((fromJust pos) + tam))
                     | otherwise = t:(aux str ts begin)
  where
    form = (fromJust (_form t))
    (b, nstr) = (isNextToken str t)
    pos = ((<$>) (+ begin) (subStrPos form str))
    tam = length form


isNextToken :: String -> CW AW -> (Bool, String)
isNextToken str t | head str == ' ' = isNextToken (drop 1 str) t
                  | otherwise = (and [isPrefixOf form str, not subword], str)
  where
    form = fromJust (_form t)
    next = head $ drop (length form) str
    subword = and $ map isAlphaNum [last form, next]

subStrPos :: String -> String -> Maybe Int
subStrPos sub str = (($ tails str) . findIndex . isPrefixOf) sub

-- addRange :: Sent -> Range -> Sent
-- addRange (Sent m w) (x,_) = Sent m nw
--   where
--     sent = snd $ last m
--     nw = map (\c -> putLen c (subStrPos (fromJust $ _form c) sent)) w

addRange :: Sent -> Range -> Sent
addRange (Sent m w) (b,_) = Sent m (aux text w b)
  where
    text = snd $ last m


-- Take conllu and add tokenranges
putRanges :: Doc -> Doc
putRanges doc = map (\(s,r) -> addRange s r) (zip doc (sRanges doc))













-- Verify if CleanEntity belongs to Sent
entINsent :: Entity -> Sent -> Maybe Bool 
entINsent e s = out where
  sRange = catMaybes [sentRange s]
  out = if null sRange then Nothing else Just $ isSubrange (entRange e) (head sRange)

-- Find t-range given (ID,Maybe Range) tuple list and entity range (284,300)
rangeTOtoken :: [(ID,Maybe Range)] -> Range -> [ID]
rangeTOtoken ls er = fst $ foldl aux ([],False) ls
  where
    jr (Just t) = t
    aux (l,b) (i,mr) | not b && isNothing mr = (l,b)
                     | not b && (fst (jr mr) == fst er) = (i:l,True)
                     | not b = (l,b)
                     | b && isNothing mr = (i:l,b)
                     | b && (fst (jr mr) >= snd er) = (l,False)
                     | b = (i:l,b)

-- Transform Entity to CleanMention using its Sent
entClean :: Entity -> Sent -> Maybe CleanMention
entClean e s = if null nodes then Nothing else
               Just $ CleanMention (etext e) (location $ head $ mentions e) t_range
  where
    w = _words s
    nodes = rangeTOtoken (zip (map _id w) (map cwRange w)) (entRange e)
    t_range = map (\(SID x) -> x) [last nodes,head nodes]

-- Update sent metadata with list of CleanEntity
metaUpdate :: Sent -> [Entity] -> Sent
metaUpdate s e = Sent (_meta s ++[("mentions", cMenTOstr $ mapMaybe (`entClean` s) e)]) (_words s)

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
  | null $ mapMaybe sentRange sents = writeConlluFile outpath outConll2
  | otherwise = writeConlluFile outpath outConll
  where
    outConll = map (\s -> metaUpdate s $ entFilter (entities js) s) sents
    outConll2 = map (\s -> metaUpdate s $ entFilter (entities js) s) (putRanges sents)

-- Recieves the filepaths, opens the files and calls addJson
merge :: [FilePath] -> IO ()
merge [jspath, clpath, outpath] = do
  esd <- readJSON jspath
  d <- readConlluFile clpath
  addJson esd d outpath
merge _ = help >> exitFailure



-- Check section


-- Recieves nodes IDs list, nodes heads list and verifies tree consistance
treeCheck :: [ID] -> [ID] -> Bool
treeCheck nodes heads = length roots > 1
  where
    roots = filter (\i -> not $ isMember i nodes) heads

-- Recieve nodes list to produce the heads list, returning an error if they are Nothing
-- (_rel, that contains the head, are maybe objects at the conllu structure)
headCheck :: [CW AW] -> Either String [ID]
headCheck ls
  | null rel = Left "Conllu inválido: \n Heads dos tokens não encontrados"
  | otherwise = Right $ map _head rel
  where
    rel = mapMaybe _rel ls

-- Recieves CleanEntity and the nodes list to check its consistence (previous errors can be spread)
cEntCheck :: CleanMention -> [CW AW] -> Either String Bool
cEntCheck m l = liftA2 treeCheck (Right tokens) heads
  where
    rt = range_t m
    tokens = map SID [head rt .. last rt]
    heads = headCheck $ filter (\x -> isMember (_id x) tokens) l

-- Takes list of CleanEntities and nodes list to produce a list of the unconsistent CleanEntities
-- (spreads the possible errors and returns one if Json is not valid)
jsonCheck :: [CleanMention] -> [CW AW] -> Either String [CleanMention]
jsonCheck es cs
  | null l = Right $ foldl (\l (c,b) -> if b then c:l else l) [] cws
  | otherwise = Left $ "Conllu inválido: \n Erro no JSON: " ++ head l
  where
    el = map (`cEntCheck` cs) es
    l = lefts el
    cws = zip es $ rights el

-- Take Conllu.Sent to map it to a list of cleanEntities that are inconsistant
-- (spreads the possible errors as strings)
sentCheck :: Sent -> Either String [CleanMention]
sentCheck s = (>>=) ment (`jsonCheck` _words s)
  where
    ment = strTOcMen $ snd $ last $ _meta s

-- Recives the filepath, reads the file and map sentCheck
check :: [FilePath] -> IO ()
check [p] = do
  clu <- readConlluFile p
  let cs = map sentCheck clu
      l = lefts cs
      r = concat $ rights cs
  putStrLn $ if null l
    then (if null r then "No inconsistences" else show r)
    else head l
check _ = help >> exitFailure



-- main interface


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