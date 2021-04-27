module ConlluProcess where


import Data.Maybe ( mapMaybe, catMaybes, fromJust, isNothing )
import Data.Either ( rights, lefts )
import Control.Applicative ( Applicative(liftA2) )
import System.Environment ( getArgs )
import System.Exit ( exitSuccess, exitFailure )
import Conllu.IO ( readConlluFile, writeConlluFile )
import Data.List ( find, isPrefixOf )
import Data.Char ( isAlphaNum, isPunctuation )
import Conllu.Type
import NLU
import JsonConlluTools



-- Token ranges identification section

auxSentRanges :: Int -> String -> [String] -> [Range]
auxSentRanges _ _ [] = []
auxSentRanges n text (x:xs) = (i, i + length x):(auxSentRanges (i + (length x) - 1) text xs)
  where
    i = n + (fromJust $ subStrPos x $ drop n text)

-- List of intervals where the sentences are contained.
sRanges :: Doc -> String -> [Range]
sRanges doc text = auxSentRanges 0 text s
  where
    s = map (snd . fromJust . ((find (\(first, _) -> first == "text ")) . _meta)) doc

-- Write a range of a CW AW given it and maybe its initial position.
writeRange :: CW AW -> Maybe Int -> CW AW
writeRange c Nothing = c
writeRange (CW id form lemma upos xpos feats rel deps misc) (Just x)
  | isNothing misc = CW id form lemma upos xpos feats rel deps
                        (Just $ "TokenRange="++show x++":"++show (x+length (fromJust form)))
  | otherwise = CW id form lemma upos xpos feats rel deps
                   ((<$>) (++"|TokenRange="++show x++":"++show (x+length (fromJust form))) misc)

-- Find and annotate the token ranges into the CW AW list with the original string and its begin
findRanges :: String -> [CW AW] -> Int -> [CW AW]
findRanges str (t:ts) begin | null (t:ts) = []
                                   | (t:ts) == [t] = [writeRange t pos]
                                   | foldl (\b c -> b && not (isAlphaNum c || isPunctuation c)) 
                                           True 
                                           str = t:ts
                                   | b = writeRange t pos:findRanges (drop tam nstr) ts (fromJust pos + tam)
                                   | otherwise = t:findRanges str ts begin
  where
    form = fromJust (_form t)
    (b, nstr) = isNextToken str t
    pos = (<$>) (+ begin) (subStrPos form str)
    tam = length form

-- Validation if token is next component of a string (necessary in cases that tokens are split by UDPipe)
isNextToken :: String -> CW AW -> (Bool, String)
isNextToken str t | head str == ' ' = isNextToken (drop 1 str) t
                  | otherwise = ((form `isPrefixOf` str) && not subword, str)
  where
    form = fromJust (_form t)
    next = str !! max 0 (length form)
    subword = all isAlphaNum [last form, next]

-- Take conllu and add tokenranges.
putRanges :: Doc -> String -> Doc
putRanges doc text = 
  zipWith (\(Sent m w) (b,_) -> Sent m (findRanges (snd 
                                                    (fromJust 
                                                      (find (\(first, _) -> first == "text ") 
                                                            m))) 
                                                   w 
                                                   b)) 
          doc 
          (sRanges doc text)



-- File Merge Section.


-- Verify if CleanEntity belongs to Sent
entINsent :: Entity -> Sent -> Maybe Bool
entINsent e s = out where
  sRange = catMaybes [sentRange s]
  out = if null sRange then Nothing else Just $ isSubrange (entRange e) (head sRange)

-- Find t-range given (ID, Maybe Range) tuple list and entity range (284,300)
rangeTOtoken :: [(ID, Maybe Range)] -> Range -> [ID]
rangeTOtoken ls er = fst $ foldl aux ([], False) ls
  where
    jr (Just t) = t
    aux (l, b) (i, mr) | not b && isNothing mr = (l, b)
                       | not b && fst (jr mr) == fst er = (i:l, True)
                       | not b = (l, b)
                       | b && isNothing mr = (i:l, b)
                       | b && fst (jr mr) >= snd er = (l, False)
                       | b = (i:l, b)

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
metaUpdate s e = Sent (_meta s ++ [("mentions", cMenTOstr $ mapMaybe (`entClean` s) e)]) (_words s)

-- Filter CleanEntity list with the ones that belong to the Sent given
entFilter :: [Entity] -> Sent -> [Entity]
entFilter [] _ = []
entFilter (x:xs) s
  | entINsent x s == Just False = entFilter xs s
  | otherwise = x:entFilter xs s

-- Receives the NLU.Document (or an reading error), a Conllu.Doc
-- to print the error or apply the check.
addJsonAndCheck :: Either String Document -> Doc -> IO ()
addJsonAndCheck (Left s) _ = putStrLn $ "JSON INVÁLIDO: \n" ++ s
addJsonAndCheck (Right js) sents = check outCll
  where
    text = analyzed_text js
    outCll = map (\s -> metaUpdate s $ entFilter (entities js) s)
                 (if null (mapMaybe sentRange sents) then putRanges sents text else sents)

-- Recieves the filepaths, opens the files and calls addJsonAndCheck
mergeAndCheck :: [FilePath] -> IO ()
mergeAndCheck [jspath, clpath] = do
  esd <- readJSON jspath
  d <- readConlluFile clpath
  addJsonAndCheck esd d
mergeAndCheck _ = help >> exitFailure


-- Functions equivalent to the previous one can only merge and write

-- Recieves the NLU.Document (or an reading error), a Conllu.Doc and a out_file path
-- to print the error or create the out conllu file with the cleanentities in the metadata
addJson :: Either String Document -> Doc -> FilePath -> IO ()
addJson (Left s) _ _ = putStrLn $ "JSON INVÁLIDO: \n" ++ s
addJson (Right js) sents outpath = writeConlluFile outpath outCll
  where
    text = analyzed_text js
    outCll = map (\s -> metaUpdate s $ entFilter (entities js) s)
                 (if null (mapMaybe sentRange sents) then putRanges sents text else sents)

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

-- Recieves CleanMention and the nodes list to check its consistence (previous errors can be spread)
cEntCheck :: CleanMention -> [CW AW] -> Either String Bool
cEntCheck m l = liftA2 treeCheck (Right tokens) heads
  where
    rt = range_t m
    tokens = map SID [head rt .. last rt]
    heads = headCheck $ filter (\x -> isMember (_id x) tokens) l

-- Takes list of CleanMentions and nodes list to produce a list of the unconsistent CleanMentions
-- (spreads the possible errors and returns one if Json is not valid)
jsonCheck :: [CleanMention] -> [CW AW] -> Either String [CleanMention]
jsonCheck es cs
  | null l = Right $ foldl (\l (c,b) -> if b then c:l else l) [] cws
  | otherwise = Left $ "Conllu inválido: \n Erro no JSON: " ++ head l
  where
    el = map (`cEntCheck` cs) es
    l = lefts el
    cws = zip es $ rights el

-- Take Conllu.Sent to map it to a list of CleanMentions that are inconsistant
-- (spreads the possible errors as strings)
sentCheck :: Sent -> Either String [CleanMention]
sentCheck s = (>>=) ment (`jsonCheck` _words s)
  where
    ment = strTOcMen $ snd $ last $ _meta s

-- Recives the doc, reads the file and map sentCheck
check :: Doc -> IO ()
check clu = do
  let cs = map sentCheck clu
      l = lefts cs
      r = concat $ rights cs
  putStrLn $ if null l
    then (if null r then "No inconsistences" else show r)
    else head l



-- main interface


msg =
  " Usage: \n\
  \  test-conllu -m JSON-file CONLLU-file CONLLU-OUTPUT \n\
  \  test-conllu -c JSON-file CONLLU-file \n"

help = putStrLn msg

parse ["-h"]    = help >> exitSuccess
parse ("-m":ls) = merge ls >> exitSuccess
parse ("-c":ls) = mergeAndCheck ls >> exitSuccess
parse ls        = help >> exitFailure

main :: IO ()
main = getArgs >>= parse