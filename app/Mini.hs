
module Mini where

import Data.Char (toLower)
import Data.Maybe ( catMaybes, isNothing, mapMaybe, fromJust )
import Data.Either ( lefts, rights )
import Data.List
import Control.Applicative ( Applicative(liftA2) )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.IO (appendFile)
import System.FilePath.Posix
import Conllu.IO ( readConlluFile, writeConlluFile, readConllu )
import Conllu.Type
import JsonConlluTools
import qualified Data.Trie as T
import qualified MorphoBr as M
import qualified Conllu.UposTagset as U
import qualified Conllu.DeprelTagset as D


---- Merge section 

search :: Maybe [String] -> [String]
search ls = if isNothing ls
              then ["not found"]
              else  map (map toLower) (fromJust ls)

addClass :: T.Trie [String] -> CW AW -> CW AW
addClass trie word = word{_deps = [Rel{_head = SID 1
                                      ,_deprel = D.ACL
                                      ,_subdep = Just $ head (aux word)
                                      ,_rest = Just $ tail (aux word)}]}
 where
  aux word = search $ T.lookup (M.packStr $ map toLower ( fromJust $_form word)) trie

featCheck :: T.Trie [String] -> CW AW -> CW AW
featCheck trie word
  | isNothing(_upos word)  = word
  | fromJust (_upos word) == U.VERB  = addClass trie word
  | fromJust (_upos word) == U.NOUN  = addClass trie word
  | fromJust (_upos word) == U.ADJ   = addClass trie word
  | fromJust (_upos word) == U.ADV   = addClass trie word
  | otherwise = word

addMorphoInfo :: T.Trie [String] -> Doc -> Doc
addMorphoInfo trie = map aux
 where
   aux :: Sent -> Sent
   aux sent =  sent { _words = map (featCheck trie) (_words sent)}

createFilePath :: FilePath -> FilePath -> FilePath
createFilePath directory cl = addExtension (combine directory (takeBaseName cl)) "conllu"

-- [outpath, jsonPath, conllu files]
merge :: [FilePath] -> IO [()]
merge (x:y:xs) = do
  tl  <- M.readJSON y
  mapM (aux x (T.fromList $ M.getList tl)) xs
 where aux directory tl clpath = do
        cl <- readConlluFile clpath
        writeConlluFile (createFilePath directory clpath) $ addMorphoInfo tl cl


---- Check section


member :: String -> [String] -> Bool
member x [] = False
member x (y:ys) | x==y = True
                | otherwise = member x ys

indFeat :: [Feat] -> String
indFeat (x:xs)
  | (_feat x == "VerbForm") && (head (_featValues x) == "Fin") = indFeat xs
  | (_feat x == "Tense") && (head (_featValues x) == "Fut")  = "+fut"  ++ (getFeatValues xs)
  | (_feat x == "Tense") && (head (_featValues x) == "Past") = "+prf"  ++ (getFeatValues xs)
  | (_feat x == "Tense") && (head (_featValues x) == "Pres") = "+prs"  ++ (getFeatValues xs)
  | (_feat x == "Tense") && (head (_featValues x) == "Imp")  = "+impf" ++ (getFeatValues xs)
  | (_feat x == "Tense") && (head (_featValues x) == "Pqp")  = "+pqp"  ++ (getFeatValues xs)
  | otherwise = getFeatValues (x:xs)
indFeat [] = ""

subFeat :: [Feat] -> String
subFeat (x:xs)
  | (_feat x == "VerbForm") && (head (_featValues x) == "Fin")  = subFeat xs
  | (_feat x == "Tense")    && (head (_featValues x) == "Fut")  = "+sbjf" ++ (getFeatValues xs)
  | (_feat x == "Tense")    && (head (_featValues x) == "Imp")  = "+sbjp" ++ (getFeatValues xs)
  | (_feat x == "Tense")    && (head (_featValues x) == "Pres") = "+sbjr" ++ (getFeatValues xs)
  | otherwise = getFeatValues (x:xs)
subFeat [] = ""

getFeatValues :: [Feat] -> String
getFeatValues (x:xs)
  | _feat (last (x:xs)) == "Gender" = "+" ++ [toLower (head (head (_featValues $ last (x:xs))))] ++ (getFeatValues $ init (x:xs))
  | _feat x  == "Gender" = "+" ++ [toLower (head (head (_featValues x)))] ++ (getFeatValues xs)
  | (_feat x == "Number") && (head ( _featValues x) == "Plur") = "+pl" ++ (getFeatValues xs)
  | (_feat x == "Number") && (head (_featValues x)  == "Sing") = "+sg" ++ (getFeatValues xs)
  | _feat x  == "Person" =  "+" ++ (head (_featValues x)) ++ (getFeatValues xs)
  | (_feat x == "Polarity") && (head (_featValues x) == "Neg") = "+neg"
  | otherwise = getFeatValues xs
getFeatValues [] = ""

getUpos :: String -> U.POS -> [Feat] -> String
getUpos lemma c (x:xs)
  | (c == U.VERB) && (head (_featValues x) == "Ind") = lemma ++ "+v" ++ (indFeat $ reverse xs)
  | (c == U.VERB) && (head (_featValues x) == "Cnd") = lemma ++ "+v+cond" ++ (getFeatValues $ tail xs)
  | (c == U.VERB) && (head (_featValues x) == "Sub") = lemma ++ "+v" ++ (subFeat $ reverse xs)
  | (c == U.VERB) && (head (_featValues x) == "Ger") = lemma ++ "+v+grd"
  | (c == U.VERB) && (head (_featValues $ last (x:xs)) == "Inf") = lemma ++ "+v+inf" ++ (getFeatValues $ reverse (x:xs)) 
  | (c == U.VERB) && (head (_featValues $ last (x:xs)) == "Part") = lemma ++ "+v+ptpass" ++ (getFeatValues $ reverse $ init (x:xs))
  | (c == U.VERB) && (head (_featValues $ last xs)     == "Pass") = lemma ++ "+v+ptpass" ++ (getFeatValues (x:xs))
  | (c == U.AUX)  && (head (_featValues x) == "Ind") = lemma ++ "+v" ++ (indFeat $ reverse xs)
  | (c == U.AUX)  && (head (_featValues x) == "Cnd") = lemma ++ "+v+cond" ++ (getFeatValues $ tail xs)
  | (c == U.AUX)  && (head (_featValues x) == "Sub") = lemma ++ "+v" ++ (subFeat $ reverse xs)
  | (c == U.AUX)  && (head (_featValues x) == "Ger") = lemma ++ "+v+grd"
  | (c == U.AUX)  && (head (_featValues $ last (x:xs)) == "Inf") = lemma ++ "+v+inf" ++ (getFeatValues $ reverse (x:xs)) 
  | (c == U.AUX)  && (head (_featValues $ last (x:xs)) == "Part") = lemma ++ "+v+ptpass" ++ (getFeatValues $ reverse $ init (x:xs))
  | (c == U.AUX)  && (head (_featValues $ last xs)     == "Pass") = lemma ++ "+v+ptpass" ++ (getFeatValues (x:xs))
  | c == U.ADJ  = lemma ++ "+a"   ++ (getFeatValues (x:xs))
  | c == U.NOUN = lemma ++ "+n"   ++ (getFeatValues (x:xs))
  | c == U.ADV  = lemma ++ "+adv" ++ (getFeatValues (x:xs))
  | otherwise = ""
getUpos lemma c []
  | c == U.ADV  = lemma ++ "+adv"
  | c == U.NOUN = lemma ++ "+n"
  | otherwise = ""

-- verifica se a classificação do conllu existe no MorphoBr e retorna um erro
-- caso não exista
-- forma flexionada -> classificação adaptada do conllu -> classificações do MorphoBr
getError :: String -> String -> [String] -> String
getError word cl m
  | member cl (sort m) = ""
  | otherwise = " | " ++ word ++ " : " ++ cl ++" : " ++ (intercalate ", " m)

comp :: T.Trie [String] -> CW AW -> String
comp trie word = getError w (getUpos lemma upos feat) morpho
 where
  w      = fromJust $ _form word
  lemma  = fromJust $ _lemma word
  upos   = fromJust $ _upos word
  feat   = _feats word
  morpho = search $ T.lookup (M.packStr $ map toLower ( fromJust $_form word)) trie

findSent :: [Comment] -> String
findSent (x:xs) 
 | fst x == "text " = snd x
 | otherwise = findSent xs
findSent [] = "sentence not found"

addSent :: Sent -> String -> String
addSent sent s 
 | s == "" = ""
 | otherwise = (findSent (_meta sent)) ++ s ++ "\n"

-- se a palavra for um verbo, nome, adjetivo ou advérbio, chamamos a função 
-- que verifica se a classificação existe 
checkCl :: T.Trie [String] -> Sent -> String
checkCl trie sent  = addSent sent (concatMap aux (_words sent))
 where
   aux word
    | isNothing(_upos word) = ""
    | (fromJust $ _upos word) == U.VERB  = comp trie word
    | (fromJust $ _upos word) == U.AUX   = comp trie word
    | (fromJust $ _upos word) == U.NOUN  = comp trie word
    | (fromJust $ _upos word) == U.ADJ   = comp trie word
    | (fromJust $ _upos word) == U.ADV   = comp trie word
    | otherwise = ""

newCheck :: [FilePath] -> IO ()
newCheck (x:y:xs) = do
  trie <- M.readJSON y
  mapM_ (aux (T.fromList $ M.getList trie)) xs
 where aux t clpath = do
        cl <- readConlluFile clpath
        appendFile x ("Processing " ++ clpath ++ " \n" ++ (concatMap (checkCl t) cl) ++ "\n")


-- main interface

help = putStrLn "Usage: \n\
                \ test-mini -t [classes de palavras, onde escrever] :: [filePath] \n\
                \ test-mini -m [outPath, jsonPath, conlluPaths]\n\
                \ test-mini -c [outPath, jsonPath, conlluPAths]\n"

parse ["-h"]    = help >> exitSuccess
parse ("-t":ls) = M.createTrieList ls >> exitSuccess
parse ("-m":ls) = merge ls >> exitSuccess
parse ("-c":ls) = newCheck ls >> exitSuccess

parse ls        = help >> exitFailure


main :: IO ()
main = getArgs >>= parse
