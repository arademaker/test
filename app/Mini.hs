
module Mini where

import Data.Char (toLower)
import Data.Maybe ( catMaybes, isNothing, mapMaybe, fromJust )
import Data.Either ( lefts, rights )
import Data.List 
import Control.Applicative ( Applicative(liftA2) )
import System.Environment ( getArgs ) 
import System.Exit ( exitFailure, exitSuccess )
import System.FilePath.Posix
import Conllu.IO ( readConlluFile, writeConlluFile )
import Conllu.Type
import JsonConlluTools
import qualified Data.Trie as T
import qualified MorphoBr as M
import qualified Conllu.UposTagset as U
import qualified Conllu.DeprelTagset as D

-- map _word sents
-- colocar as classificaçoes no deps
-- pegar a palavra no form
-- comparar com feats 

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

-- [outpath, jsonPath, conllu files ]
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

getFeatValues :: [Feat] -> String
getFeatValues (x:xs) 
  | _feat x == "Gender" = "+" ++ [toLower (head (head (_featValues x)))] ++ (getFeatValues xs)
  | (_feat x == "Number") && (head ( _featValues x) == "Plur") = "+pl" ++ (getFeatValues xs)
  | (_feat x == "Number") && (head (_featValues x) == "Sing") = "+sg" ++ (getFeatValues xs)
  | _feat x == "Person" =  "+" ++ (head (_featValues x)) ++ (getFeatValues xs)
  | (_feat x == "Tense") && (head (_featValues x) == "Fut") = "+fut" ++ (getFeatValues xs)
  | (_feat x == "Tense") && (head (_featValues x) == "Past") = "+prf" ++ (getFeatValues xs)
  | (_feat x == "Tense") && (head (_featValues x) == "Pres") = "+prs" ++ (getFeatValues xs)
  | (_feat x == "Voice") && (head (_featValues x) == "Pass") = "+ptpass" ++ (getFeatValues xs)
  | otherwise = getFeatValues xs
getFeatValues [] = ""

getUpos :: String -> U.POS -> [Feat] -> String
getUpos lemma c (x:xs)
  | (c == U.VERB) && (head (_featValues x) == "Ind") = lemma ++ "+v" ++ (getFeatValues $ reverse xs)
  | (c == U.VERB) && (head (_featValues x) == "Cnd") = lemma ++ "+v+cond" ++ (getFeatValues $ reverse $ tail xs)
  | (c == U.VERB) && (head (_featValues x) == "Sub") = lemma ++ "+v+sbjr" ++ (getFeatValues $ reverse $ tail xs)
  | (c == U.VERB) && (head (_featValues x) == "Ger") = lemma ++ "+v+grd"
  | c == U.ADJ = lemma ++ "+a" ++ (getFeatValues (x:xs))
  | c == U.NOUN = lemma ++ "+n" ++ (getFeatValues (x:xs))
  | c == U.ADV = lemma ++ "+adv"
  | otherwise = ""


-- verifica se a classificação do conllu existe no MorphoBr e retorna um erro
-- caso não exista
-- forma flexionada -> classificação adaptada do conllu -> classificações do MorphoBr
getError :: String -> String -> [String] -> String
getError word cl m 
  | member cl (sort m) = ""
  | otherwise = " error on " ++ cl ++ " " ++ (head m) ++ " \n"

comp :: CW AW -> String
comp word = getError w (getUpos lemma upos feat) morpho
 where
  w = fromJust $ _form word
  lemma = fromJust $ _lemma word
  upos = fromJust $ _upos word
  feat = _feats word
  morpho = fromJust $ _rest $ head $ _deps word


-- se a palavra for um verbo, nome, adjetivo ou advérbio, chamamos a função 
-- que verifica se a classificação existe 
checkCl :: Sent -> String
checkCl sent  = concatMap aux (_words sent)
 where 
   aux word
    | isNothing(_upos word) = ""
    | (fromJust $ _upos word) == U.VERB  = comp word
    | (fromJust $ _upos word) == U.NOUN  = comp word
    | (fromJust $ _upos word) == U.ADJ   = comp word
    | (fromJust $ _upos word) == U.ADV   = comp word
    | otherwise = ""

check :: [FilePath] -> IO [()]
check = mapM aux 
 where 
   aux clpath = do 
    cl <- readConlluFile clpath 
    print $ concatMap checkCl cl 


-- main interface

help = putStrLn "Usage: \n\
                \ test-mini -t [classes de palavras, onde escrever] :: [filePath] \n\
                \ test-mini -m [outPath, jsonPath, conlluPaths]\n\
                \ test-mini -c [conlluPAths]\n"

parse ["-h"]    = help >> exitSuccess
parse ("-t":ls) = M.createTrieList ls >> exitSuccess
parse ("-m":ls) = merge ls >> exitSuccess
parse ("-c":ls) = check ls >> exitSuccess

parse ls        = help >> exitFailure

    
main :: IO ()
main = getArgs >>= parse
