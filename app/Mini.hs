
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

-- Merge em apenas um arquivo -o

search :: Maybe [String] -> String
search ls  = if isNothing ls 
              then "not found"
              else intercalate "|"  $ fromJust ls


addClass :: T.Trie [String] -> CW AW -> CW AW
addClass trie word = word{_deps = [Rel{_head = SID 1 
                                      ,_deprel = D.ACL
                                      ,_subdep = Just (aux word)
                                      ,_rest= Just [""]}]}
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

merge :: [FilePath] -> IO()
merge [clpath,jspath,outpath] = do 
  cl <- readConlluFile clpath
  tl <- M.readJSON jspath
  writeConlluFile outpath $ addMorphoInfo (T.fromList $ M.getList tl) cl 

-- merge em vários arquivos 

createFilePath :: FilePath -> FilePath -> FilePath 
createFilePath directory cl = addExtension (combine directory (takeBaseName cl)) "-n.conllu"

merges :: [FilePath] -> IO [()]
merges (x:y:xs) = do 
  tl  <- M.readJSON y
  mapM (aux x tl) xs
 where aux directory tl clpath = do
        cl <- readConlluFile clpath
        writeConlluFile (createFilePath directory clpath) $ addMorphoInfo (T.fromList $ M.getList tl) cl


-- main interface

help = putStrLn "Usage: \n\
                \ test-mini -c [classes de palavras, onde escrever] :: [filePath] \n\
                \ test-mini -o [conlluPath, jsonPath, outPath] \n"

parse ["-h"]    = help >> exitSuccess
parse ("-c":ls) = M.createTrieList ls >> exitSuccess
parse ("-o":ls) = merge ls >> exitSuccess
parse ("-m":ls) = merges ls >> exitSuccess
parse ls        = help >> exitFailure

    
main :: IO ()
main = getArgs >>= parse
