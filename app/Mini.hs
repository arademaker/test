
module Mini where


import Data.Maybe ( catMaybes, isNothing, mapMaybe )
import Data.Either ( lefts, rights )
import Control.Applicative ( Applicative(liftA2) )
import System.Environment ( getArgs ) 
import System.Exit ( exitFailure, exitSuccess )
import Conllu.IO ( readConlluFile, writeConlluFile )
import Conllu.Type
import NLU
import JsonConlluTools
import MorphoBr

-- main interface

check (directory:"-d":xs) = []  -- para cada x no xs ler entradas em uma estrutura de dados

msg =
  " Usage: \n\
  \  test-conllu -m JSON-file CONLLU-file CONLLU-file\n\
  \  test-conllu -c CONLLU-file  => NER and POS check to stdout\n"

help = putStrLn msg

parse ["-h"]    = help >> exitSuccess
-- parse ("-c":ls) = check ls >> exitSuccess
parse ls        = help >> exitFailure
    
main :: IO ()
main = getArgs >>= parse
