{-# LANGUAGE OverloadedStrings #-}

module Main2 where

import Data.Text (Text, pack)
import qualified Data.Text.IO as Txt
import NLP.Punkt (split_sentences)

corpus :: Text
corpus = "«Ellis O. Briggs» nasceu em Watertown, Massachusetts, EUA, no dia 1º de dezembro de 1899, filho de James Briggs e de Lucy Hill Briggs. Agora só falta você."

main :: IO ()
main = mapM_ Txt.putStrLn (split_sentences corpus)

{-

1. ler https://www.aclweb.org/anthology/J06-4003.pdf (sobre Punkt)

2. adaptar codigo para ler um arquivo TXT (raw), chamar
   split_sentences -> produzir um TXT (offset, dois numeros por linha)

3. comparar os offset produzidos na etapa 2 (acima) com os offsets
   produzidos pelo nltk ???

4. incoporar este codigo no fluxo de segmentacao !!!

-}
