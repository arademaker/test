module NEValidation where

import WKS
import NLU
import Data.Aeson
import System.Exit
import System.Environment
import Data.Either


-- Receive [json-WKS, json-NLU] and check if the texts are the same
checkTexts :: Either String WKS.Document -> Either String NLU.Document -> Bool
checkTexts jsonWKS jsonNLU = verify jsonNLU jsonWKS
    where
      verify (Right x) (Right y) = analyzed_text x == docText y
      verify _ _ = False


catchDiffs :: (WKS.Document -> [Mentions]) -> (NLU.Document -> [Entity]) -> IO Bool
catchDiffs menNLU menWKS = return True 

getMentions :: (NLU.Document -> [Entity]) -> [Mentions]


validation :: Either String NLU.Document -> Either String WKS.Document -> IO Bool
validation (Right jsonNLU) (Right jsonWKS) = do
    mentionsNLU <- getMentions entities
    return catchDiffs mentionsWKS entities 


reading :: [FilePath] -> IO Bool
reading [wksPath, nluPath] =  do 
    nlu <- readJSON nluPath
    wks <- readGJson wksPath
    if checkTexts wks nlu then validation nlu wks 
        else return False
    


-- future main:

-- msg =
--   " Checking for differences between WKS and NLU. \n\
--   \ Usage: \n\
--   \     NEValidation -t [json-WKS, json-NLU] (check if the texts are the same)"

-- help = putStrLn msg

-- parse ["-h"]       = help >> exitSuccess
-- parse ("-t":ls)    = checkTexts ls >> exitSuccess
-- parse ls           = help >> exitFailure

-- main :: IO ()
-- main = getArgs >>= parse

main :: IO ()
main = putStrLn "OK"



{-
wks <- readGJson "/home/ana/dhbb/dhbb-nlp/ner/wks/gt/59612d70-d9aa-11ea-bff2-05ea84f8fa50-40.json"
js  <- readJSON "/home/ana/dhbb/dhbb-ner/JSON/11576.json"
-}
