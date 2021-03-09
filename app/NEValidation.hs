module NEValidation where

import WKS
import NLU
import Data.Aeson
import System.Exit
import System.Environment
import Data.Either
import Data.List (sortOn)
import Data.Ord (comparing)



-- Receive json-WKS json-NLU and check if the texts are the same
checkTexts :: Either String WKS.Document -> Either String NLU.Document -> Bool
checkTexts jsonWKS jsonNLU = verify jsonNLU jsonWKS
    where
      verify (Right x) (Right y) = analyzed_text x == docText y
      verify _ _ = False


-- Receive [Mentions of WKS] [Entity of NLU] and return a list of diffs
catchDiffs :: [Mentions] -> [Entity] -> [Either Mentions Entity]
catchDiffs (x:xs) (y:ys)
    | x == y    = catchDiffs xs ys
    | x > y     = Right y : catchDiffs (x:xs) ys
    | otherwise = Left x : catchDiffs xs (y:ys)


-- Receive [Mentions of WKS] and sort this list
sortWKS :: [Mentions] -> [Mentions]
sortWKS  = sortOn menBegin

-- Receive [Entity of NLU] and sort this list
sortNLU :: [Entity] -> [Entity]
sortNLU  = sortOn (head . location . head . mentions)


-- Receive files WKS and NLU and return a list of diffs
validation :: Either String NLU.Document -> Either String WKS.Document -> IO [Either Mentions Entity]
validation (Right jsonNLU) (Right jsonWKS) = catchDiffs menWKS ent 
    where
        menWKS = sortWKS (mentionsWKS  jsonWKS)
        ent    = sortNLU (entities jsonNLU)



-- Receive files, check texts and apply validation
reading :: [FilePath] -> IO Bool
reading [wksPath, nluPath] =  do 
    nlu <- readJSON nluPath
    wks <- readGJson wksPath
    if checkTexts wks nlu then validation nlu wks 
        else return False
    

main :: IO ()
main = putStrLn "OK"
