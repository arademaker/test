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

isMatch :: Mentions -> Entity -> Bool
isMatch m e = (begin m) == $ head (location (mentions e)) and 
              (end m) == $ last (location (mentions e)) and
              (type m) == (type e)

isLarger :: Mentions -> Entity -> Bool
isLarger m e = (begin m) > $ head (location (mentions e))

isSmaller :: Mentions -> Entity -> Bool
isSmaller m e = (begin m) < $ head (location (mentions e))

 
-- Sugestão retornar uma lista de listas [[Match], [Failure], [Mismatch]]
-- Receive [Mentions of WKS] [Entity of NLU] and return a list of diffs
-- catchDiffs :: [Mentions] -> [Entity] -> [Either Mentions Entity]
-- catchDiffs (x:xs) (y:ys)
--     | isMatch x y = catchDiffs xs ys
--     | isMaior x y = Right y : catchDiffs (x:xs) ys
--     | isMenor x y = Left x : catchDiffs xs (y:ys)
--     | otherwise 

listMatch :: [Mentions] -> [Entity] -> [Entity]
listMatch (x:xs) (y:ys)
    | isMatch x y   = y : listMatch xs ys
    | isLarger x y  = listMatch (x:xs) ys
    | isSmaller x y = listMatch xs (y:ys)

listFailure :: [Mentions] -> [Entity] -> [Either Mentions Entity]


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
