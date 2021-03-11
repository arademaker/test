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
isMatch m e = (begin m) == (head (location (mentions e))) && 
              (end m) == (last (location (mentions e))) &&
              (type m) == (type e)

isLarger :: Mentions -> Entity -> Bool
isLarger m e = (begin m) > $ head (location (mentions e)) &&

isSmaller :: Mentions -> Entity -> Bool
isSmaller m e = (begin m) < $ head (location (mentions e))

isMismatch :: Mentions -> Entity -> Bool
isMismatch m e = (begin m) == (head (location (mentions e))) && 
                 (end m) == (last (location (mentions e)))


-- Receive [Mentions of WKS] [Entity of NLU] and return a list of diffs
catchDiffs :: [Mentions] -> [Entity] -> [[Either Mentions Entity]] -> IO [[Either Mentions Entity]]
catchDiffs (x:xs) (y:ys) (ma:fa:mi:_)
    | isNull x:xs    = [reverse ma, (map (\n -> Right n) y:ys):reverse fa, reverse mi]
    | isNull y:ys    = [reverse ma, (map (\n -> Left n) x:xs):reverse fa, reverse mi]
    | isMatch x y    = catchDiffs xs ys [Right y:ma,fa,mi]
    | isMismatch x y = catchDiffs xs ys [ma,fa,Left x:Right y:mi]
    | isLarger x y   = catchDiffs (x:xs) ys [ma,Right y:fa,mi]
    | isSmaller x y  = catchDiffs xs (y:ys) [ma,Left x:fa,mi]
    | otherwise      = do
        putStrLn "Error in catchDiffs"
        return [ma:fa:mi]

-- listMatch :: [Mentions] -> [Entity] -> [Entity]
-- listMatch (x:xs) (y:ys)
--     | isMatch x y   = y : listMatch xs ys
--     | isLarger x y  = listMatch (x:xs) ys
--     | isSmaller x y = listMatch xs (y:ys)

-- listFailure :: [Mentions] -> [Entity] -> [Either Mentions Entity]


-- Receive [Mentions of WKS] and sort this list
sortWKS :: [Mentions] -> [Mentions]
sortWKS  = sortOn menBegin

-- Receive [Entity of NLU] and sort this list
sortNLU :: [Entity] -> [Entity]
sortNLU  = sortOn (head . location . head . mentions)


-- Receive files WKS and NLU and return a list of diffs
validation :: Either String NLU.Document -> Either String WKS.Document -> IO [Either Mentions Entity]
validation (Right jsonNLU) (Right jsonWKS) = catchDiffs men ent ([], [], [])
    where
        men = sortWKS (mentionsWKS  jsonWKS)
        ent = sortNLU (entities jsonNLU)



-- Receive files, check texts and apply validation
reading :: [FilePath] -> IO Bool
reading [wksPath, nluPath] =  do 
    nlu <- readJSON nluPath
    wks <- readGJson wksPath
    if checkTexts wks nlu then validation nlu wks 
        else return False
    

main :: IO ()
main = putStrLn "OK"
