module NEValidation where

import WKS
import NLU
import Data.Aeson
import System.Exit
import System.Environment
import Data.Either
import Data.List
import Data.Ord (comparing)



-- Receive json-WKS json-NLU and check if the texts are the same
checkTexts :: Either String WKS.Document -> Either String NLU.Document -> Bool
checkTexts jsonWKS jsonNLU = verify jsonWKS jsonNLU
    where
      verify (Right x) (Right y) = docText x == analyzed_text y
      verify _ _ = False

isMatch :: Mentions -> Entity -> Bool
isMatch m e = (menBegin m) == (head (location (head (mentions e)))) && 
              (menEnd m) == (last (location (head (mentions e)))) &&
              (menType m) == (etype e)

isLarger :: Mentions -> Entity -> Bool
isLarger m e = (menBegin m) > head (location (head (mentions e)))

isSmaller :: Mentions -> Entity -> Bool
isSmaller m e = (menBegin m) < head (location (head (mentions e)))

isMismatch :: Mentions -> Entity -> Bool
isMismatch m e = (menBegin m) == (head (location (head (mentions e)))) && 
                 (menEnd m) == (last (location (head (mentions e))))


-- Receive [Mentions of WKS] [Entity of NLU] and return a list of diffs
catchDiffs :: [Mentions] -> [Entity] -> [[Either Mentions Entity]] -> [[Either Mentions Entity]]
catchDiffs (x:xs) (y:ys) (ma:fa:mi:_)
    | null (x:xs)    = [reverse ma, (reverse fa) ++ (map (\n -> Right n) (y:ys)), reverse mi]
    | null (y:ys)    = [reverse ma, (reverse fa) ++ (map (\n -> Left n) (x:xs)), reverse mi]
    | isMatch x y    = catchDiffs xs ys [Right y:ma,fa,mi]
    | isMismatch x y = catchDiffs xs ys [ma,fa,Left x:Right y:mi]
    | isLarger x y   = catchDiffs (x:xs) ys [ma,Right y:fa,mi]
    | isSmaller x y  = catchDiffs xs (y:ys) [ma,Left x:fa,mi]
    | otherwise      = [ma, fa, mi]

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
validation :: Either String NLU.Document -> Either String WKS.Document -> [[Either Mentions Entity]]
validation (Right jsonNLU) (Right jsonWKS) = catchDiffs men ent [[], [], []]
    where
        men = sortWKS (mentionsWKS  jsonWKS)
        ent = sortNLU (entities jsonNLU)



-- Receive files, check texts and apply validation
reading :: [FilePath] -> IO Bool
reading [wksPath, nluPath] =  do 
    nlu <- readJSON nluPath
    wks <- readGJson wksPath
    if checkTexts wks nlu then return (null $ validation nlu wks) 
        else return False
    

main :: IO ()
main = putStrLn "OK"
