
module NEValidation where

import qualified WKS as W
import qualified NLU as N
import Data.Aeson
import System.Exit
import System.Environment
import Data.Either
import Data.List
import Data.Ord (comparing)


data Annotation =
  Annotation
    { anType :: String
    , anBegin :: Int
    , anEnd :: Int
    , anSource :: String
    }
  deriving (Eq, Show)


-- receive json-WKS json-NLU and check if the texts are the same
checkTexts :: Either String W.Document -> Either String N.Document -> Bool
checkTexts
  | (Right x) (Right y) = W.docText x == N.analyzed_text y
  | _ _ = False


-- -- Receive a mention and an entity and check if the begins, the ends 
-- -- and the types are the same (match)
-- isMatch :: Mentions -> Entity -> Bool
-- isMatch m e = menBegin m == head (location (head (mentions e))) && 
--               menEnd m   == last (location (head (mentions e))) &&
--               menType m  == etype e

-- -- Receive a mention and an entity and check if the begin of the mention
-- -- is larger than the begin of the entity (failure)
-- isLarger :: Mentions -> Entity -> Bool
-- isLarger m e = menBegin m > head (location (head (mentions e)))

-- -- Receive a mention and an entity and check if the begin of the mention
-- -- is smaller than the begin of the entity (failure)
-- isSmaller :: Mentions -> Entity -> Bool
-- isSmaller m e = menBegin m < head (location (head (mentions e)))

-- -- Receive a mention and an entity and check if the begins and the ends
-- -- are the same (mismatch)
-- isMismatch :: Mentions -> Entity -> Bool
-- isMismatch m e = menBegin m == head (location (head (mentions e))) && 
--                  menEnd m   == last (location (head (mentions e)))

-- -- Receive [Mentions of WKS] [Entity of NLU] and return a list of diffs
-- catchDiffs :: [Mentions] -> [Entity] -> [[Either Mentions Entity]] -> [[Either Mentions Entity]]
-- catchDiffs (x:xs) (y:ys) (ma:fa:mi:_)
--     | null (x:xs)    = [reverse ma, reverse fa ++ map Right (y:ys), reverse mi]
--     | null (y:ys)    = [reverse ma, reverse fa ++ map Left  (x:xs), reverse mi]
--     | isMatch x y    = catchDiffs xs ys [Right y:ma,fa,mi]
--     | isMismatch x y = catchDiffs xs ys [ma,fa,Left x:Right y:mi]
--     | isLarger x y   = catchDiffs (x:xs) ys [ma,Right y:fa,mi]
--     | isSmaller x y  = catchDiffs xs (y:ys) [ma,Left x:fa,mi]
--     | otherwise      = [ma, fa, mi]


-- -- Receive files WKS and NLU and return a list of diffs
-- validation :: Either String NLU.Document -> Either String WKS.Document -> [[Either Mentions Entity]]
-- validation (Right jsonNLU) (Right jsonWKS) = catchDiffs men ent [[], [], []]
--     where
--         menWKS = sortOn menBegin (W.mentions jsonWKS)
--         menNLU = sortOn (head . location) $ concatMap N.mentions (entities jsonNLU)

wksAnn :: W.Document -> [Annotation]
wksAnn doc = map aux (W.mentions doc)
  where
    aux m =
      Annotation
        { anType = W.menType m
        , anBegin = W.menBegin m
        , anEnd = W.menEnd m
        , anSource = "WKS"
        } 
            
nluAnn :: N.Document -> [Annotation]
nluAnn doc = concatMap aux1 (N.entities doc)
  where
    aux1 ent = map (aux2 ent) (N.mentions ent)
    aux2 ent men =
      Annotation
        { anType = N.etype ent
        , anBegin = head $ N.location men
        , anEnd = (N.location men) !! 1
        , anSource = "NLU"
        } 
      

-- -- Receive files, check texts and apply validation
-- reading :: [FilePath] -> IO Bool
-- reading [wksPath, nluPath] = do
--   nlu <- N.readJSON nluPath
--   wks <- W.readJSON wksPath
--   if checkTexts wks nlu
--     then return (null $ validation nlu wks)
--     else return False
    

main :: IO ()
main = putStrLn "OK"

{-

saida são linhas com:

begin,end,type,-,source,-
begin,end,-,type,-,source
begin,end,type1,type2,source1,source2
begin,end,type1,type1,source1,source2

-}
