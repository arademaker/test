
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
    { anType :: [String]
    , anBegin :: Int
    , anEnd :: Int
    , anSource :: [String]
    }
  deriving (Eq, Show)


-- -- receive json-WKS json-NLU and check if the texts are the same
-- checkTexts :: Either String W.Document -> Either String N.Document -> Bool
-- checkTexts
--   | (Right x) (Right y) = W.docText x == N.analyzed_text y
--   | _ _ = False


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


-- Receive files WKS and NLU and return a list of diffs
validation :: Either String N.Document -> Either String W.Document -> [Annotation]
validation (Right docNLU) (Right docWKS) = annNluWks aN aW
    where
        aN = sortOn anBegin (nluAnn docNLU)
        aW = sortOn anBegin (wksAnn docWKS)

wksAnn :: W.Document -> [Annotation]
wksAnn doc = map aux (W.mentions doc)
  where
    aux m =
      Annotation
        { anType = [W.menType m]
        , anBegin = W.menBegin m - 1
        , anEnd = W.menEnd m - 1
        , anSource = ["WKS"]
        } 
            
nluAnn :: N.Document -> [Annotation]
nluAnn doc = concatMap aux1 (N.entities doc)
  where
    aux1 ent = map (aux2 ent) (N.mentions ent)
    aux2 ent men =
      Annotation
        { anType = [N.etype ent]
        , anBegin = head $ N.location men
        , anEnd = (N.location men) !! 1
        , anSource = ["NLU"]
        }

isMatch :: Annotation -> Annotation -> Bool
isMatch ann1 ann2 = anType ann1 == anType ann2 && 
                    anBegin ann1 == anBegin ann2 && 
                    anEnd ann1 == anEnd ann2

isMismatch :: Annotation -> Annotation -> Bool
isMismatch ann1 ann2 = anBegin ann1 == anBegin ann2 && 
                       anEnd ann1 == anEnd ann2


-- annNluWks :: N.Document -> W.Document -> [Annotation]
-- annNluWks docNlu docWks = 
--   let annNlu = nluAnn docNlu 
--       annWks = wksAnn docWks 
--   in  concatMap aux annNlu 
--         where 
--           aux aN = concatMap aux2 annWks 
--           aux2 aN aW = 
--             |annIsMatch aN aW = [Annotation { anType = [anType aN], anBegin = anBegin aN, anEnd = anEnd aN , anSource = ["NLU", "WKS"]}]    
--             |isMismatch aN aW = [Annotation { anType = [anType aN, anType aW], anBegin = anBegin aN, anEnd = anEnd aN, anSource = ["NLU", "WKS"]}]  
--             |otherwise = [aN, aW]

-- receive annotations list of Nlu and Wks return a Union Annotation
annNluWks :: [Annotation] -> [Annotation] -> [Annotation]
annNluWks (x:xs) (y:ys)
  | null (x:xs) = (y:ys)
  | null (y:ys) = (x:xs)
  | isMatch x y = Annotation 
                    { anType = anType x
                    , anBegin = anBegin x
                    , anEnd = anEnd x
                    , anSource = anSource x ++ anSource y
                    } : annNluWks xs ys
  | isMismatch x y =  Annotation 
                        { anType = anType x ++ anType y
                        , anBegin = anBegin x
                        , anEnd = anEnd x
                        , anSource = anSource x ++ anSource y
                        } : annNluWks xs ys
  | anBegin x > anBegin y = y: annNluWks (x:xs) ys
  | anBegin x < anBegin y = x: annNluWks xs (y:ys)
  | otherwise = []


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
