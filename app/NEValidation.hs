
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


-- receive json-WKS json-NLU and check if the texts are the same
checkTexts :: W.Document -> N.Document -> Bool
checkTexts x y = W.docText x == N.analyzed_text y

wksAnn :: W.Document -> [Annotation]
wksAnn doc = map aux (W.mentions doc)
  where
    aux m =
      Annotation
        { anType = [W.menType m]
        , anBegin = W.menBegin m
        , anEnd = W.menEnd m
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



-- Receive files WKS and NLU and return a list of diffs
validation :: Either String N.Document -> Either String W.Document -> [Annotation]
validation (Right docNLU) (Right docWKS) = if checkTexts docWKS docNLU then annNluWks aN aW else []
    where
        aN = sortOn anBegin (nluAnn docNLU)
        aW = sortOn anBegin (wksAnn docWKS)
    

main :: IO ()
main = putStrLn "OK"

{-

saida são linhas com:

begin,end,type,-,source,-
begin,end,-,type,-,source
begin,end,type1,type2,source1,source2
begin,end,type1,type1,source1,source2

-}
