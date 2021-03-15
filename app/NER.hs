
module NER where

import qualified WKS as W
import qualified NLU as N
import Data.Aeson
import System.Exit
import System.Environment
import Data.Either
import Data.Maybe
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
        , anEnd = N.location men !! 1
        , anSource = ["NLU"]
        }


merge1 :: Annotation -> Annotation -> Maybe Annotation
merge1 a b =
  if anBegin a == anBegin b && anEnd a == anEnd b
    then Just
           a
             { anType = anType a ++ anType b
             , anSource = anSource a ++ anSource b
             }
    else Nothing
              

-- receive annotations list of annotations from the WKS GT and NLU
-- return the consolidation of the annotations.

merge :: [Annotation] -> [Annotation] -> [Annotation]
merge [] ys = ys
merge xs [] = xs
merge x'@(x:xs) y'@(y:ys)
  | anBegin x < anBegin y = x : merge xs y'
  | anBegin x > anBegin y = y : merge x' ys
  | isJust res = fromJust res : merge xs ys
  | otherwise = x : y : merge xs ys
  where
    res = merge1 x y

    
-- Receive files WKS and NLU and return a list of diffs. The return
-- Nothing means that either the inputs are Left (error in the parser)
-- or the texts are different. Note that the return can also be Just
-- [], meaning that no annotation were available.

validation :: Either String N.Document -> Either String W.Document -> Maybe [Annotation]
validation (Right docNLU) (Right docWKS) =
  if checkTexts docWKS docNLU
    then Just (merge aN aW)
    else Nothing
  where
    aN = sortOn anBegin (nluAnn docNLU)
    aW = sortOn anBegin (wksAnn docWKS)
validation _ _ = Nothing


createCSV :: Either String N.Document -> Either String W.Document -> String -> IO ()
createCSV nluJson wksJson name = writeFile (name ++ ".csv") $ concatMap aux $ fromJust $ validation nluJson wksJson 
  where
    aux a 
      | (anSource a) !! 0 == "WKS" = (show (anBegin a)) 
                                  ++ "," 
                                  ++ (show (anEnd a)) 
                                  ++ ",-," 
                                  ++ (head (anType a)) 
                                  ++ ",-," 
                                  ++ (last (anSource a)) 
                                  ++ "\n"  
      | last (anSource a) == "NLU" = (show (anBegin a)) 
                                  ++ "," 
                                  ++ (show (anEnd a)) 
                                  ++ "," 
                                  ++ (head (anType a)) 
                                  ++ ",-," 
                                  ++ (last (anSource a)) 
                                  ++ ",-\n"
      | otherwise                  = (show (anBegin a)) 
                                  ++ "," 
                                  ++ (show (anEnd a)) 
                                  ++ "," 
                                  ++ (head (anType a)) 
                                  ++ "," 
                                  ++ (last (anType a)) 
                                  ++ "," 
                                  ++ (head (anSource a)) 
                                  ++ "," 
                                  ++ (last (anSource a)) 
                                  ++ "\n"


main :: IO ()
main = putStrLn "OK"

{-

saida são linhas com:

(1) begin,end,type,-,source,-
(2) begin,end,-,type,-,source
(3) begin,end,type1,type2,source1,source2
(4) begin,end,type1,type1,source1,source2 <- equivalente a (3)

-}
