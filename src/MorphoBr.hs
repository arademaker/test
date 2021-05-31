
{-# LANGUAGE OverloadedStrings #-}

module MorphoBr where

import Data.Either
import System.IO
import qualified Data.Map as M
import System.Directory
import System.FilePath.Posix
import qualified Data.Text as T
import qualified Data.Text.IO as TO
import Data.List (groupBy, intercalate, sort)
import Data.Maybe ( fromJust, isNothing )


-- Na simplificação, para cada chave, os valores são ordenados e agrupados de acordo com o lema,
-- em seguida é feita a interseção dois a dois de cada grupo 

member :: (Eq a) => a -> [a] -> Bool
member x [] = False
member x (y:ys) | x==y = True
                | otherwise = member x ys

lines2pairs :: [T.Text] -> [(T.Text, [T.Text])]
lines2pairs =
  map (\s -> let p = T.splitOn "\t" s in (head p, tail p))

inter :: [T.Text] -> [T.Text] -> [T.Text]
inter (x:xs) (y:ys)
 | x == y = x : inter xs ys
 | otherwise = inter xs ys
inter [] (y:ys) = []
inter (x:xs) [] = []
inter [] [] = []

simplify :: [T.Text] -> [T.Text]
simplify ms =
  map aux $ groupBy (\a b -> (head a) == (head b)) (map (T.splitOn (T.pack "+")) (sort ms))
   where
     aux ls = T.intercalate "+" $ foldl1 inter ls

readF1 :: FilePath -> IO (M.Map T.Text [T.Text])
readF1 fn = do
  content <- TO.readFile fn
  return $ M.fromListWith (++) $ lines2pairs (T.lines content)

createSimpMap :: FilePath -> [FilePath] -> IO (M.Map T.Text [T.Text])
createSimpMap dir paths = do
  dicts <- mapM (readF1 . combine dir) paths
  return (M.map simplify $ foldr M.union M.empty dicts)

clean :: [T.Text] -> [T.Text]
clean (x:xs)
 | x == "" = clean xs
 | otherwise = x : clean xs
clean [] = []

---- Comparação de resultados

createMap :: FilePath -> [FilePath] -> IO (M.Map T.Text [T.Text])
createMap dir paths = do
  dicts <- mapM (readF1 . combine dir) paths
  return (foldr M.union M.empty dicts)

check :: M.Map T.Text [T.Text] -> [T.Text] -> T.Text
check m xs
 | member (last xs) (fromJust (M.lookup (head xs) m)) = ""
 | otherwise = T.append (last xs) $ T.append " | " (T.intercalate " " (fromJust (M.lookup (head xs) m)))

-- recebe o diretório noun ou o diretório adjectives do MorphoBr e um diretório contendo os
-- arquivos produzidos pelo script em python
getDiffs :: FilePath -> FilePath -> IO [[T.Text]]
getDiffs mpath epath = do
  mfiles <- listDirectory mpath
  efiles <- listDirectory epath
  m <- createMap mpath mfiles
  mapM (aux m epath) efiles 
   where
     aux m dir path = do
       content <- TO.readFile $ combine dir path
       return (clean $ map (check m . (T.splitOn "\t")) (T.lines content))

---- Correção de erros

-- lebrão e lebrões foram simplificados mas não deveriam ser
-- arquivo nouns-ac.dict
erro1 :: M.Map T.Text [T.Text] -> M.Map T.Text [T.Text]
erro1 m =
  M.insert (T.pack "lebrões") [(T.pack "lebre+N+AUG+M+PL"), (T.pack "lebre+N+M+PL")] $ 
  M.insert (T.pack "lebrão") [(T.pack "lebre+N+AUG+M+SG"),(T.pack "lebre+N+M+SG")] m

-- a simplificação de "zurupável" estava errada, o motivo desse erro foi a duplicação de 
-- "zurupável	zurupável+A+M+SG" no arquivo adjectives-ae.dict
erro2 :: M.Map T.Text [T.Text] -> M.Map T.Text [T.Text]
erro2 = 
  M.insert (T.pack "zurupável") [T.pack "zurupável+A+SG"]


-- "cimbráveis" não era simplificado porque ao unir os maps de cada arquivo do diretório adjectives
-- o valor da chave "cimbráveis" (cimbrável+A+F+PL), produzido pelo map do arquivo adjectives-aa.dict, 
-- era substituído  pelo valor (cimbrável+A+M+PL) produzido pelo map do arquivo adjectives-ab.dict
erro3 :: M.Map T.Text [T.Text] -> M.Map T.Text [T.Text]
erro3 =
  M.insert (T.pack "cimbráveis") [T.pack "cimbrável+A+PL"]

erro4 :: M.Map T.Text [T.Text] -> M.Map T.Text [T.Text]
erro4  m =
  M.insert (T.pack "vilanaz") [T.pack "vilanaz+A+SG",T.pack "vilão+A+AUG+SG",T.pack "vilão+A+F+SG"] $
  M.insert (T.pack "vilanazes") [T.pack "vilanaz+A+PL",T.pack "vilão+A+AUG+PL",T.pack "vilão+A+F+PL"] m 

erro5 :: M.Map T.Text [T.Text] -> M.Map T.Text [T.Text]
erro5  m =
  M.insert (T.pack "vilanaz") [T.pack "vilão+N+AUG+SG",T.pack "vilão+N+F+SG"] $
  M.insert (T.pack "vilanazes") [T.pack "vilão+N+AUG+PL",T.pack "vilão+N+F+PL"] m 



-- fixN e fixA são usadas para corrigir erros pontuais, isso é feito inserindo as entradas corretas
-- Obs: ao inserir uma nova chave, se já existir uma chave igual no map, os valores da
-- chave antiga serão substituídos pelos valores da chave nova
fixA :: M.Map T.Text [T.Text] -> M.Map T.Text [T.Text]
fixA m = erro2 $ erro3 $ erro4 m 

fixN :: M.Map T.Text [T.Text] -> M.Map T.Text [T.Text]
fixN m = erro5 $ erro1 m


---- Produção de novos arquivos 

serialize :: (T.Text, [T.Text]) -> [T.Text] 
serialize (k,xs) = map (\x -> T.append k (T.append "\t" x)) xs

checkLemma :: Int -> [T.Text] -> ([T.Text],[T.Text])
checkLemma n xs 
 | length xs < n = (xs,[])
 | head(T.splitOn "+" (last (T.splitOn "\t" (xs!!n)))) == 
   head(T.splitOn "+" (last (T.splitOn "\t" (xs!!(n-1))))) = checkLemma (n+1) xs
 | otherwise = splitAt n xs

splitEvery :: Int -> [T.Text] -> [[T.Text]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = checkLemma n list

toText :: (T.Text, [T.Text ]) -> [T.Text]
toText (k,xs) = reverse xs

toPairs :: [T.Text] -> [(T.Text ,[T.Text])]
toPairs =
  map (\s -> let p = T.splitOn "\t" s in (head(T.splitOn "+" (last p)), [s]))

separate :: Int -> [T.Text] -> [[T.Text ]]
separate n xs = splitEvery n $ concatMap toText $ M.toList $ M.fromListWith (++) $ toPairs xs
 


-- newADJ recebe o diretório adjectives do MorphoBr e um diretório
-- onde será salva a versão compacta
newADJ :: FilePath -> FilePath -> IO [()]
newADJ path outdir = do
  files <- listDirectory path
  m <- createSimpMap path files
  mapM (aux outdir) (separate 18560 $ concatMap serialize (M.toList (fixA m)))
   where 
     aux outdir (x:xs) =
       TO.writeFile (combine outdir ("adjectives-"++[T.head x]++[T.head (last xs)])) (T.intercalate "\n" (x:xs)) 

-- newNouns recebe o diretório nouns do MorphoBr e um diretório
-- onde será salva a versão compacta
newNouns :: FilePath -> FilePath -> IO [()]
newNouns path outdir = do
  files <- listDirectory path
  m <- createSimpMap path files
  mapM (aux outdir) (separate 19040 $ concatMap serialize (M.toList (fixN m)))
   where 
     aux outdir (x:xs) =
       TO.writeFile (combine outdir ("nouns-"++[T.head x]++[T.head (last xs)])) (T.intercalate "\n" (x:xs)) 


---- Reconstrução a partir da versão simplificada

getEntries :: [(T.Text,[T.Text])] -> [T.Text]
getEntries (x:xs)
 | member (last $ snd x) [T.pack "A",T.pack "N"] =
   [T.append (fst x) (T.append "\t" (T.intercalate "+" (snd x++[T.pack "F+PL"]))),
    T.append (fst x) (T.append "\t" (T.intercalate "+" (snd x++[T.pack "F+SG"]))),
    T.append (fst x) (T.append "\t" (T.intercalate "+" (snd x++[T.pack "M+PL"]))),
    T.append (fst x) (T.append "\t" (T.intercalate "+" (snd x++[T.pack "M+SG"])))] ++ getEntries xs
 | member (tail $ snd x) [[(T.pack "A"),(T.pack "F")],[(T.pack "A"),(T.pack "M")],
                          [(T.pack "N"),(T.pack "F")],[(T.pack "N"),(T.pack "M")]] = 
    [T.append (fst x) (T.append "\t" (T.intercalate "+" (snd x ++ [T.pack "PL"]))),
     T.append (fst x) (T.append "\t" (T.intercalate "+" (snd x ++ [T.pack "SG"])))] ++ getEntries xs
 | member (tail (snd x)) [[(T.pack "A"),(T.pack "SG")],[(T.pack "A"),(T.pack "PL")],
                          [(T.pack "N"),(T.pack "SG")],[(T.pack "N"),(T.pack "PL")]] =
    [T.append (fst x) (T.append "\t" (T.intercalate "+" (init (snd x) ++ [T.pack "F"] ++ [last (snd x)]))),
     T.append (fst x) (T.append "\t" (T.intercalate "+" (init (snd x) ++ [T.pack "M"] ++ [last (snd x)])))] ++ getEntries xs
 | member (tail (snd x)) [[(T.pack "A"),(T.pack "AUG"),(T.pack "PL")],[(T.pack "A"),(T.pack "AUG"),(T.pack "SG")],
                          [(T.pack "A"),(T.pack "SUPER"),(T.pack "PL")],[(T.pack "A"),(T.pack "SUPER"),(T.pack "SG")],
                          [(T.pack "A"),(T.pack "DIM"),(T.pack "PL")],[(T.pack "A"),(T.pack "DIM"),(T.pack "SG")],
                          [(T.pack "N"),(T.pack "AUG"),(T.pack "PL")],[(T.pack "N"),(T.pack "AUG"),(T.pack "SG")],
                          [(T.pack "N"),(T.pack "SUPER"),(T.pack "PL")],[(T.pack "N"),(T.pack "SUPER"),(T.pack "SG")],
                          [(T.pack "N"),(T.pack "DIM"),(T.pack "PL")],[(T.pack "N"),(T.pack "DIM"),(T.pack "SG")]] =
    [T.append (fst x) (T.append "\t" (T.intercalate "+" (init (snd x) ++ [T.pack "F"] ++ [last (snd x)]))),
     T.append (fst x) (T.append "\t" (T.intercalate "+" (init (snd x) ++ [T.pack "M"] ++ [last (snd x)])))] ++ getEntries xs
 | otherwise = (T.append (fst x) (T.append "\t" (T.intercalate "+" (snd x)))) : getEntries xs
getEntries [] = []


rebuild :: FilePath -> IO (T.Text)
rebuild fn = do
  content <- TO.readFile fn
  return (T.intercalate "\n" (getEntries (map aux (T.lines content))))
   where 
     aux s = do
       let p = T.splitOn "\t" s in (head p, T.splitOn "+" (last p))


expand :: FilePath -> FilePath -> IO ()
expand path outpath = do
  files <- listDirectory path
  dic <- mapM (rebuild . combine path) files
  TO.writeFile outpath (T.intercalate "\n" dic)