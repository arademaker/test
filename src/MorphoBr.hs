
{-# LANGUAGE OverloadedStrings #-}

module MorphoBr where

import Data.Either
import System.IO
import qualified Data.Map as M
import System.Directory
import System.FilePath.Posix
import qualified Data.Text as T
import qualified Data.Text.IO as TO
import Data.List (groupBy, intercalate, sort, nub, sortOn)
import Data.List.Split (splitPlaces, chunksOf)
import Data.Maybe ( fromJust, isNothing )


---- Simplificação

-- Na simplificação, para cada chave, os valores são ordenados e agrupados de acordo com o lema,
-- em seguida é feita a interseção de cada grupo 

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
  map aux $ groupBy (\a b -> (head a) == (head b)) (map (T.splitOn (T.pack "+")) (nub $ sort ms))
   where
     aux ls = T.intercalate "+" $ foldl1 inter ls

readF1 :: FilePath -> IO (M.Map T.Text [T.Text])
readF1 fn = do
  content <- TO.readFile fn
  return $ M.fromListWith (++) $ lines2pairs (T.lines content)

createSimpMap :: FilePath -> [FilePath] -> IO (M.Map T.Text [T.Text])
createSimpMap dir paths = do
  dicts <- mapM (readF1 . combine dir) paths
  return (M.map simplify $ foldr (M.unionWith (++)) M.empty dicts)

clean :: [T.Text] -> [T.Text]
clean (x:xs)
 | x == "" = clean xs
 | otherwise = x : clean xs
clean [] = []

---- Comparação de resultados

createMap :: FilePath -> [FilePath] -> IO (M.Map T.Text [T.Text])
createMap dir paths = do
  dicts <- mapM (readF1 . combine dir) paths
  return (foldr (M.unionWith (++)) M.empty dicts)

check :: M.Map T.Text [T.Text] -> [T.Text] -> T.Text
check m xs
 | member (last xs) (fromJust (M.lookup (head xs) m)) = ""
 | otherwise = T.append (last xs) $ T.append " | " (T.intercalate " " (fromJust (M.lookup (head xs) m)))

-- recebe dois diretórios e compara seus arquivos pesquisando as entradas do segundo no map do primeiro 
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

-- fixN e fixA são usadas para corrigir erros pontuais, isso é feito inserindo as entradas corretas
-- Obs: ao inserir uma nova chave, se já existir uma chave igual no map, os valores da
-- chave antiga serão substituídos pelos valores da chave nova

-- lebrão e lebrões foram simplificados mas não deveriam ser
fixN :: M.Map T.Text [T.Text] -> M.Map T.Text [T.Text]
fixN m =
  M.insert (T.pack "lebrões") [(T.pack "lebre+N+AUG+M+PL"), (T.pack "lebre+N+M+PL")] $
  M.insert (T.pack "lebrão") [(T.pack "lebre+N+AUG+M+SG"),(T.pack "lebre+N+M+SG")] $
  M.insert (T.pack "lebrãozinho") [(T.pack "lebre+N+AUG+DIM+M+SG"),(T.pack "lebre+N+DIM+M+SG")] $
  M.insert (T.pack "lebrõezinhos") [(T.pack "lebre+N+AUG+DIM+M+PL"),(T.pack "lebre+N+DIM+M+PL")] m

-- a simplificação de "zurupável" estava errada, o motivo desse erro foi a duplicação de 
-- "zurupável	zurupável+A+M+SG" no arquivo adjectives-ae.dict
fixA :: M.Map T.Text [T.Text] -> M.Map T.Text [T.Text]
fixA =
  M.insert (T.pack "zurupável") [T.pack "zurupável+A+SG"]


---- Produção de novos arquivos 

serialize :: (T.Text, [T.Text]) -> [(T.Text,T.Text)]
serialize (k,xs) = map (\x -> (k,x)) xs

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

toText :: (T.Text, T.Text) -> T.Text
toText (k,x) = T.append k (T.append "\t" x)

separate :: Int -> [(T.Text,T.Text)] -> [[T.Text]]
separate n xs = splitEvery n (map toText (sortOn snd xs))


-- newADJ recebe o diretório adjectives do MorphoBr e um diretório
-- onde será salva a versão compacta
newADJ :: FilePath -> FilePath -> IO [()]
newADJ path outdir = do
  files <- listDirectory path
  m <- createSimpMap path files
  mapM (aux outdir) (splitEvery 19000 (map toText (sortOn snd $ concatMap serialize (M.toList $ fixA m))))
   where
    aux outdir (x:xs) =
     TO.writeFile (combine outdir ("adjectives-"++(take 3 $ T.unpack x)++".dict")) 
     (T.append (T.intercalate "\n" (x:xs)) "\n")

-- newNouns recebe o diretório nouns do MorphoBr e um diretório
-- onde será salva a versão compacta
newNouns :: FilePath -> FilePath -> IO [()]
newNouns path outdir = do
  files <- listDirectory path
  m <- createSimpMap path files
  mapM (aux outdir) (splitEvery 19000 (map toText (sortOn snd $ concatMap serialize (M.toList $ fixN m))))
   where
    aux outdir (x:xs) =
     TO.writeFile (combine outdir ("nouns-"++(take 3 $ T.unpack x)++".dict")) 
     (T.append (T.intercalate "\n" (x:xs)) "\n") 


---- Reconstrução a partir da versão simplificada

getEntries :: (T.Text,[T.Text]) -> [T.Text]
getEntries x
 | not (member (T.pack "F") (snd x) || member (T.pack "M") (snd x)) &&
     not (member (T.pack "SG") (snd x) || member (T.pack "PL") (snd x)) =
        [T.intercalate "+" (snd x ++ [T.pack "F",T.pack "SG"]),
         T.intercalate "+" (snd x ++ [T.pack "F",T.pack "PL"]),
         T.intercalate "+" (snd x ++ [T.pack "M",T.pack "SG"]),
         T.intercalate "+" (snd x ++ [T.pack "M",T.pack "PL"])]
  | not (member (T.pack "PL") (snd x)) && not (member (T.pack "SG") (snd x)) =
    [T.intercalate "+" (snd x ++ [T.pack "PL"]),
     T.intercalate "+" (snd x ++ [T.pack "SG"])]
  | not (member (T.pack "F") (snd x)) && not (member (T.pack "M") (snd x)) =
    [T.intercalate "+" (init (snd x) ++ [T.pack "F"]++ [last (snd x)]),
     T.intercalate "+" (init (snd x) ++ [T.pack "M"]++ [last (snd x)])]
  | otherwise = [T.intercalate "+" (snd x)]

expand :: [T.Text] -> [T.Text]
expand = concatMap  (getEntries . aux)
   where
     aux s = do
       let p = T.splitOn "\t" s in (head p, T.splitOn "+" (last p))

createExpMap :: FilePath -> [FilePath] -> IO (M.Map T.Text [T.Text])
createExpMap dir paths = do
  dicts <- mapM (readF1 . combine dir) paths
  return (M.map expand $ foldr (M.unionWith (++)) M.empty dicts)

-- recebe um diretório com os arquivos simplificados e o path onde será salva a versão reconstruída 
rebuild :: FilePath -> FilePath -> IO ()
rebuild path outpath = do
  files <- listDirectory path
  m <- createExpMap path files
  TO.writeFile outpath 
   (T.intercalate "\n" (map toText (sortOn snd $ concatMap serialize (M.toList m))))


---- formas sem plural ou singular


readDict :: FilePath -> IO (M.Map T.Text [T.Text])
readDict fn = do
  content <- TO.readFile fn
  return $ M.fromListWith (++) $ aux (T.lines content)
 where
   aux = map (\s -> let p = (T.splitOn "+" (last $ T.splitOn "\t" s)) in (head p, [s]))

countPL :: [T.Text] -> Int
countPL (x:xs)
 | member (T.pack "PL") (T.splitOn "+" x) = 1 + countPL xs
 | otherwise = countPL xs
countPL [] = 0

countSG :: [T.Text] -> Int
countSG (x:xs)
 | member (T.pack "SG") (T.splitOn "+" x) = 1 + countSG xs
 | otherwise = countSG xs
countSG [] = 0

getDifNum :: (T.Text,[T.Text]) -> String
getDifNum (lemma,xs)
 | (countSG xs) /= (countPL xs) = (intercalate ", " (map T.unpack xs)) ++ "\n" 
 | otherwise = ""

-- recebe o diretório dos arquivos a serem checados e um path onde serão salvas as listas de entradas 
-- que tem número de formas no plural diferente do número de formas no singular 
checkNum :: FilePath -> FilePath -> IO ()
checkNum path outpath = do
  files <- listDirectory path
  dicts <- mapM (readDict . combine path) files
  writeFile outpath $ concatMap getDifNum (M.toList $ foldr (M.unionWith (++)) M.empty dicts)
