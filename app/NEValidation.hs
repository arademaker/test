module NEValidation where

import WKS
import NLU
import Data.Aeson
import System.Exit
import System.Environment
import Data.Either


-- Receive [json-WKS, json-NLU] and check if the texts are the same
checkTexts :: [FilePath] -> IO Bool
checkTexts [gjson, json] = do
  jsonWKS <- readGJson gjson
  jsonNLU <- readJSON json
  return $ verify jsonNLU jsonWKS
    where
      verify (Right x) (Right y) = analyzed_text x == docText y
      verify _ _ = False
    


-- future main:

-- msg =
--   " Checking for differences between WKS and NLU. \n\
--   \ Usage: \n\
--   \     NEValidation -t [json-WKS, json-NLU] (check if the texts are the same)"

-- help = putStrLn msg

-- parse ["-h"]       = help >> exitSuccess
-- parse ("-t":ls)    = checkTexts ls >> exitSuccess
-- parse ls           = help >> exitFailure

-- main :: IO ()
-- main = getArgs >>= parse

main :: IO ()
main = putStrLn "OK"