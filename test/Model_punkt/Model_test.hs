import Data.Text (Text, pack)
import NLP.Punkt (find_breaks)


main :: IO ()
main = do
    file <- readFile "1.raw"

    mapM_ print $ find_breaks $ pack file