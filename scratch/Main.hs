module Main (main) where

import qualified Data.Vector.Unboxed as Vector
import           EngProb
import           Paths_eng_prob (getDataFileName)

main :: IO ()
main = do
    fileName <- getDataFileName "sensor3.dat"
    stream <- readFile fileName
    let result = (flip Vector.unfoldr) (tokenize stream) $ \ts ->
                    case parse ((,) <$> double <*> double) ts of
                        Valid pair ts' -> Just (pair, ts')
                        _ -> Nothing
    print result
    putStrLn "Done"

