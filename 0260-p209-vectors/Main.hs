{-|
Module      : Main
Description : 0260-p209-vectors
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

There are many Haskell packages that provide array-like data structures with efficient indexing etc. This program demonstrates the use of the @Vector@-family of data structures.
-}

module Main (main) where

import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import           EngProb (readDouble)
import           EngProb.Prelude
import           Paths_eng_prob

main :: IO ()
main = do
    let g :: Vector Double
        g = Vector.generate 21 $ \k -> fromIntegral k * 0.5
    print g

    fileName <- getDataFileName "sensor3.dat"
    stream <- readFile fileName

    let (ts, ms) =
            unzip $ unfoldr
                (\ls -> case ls of
                            l : ls' -> case words l of
                                            st : sm : _ -> Just ((readDouble st, readDouble sm), ls')
                                            _ -> Nothing
                            _ -> Nothing)
                (lines stream)
        times = Vector.fromList ts
        motions = Vector.fromList ms

    print times
    print motions
