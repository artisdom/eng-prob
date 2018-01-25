{-|
Module      : Main
Description : 0180-p166-chapter4_3
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program generates and prints ten random integers between 1 and RAND_MAX.
-}

module Main (main) where

import           EngProb
import           System.Random (mkStdGen, randomRs)

randMax :: Int
randMax = 32767

main :: IO ()
main = do
    -- Get seed value from the user
    seed <- prompt "Enter a positive integer seed value:\n"

    let
        -- Create a random generator with given seed
        g = mkStdGen seed

        -- Generate ten random numbers
        values = take 10 (randomRs (0, randMax) g)

    -- Print random numbers
    putStrLn "Random Numbers:"
    forM_ values $ \value ->
        putStr $ printf "%d " value
    putStrLn ""
