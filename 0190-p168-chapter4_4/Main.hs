{-|
Module      : Main
Description : 0190-p168-chapter4_4
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program generates and prints ten random integers between user-specified limits.
-}

module Main (main) where

import           EngProb (prompt)
import           EngProb.Prelude
import           System.Random (StdGen, mkStdGen, randomRs)

main :: IO ()
main = do
    -- Get seed value and interval limits
    seed <- prompt "Enter a positive integer seed value:\n"
    a <- prompt "Enter integer limit a:\n"
    b <- prompt "Enter integer limit b (a < b):\n"

    let
        randInts :: StdGen -> [Int]
        randInts = randomRs (a, b)

        -- Create a random generator with given seed
        g = mkStdGen seed

        -- Generate ten random numbers
        values = take 10 (randInts g)

    -- Print random numbers
    putStrLn "Random Numbers:"
    forM_ values $ \value ->
        putStr $ printf "%d " value
    putStrLn ""
