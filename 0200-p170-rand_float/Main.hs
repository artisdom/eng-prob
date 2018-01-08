{-|
Module      : Main
Description : 0200-p170-rand_float
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program generates and prints ten random floating-point numbers between user-specified limits.
-}

module Main (main) where

import           Control.Monad (forM_)
import           EngProb (prompt)
import           System.Random (StdGen, mkStdGen, randomRs)
import           Text.Printf (printf)

main :: IO ()
main = do
    -- Get seed value and interval limits
    seed <- prompt "Enter a positive integer seed value:\n"
    a <- prompt "Enter limit a:\n"
    b <- prompt "Enter limit b (a < b):\n"

    let
        randFloats :: StdGen -> [Double]
        randFloats = randomRs (a, b)

        -- Create a random generator with given seed
        g = mkStdGen seed

        -- Generate ten random numbers
        values = take 10 (randFloats g)

    -- Print random numbers
    putStrLn "Random Numbers:"
    forM_ values $ \value ->
        putStr $ printf "%g " value
    putStrLn ""
