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

import           Control.Monad (forM_)
import           EngProb (prompt)
import           System.Random (StdGen, mkStdGen, randomR)
import           Text.Printf (printf)

repeatN :: Int -> (a -> a) -> a -> a
repeatN n f x = foldr (\_ x' -> f x') x [1..n]

main :: IO ()
main = do
    -- Get seed value and interval limits
    seed <- prompt "Enter a positive integer seed value:\n"
    a <- prompt "Enter integer limit a:\n"
    b <- prompt "Enter integer limit b (a < b):\n"

    let
        randInt :: StdGen -> (Int, StdGen)
        randInt = randomR (a, b)

        -- Create a random generator with given seed
        g = mkStdGen seed

        -- Generate ten random numbers
        (values, _) = repeatN 10
                        (\(values', g') ->
                            let (value'', g'') = randInt g'
                            in (value'' : values', g''))
                        ([], g)

    -- Print random numbers
    putStrLn "Random Numbers:"
    forM_ values $ \value ->
        putStr $ printf "%d " value
    putStrLn ""
