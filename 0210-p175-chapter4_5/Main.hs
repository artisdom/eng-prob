{-|
Module      : Main
Description : 0210-p175-chapter4_5
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program estimates the reliability of a series and a parallel configuration using a computer simulation.
-}

module Main (main) where

import           EngProb (prompt)
import           System.Random (StdGen, mkStdGen, randomRs)
import           Text.Printf (printf)

repeatN :: Int -> (a -> a) -> a -> a
repeatN n f x = foldr (\_ x' -> f x') x [1..n]

toDouble :: Int -> Double
toDouble = fromIntegral

main :: IO ()
main = do
    -- Get information for the simulation
    componentReliability <- prompt "Enter individual component reliability:\n"
    n <- prompt "Enter number of trials:\n"
    seed <- prompt "Enter unsigned integer seed:\n"

    let
        -- Compute analytical reliabilities
        aSeries = componentReliability ** 3
        aParallel = 3 * componentReliability
                        + (-3) * componentReliability ** 2
                        + componentReliability ** 3

        randFloats :: StdGen -> [Double]
        randFloats = randomRs (0.0, 1.0)

        -- Create a random generator with given seed
        g = mkStdGen seed

        -- Determine simulation reliability estimates
        (seriesSuccess, parallelSuccess, _) =
            repeatN n
                (\(ss, ps, nums) ->
                    let num1 : num2 : num3 : nums' = nums
                        ss' = if num1 <= componentReliability &&
                            num2 <= componentReliability &&
                            num3 <= componentReliability
                            then ss + 1 else ss
                        ps' = if num1 <= componentReliability ||
                            num2 <= componentReliability ||
                            num3 <= componentReliability
                            then ps + 1 else ps
                    in (ss', ps', nums'))
                (0, 0, randFloats g)

    -- Print results
    putStrLn "Analytical Reliability"
    putStrLn $ printf "Series: %.3f  Parallel: %.3f" aSeries aParallel
    putStrLn $ printf "Simulation Reliability, %d trials" n
    putStrLn $ printf "Series: %.3f  Parallel: %.3f" (toDouble seriesSuccess / toDouble n) (toDouble parallelSuccess / toDouble n)
