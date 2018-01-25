{-|
Module      : Main
Description : 0130-p125-chapter3_6
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program generates a summary report from a data file that has a trailer record with negative values.
-}

module Main (main) where

import           EngProb (expandRange, readDouble)
import           EngProb.Prelude
import           Paths_eng_prob (getDataFileName)

main :: IO ()
main = do
    -- Open file for lazy input
    fileName <- getDataFileName "sensor2.dat"
    stream <- readFile fileName

    -- Read data and compute summary information
    let (_, motions) =
                unzip $ takeWhile
                    (\tm -> tm /= (-99 :: Double, -99 :: Double)) $
                    map (\l -> let t : m : _ = words l in (readDouble t, readDouble m)) (lines stream)
        (n, sumMotion, minMotion, maxMotion) =
            foldl'
                (\(i, sm, mn, mx) m ->
                    let (mn', mx') = expandRange i mn mx m
                    in (i + 1, sm + m, mn', mx'))
                (0, 0.0, 0.0, 0.0)
                motions

    -- Show summary information
    putStrLn $ printf "Number of sensor readings: %d" n
    putStrLn $ printf "Average reading:           %.2f" (sumMotion / fromIntegral n)
    putStrLn $ printf "Maximum reading:           %.2f" maxMotion
    putStrLn $ printf "Minimum reading:           %.2f" minMotion
