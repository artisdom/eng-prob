{-|
Module      : Main
Description : 0120-p123-chapter3_5
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program generates a summary report from a data file that has the number of data points in the first record.
-}

module Main (main) where

import           EngProb (expandRange, readDouble, readInt)
import           EngProb.Prelude
import           Paths_eng_prob (getDataFileName)

main :: IO ()
main = do
    -- Open file for lazy input
    fileName <- getDataFileName "sensor1.dat"
    stream <- readFile fileName

    let h : ls = lines stream
        n = readInt h

        -- Read data and compute summary information
        motions = map (\l -> let _ : m : _ = words l in readDouble m) (take n ls)
        (_, sumMotion, minMotion, maxMotion) =
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
