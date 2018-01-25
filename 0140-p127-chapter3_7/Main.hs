{-|
Module      : Main
Description : 0140-p127-chapter3_7
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program generates a summary report from a data file that does not have a header record or a trailer record.
-}

module Main (main) where

import EngProb

main :: IO ()
main = do
    -- Open file for lazy input
    fileName <- getDataFileName "sensor3.dat"
    content <- readFile fileName

    -- Read data and compute summary information
    let motions =
            unfoldr
                (\ls -> case ls of
                            l : ls' -> case words l of
                                            _ : m : _ -> Just (readDouble m, ls')
                                            _ -> Nothing
                            _ -> Nothing)
                (lines content)
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
