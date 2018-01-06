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

import           EngProb
                    ( Parser
                    , double
                    , expandRange
                    , processM
                    , tokenize
                    )
import           Paths_eng_prob (getDataFileName)
import           Text.Printf (printf)

-- A parser for (time, motion) pairs
timeMotion :: Parser (Double, Double)
timeMotion = (,) <$> double <*> double

main :: IO ()
main = do
    -- Open file for lazy input
    fileName <- getDataFileName "sensor3.dat"
    stream <- readFile fileName

    -- Read data and compute summary information
    result <-
        processM timeMotion
            (tokenize stream)
            (0, 0.0, 0.0, 0.0) $ \(i, sm, mn, mx) (t, m) -> do
                putStrLn $ printf "%.1f %.1f" t m
                let (mn', mx') = expandRange i mn mx m
                return (i + 1, sm + m, mn', mx')

    let (n, sumMotion, minMotion, maxMotion) =
            case result of
                Left s -> error $ "Processing failed: " ++ s
                Right (x, _) -> x

    -- Show summary information
    putStrLn $ printf "Number of sensor readings: %d" n
    putStrLn $ printf "Average reading:           %.2f" (sumMotion / fromIntegral n)
    putStrLn $ printf "Maximum reading:           %.2f" maxMotion
    putStrLn $ printf "Minimum reading:           %.2f" minMotion
