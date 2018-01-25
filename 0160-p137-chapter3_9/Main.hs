{-|
Module      : Main
Description : 0160-p137-chapter3_9
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program computes a linear model for a set of altitude and ozone mixing ratio values.
-}

module Main (main) where

import           EngProb (expandRange)
import           EngProb.Prelude
import           Paths_eng_prob (getDataFileName)
import           Text.Printf (printf)

main :: IO ()
main = do
    -- Open and read file
    fileName <- getDataFileName "zone1.dat"
    stream <- readFile fileName

    let xys =
            unfoldr
                (\ls -> case ls of
                            l : ls' -> case words l of
                                            sx : sy : _ -> Just ((read sx, read sy), ls')
                                            _ -> Nothing
                            _ -> Nothing)
                (lines stream)

        -- Accumulate information
        (n, minHeight, maxHeight, sumx, sumy, sumx2, sumxy) =
            foldl'
                (\(i, minX, maxX, sx, sy, sx2, sxy) (x, y) ->
                    let (minX', maxX') = expandRange i minX maxX x
                    in (i + 1, minX', maxX', sx + x, sy + y, sx2 + x * x, sxy + x * y))
                (0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
                xys

        floatN = fromIntegral n
        denominator = sumx * sumx - floatN * sumx2
        m = (sumx * sumy - floatN * sumxy) / denominator
        b = (sumx * sumxy - sumx2 * sumy) / denominator

    -- Print summary of information
    putStrLn "Range of altitudes in km:"
    putStrLn $ printf "%.2f to %.2f\n" minHeight maxHeight
    putStrLn "Linear model:"
    putStrLn $ printf "ozone-mix-ratio = %.2f altitude + %.2f" m b
