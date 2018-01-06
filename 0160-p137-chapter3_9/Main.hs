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

import           EngProb
                    ( Parser
                    , double
                    , expandRange
                    , processM
                    , tokenize
                    )
import           Paths_eng_prob (getDataFileName)
import           Text.Printf (printf)

record :: Parser (Double, Double)
record = (,) <$> double <*> double

main :: IO ()
main = do
    -- Open and read file
    fileName <- getDataFileName "zone1.dat"
    stream <- readFile fileName

    -- Accumulate information
    result <-
        processM record
            (tokenize stream)
            (0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0) $ \(i, minX, maxX, sx, sy, sx2, sxy) (x, y) -> do
                putStrLn $ printf "%.1f %.1f" x y
                let (minX', maxX') = expandRange i minX maxX x
                return (i + 1, minX', maxX', sx + x, sy + y, sx2 + x * x, sxy + x * y)

    let (n, minHeight, maxHeight, sumx, sumy, sumx2, sumxy) =
            case result of
                Left s -> error $ "Processing failed: " ++ s
                Right (x, _) -> x
        floatN = fromIntegral n
        denominator = sumx * sumx - floatN * sumx2
        m = (sumx * sumy - floatN * sumxy) / denominator
        b = (sumx * sumxy - sumx2 * sumy) / denominator

    -- Print summary of information
    putStrLn "Range of altitudes in km:"
    putStrLn $ printf "%.2f to %.2f\n" minHeight maxHeight
    putStrLn "Linear model:"
    putStrLn $ printf "ozone-mix-ratio = %.2f altitude + %.2f" m b
