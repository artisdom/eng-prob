{-|
Module      : Main
Description : 0270-p211-chapter5_1
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program reads 100 values from a data file and determines the number of values greater than the average.
-}

module Main (main) where

import qualified Data.Vector.Unboxed as Vector
import           EngProb

n :: Int
n = 10

main :: IO ()
main =  do
    -- Open file, read content
    fileName <- getDataFileName "lab.dat"
    content <- readFile fileName

    -- Read data into vector
    let y = Vector.unfoldrN
                n
                (\ws -> case ws of
                            h : t -> Just (readDouble h, t)
                            [] -> Nothing)
                (words content)

    when (Vector.length y /= n) $
        error ("File did not contain expected number of values")

    let
        -- Compute a sum of the values
        ySum = Vector.sum y
        -- Compute average
        yAve = ySum / fromIntegral n
        -- Count values that are greater than the average
        count :: Int
        count = Vector.foldl' (\c yk -> if yk > yAve then c + 1 else c) 0 y

    -- Print count
    putStrLn $ printf "%d values greater than the average" count
