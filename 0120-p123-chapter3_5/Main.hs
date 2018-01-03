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

{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (forM_)
import Data.List (foldl')
import EngProb (readValue)
import Paths_eng_prob (getDataFileName)
import Prelude hiding (max, min, sum)
import Text.Printf (printf)

readTimeMotion :: String -> ((Double, Double), String)
readTimeMotion s =
    let (time, s1) = readValue s
        (motion, s2) = readValue s1
    in ((time, motion), s2)

main :: IO ()
main = do
    -- Open file
    file_name <- getDataFileName "sensor1.dat"
    s <- readFile file_name

    -- Read the number of data points
    let (numDataPts :: Int, s1) = readValue s

    -- Read data and compute summary information
    let (_, items, sum, min, max) = foldl'
                                        (\(s', items', sum', min', max') k ->
                                            let (item@(_, motion), s0) = readTimeMotion s'
                                                (min0, max0) = if k == 1 then (motion, motion) else (min', max')
                                                min1 = if motion < min0 then motion else min0
                                                max1 = if motion > max0 then motion else max0
                                            in (s0, items' ++ [ item ], sum' + motion, min1, max1)
                                        )
                                        (s1, [], 0, 0, 0)
                                        [1..numDataPts]

    forM_ items $ \(time, motion) ->
        putStrLn $ printf "%.2f %.f" time motion

    putStrLn $ printf "Number of sensor readings: %d" numDataPts
    putStrLn $ printf "Average reading:           %.2f" (sum / fromIntegral numDataPts)
    putStrLn $ printf "Maximum reading:           %.2f" max
    putStrLn $ printf "Minimum reading:           %.2f" min
