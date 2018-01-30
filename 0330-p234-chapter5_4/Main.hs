{-|
Module      : Main
Description : 0330-p234-chapter5_4
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program computes power averages over a 10-week period.
-}

module Main (main) where

import qualified Data.Matrix as Matrix (fromList, getCol)
import           EngProb

nRows :: Int
nRows = 8

nCols :: Int
nCols = 7

main :: IO ()
main = do
    -- Open file, read content
    fileName <- getDataFileName "power1.dat"
    content <- readFile fileName

    -- Print out matrix
    let power = Matrix.fromList nRows nCols (map readDouble $ words content)
        averages = foldr
                    (\j items -> fromJust (mean $ Matrix.getCol j power) : items)
                    []
                    [1..nCols]

    forM_ (zip ([1..] :: [Int]) averages) $ \(j, average) ->
        putStrLn $ printf "Day %d: Average = %.2f" j average
