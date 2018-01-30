{-|
Module      : Main
Description : 0340-p242-chapter5_5
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program determines the location of peaks in an elevation grid of data.
-}

module Main (main) where

import           Data.Matrix ((!))
import qualified Data.Matrix as Matrix (fromList)
import           EngProb

main :: IO ()
main = do
    -- Open file, read content into matrix
    fileName <- getDataFileName "grid1.dat"
    content <- readFile fileName

    let nRowsStr : nColsStr : ws = words content
        nRows = readInt nRowsStr
        nCols = readInt nColsStr
        elevation = Matrix.fromList nRows nCols (map readDouble ws)
        peaks =
            foldr
                (\(i, j) ps ->
                    let value = elevation ! (i, j)
                    in if elevation ! (i - 1, j) < value &&
                            elevation ! (i + 1, j) < value &&
                            elevation ! (i, j - 1) < value &&
                            elevation ! (i, j + 1) < value
                            then (i - 1, j - 1) : ps
                            else ps)
                []
                ((,) <$> [2..nRows - 1] <*> [2..nCols - 1])

    forM_ peaks $ \(i, j) ->
        putStrLn $ printf "Peak at row: %d column: %d" i j
