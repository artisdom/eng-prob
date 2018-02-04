{-|
Module      : Main
Description : 0350-p246-transpose
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This function generates a matrix transpose.
-}

module Main (main) where

import           Data.Matrix ((!), Matrix, matrix)
import qualified Data.Matrix as Matrix (fromList, nrows, ncols)
import           EngProb

transpose :: Matrix Double -> Matrix Double
transpose b =
    matrix
        (Matrix.ncols b)
        (Matrix.nrows b) $
            \(i, j) -> b ! (j, i)

main :: IO ()
main = do
    -- Open file, read content into matrix
    fileName <- getDataFileName "grid1.dat"
    content <- readFile fileName

    let nRowsStr : nColsStr : ws = words content
        nRows = readInt nRowsStr
        nCols = readInt nColsStr
        b = Matrix.fromList nRows nCols (map readDouble ws)
        bt = transpose b
    print b
    print bt
