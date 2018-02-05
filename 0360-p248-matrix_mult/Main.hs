{-|
Module      : Main
Description : 0360-p248-matrix_mult
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This function performs a matrix multiplication of two NxN matrices using sums of products.
-}

module Main (main) where

import           Data.Matrix (Matrix, matrix)
import qualified Data.Matrix as Matrix (fromList, getCol, getRow, nrows, ncols)
import qualified Data.Vector as Vector (sum, zipWith)

matrixMult :: Matrix Double -> Matrix Double -> Maybe (Matrix Double)
matrixMult a b =
    let aRows = Matrix.nrows a
        aCols = Matrix.ncols a
        bRows = Matrix.nrows b
        bCols = Matrix.ncols b
    in if aCols /= bRows
        then Nothing
        else Just $ matrix aRows bCols $ \(i, j) ->
                let aRow = Matrix.getRow i a
                    bCol = Matrix.getCol j b
                in Vector.sum (Vector.zipWith (*) aRow bCol)

main :: IO ()
main = do
    let a = Matrix.fromList 3 4 [1..]
        b = Matrix.fromList 4 2 [2..]
    print $ matrixMult a b
