{-|
Module      : Main
Description : 0320-p232-matrices
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program demonstrates matrices (instead of C's two-dimensional arrays).
-}

module Main (main) where

import qualified Data.Matrix as Matrix (fromList)
import           EngProb

nRows :: Int
nRows = 10

nCols :: Int
nCols = 5

main :: IO ()
main = do
    -- Open file, read content
    fileName <- getDataFileName "rcook-rands.dat"
    content <- readFile fileName

    -- Print out matrix
    print $ Matrix.fromList nRows nCols (words content)
