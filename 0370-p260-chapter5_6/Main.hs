{-|
Module      : Main
Description : 0370-p260-chapter5_6
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program uses Gauss elimination to determine the mesh currents for a circuit.
-}

module Main (main) where

import           Data.Matrix ((!), Matrix)
import qualified Data.Matrix as Matrix (fromList, mapRow, nrows, setElem)
import           Data.Vector ((//) ,Vector)
import qualified Data.Vector as Vector ((!), replicate)
import           EngProb

n :: Int
n = 3

main :: IO ()
main = do
    -- Get user input
    r1 <- prompt "Enter resistor value R1 in ohms:\n"
    r2 <- prompt "Enter resistor value R2 in ohms:\n"
    r3 <- prompt "Enter resistor value R3 in ohms:\n"
    r4 <- prompt "Enter resistor value R4 in ohms:\n"
    r5 <- prompt "Enter resistor value R5 in ohms:\n"
    v1 <- prompt "Enter voltage value V1 in volts:\n"
    v2 <- prompt "Enter voltage value V2 in volts:\n"

    -- Specify equation coefficients
    let a = Matrix.fromList n (n + 1)
                [ r1 + r2, -r2, 0, v1
                , -r2, r2 + r3 + r4, -r4, 0
                , 0, 0, r4 + r5, -v2
                ]

        -- Perform elimination step
        a' = foldl' (\aTemp index -> eliminate aTemp index) a [1..n - 1]

        -- Perform back substitution step
        soln = backSubstitute a'

    -- Print solution
    forM_ [1..n] $ \i ->
        putStrLn $ printf "Mesh current %d: %f" i ((Vector.!) soln (i - 1))

-- Data.Matrix.rref exists, but we'll implement it from scratch...
eliminate :: Matrix Double -> Int -> Matrix Double
eliminate a index =
    let rowCount = Matrix.nrows a
    in foldl' (\a' row ->
        let scaleFactor = -(a' ! (row, index)) / a' ! (index, index)
        in Matrix.mapRow (\col x ->
            if col == index
                then 0
                else if col > index
                    then x + a' ! (index, col) * scaleFactor
                    else x)
            row a')
            a [index + 1..rowCount]

backSubstitute :: Matrix Double -> Vector Double
backSubstitute a =
    let rowCount = Matrix.nrows a
        (_, result) = foldl' (\(a', soln) row ->
            if row == rowCount
                then (a', soln // [(rowCount - 1, a' ! (rowCount, rowCount + 1) / a' ! (rowCount, rowCount))])
                else
                    let temp = foldl' (\acc col ->
                                acc + (Vector.!) soln (col - 1) * a' ! (row, col))
                                0
                                [rowCount, rowCount - 1..row + 1]
                        newValue = a' ! (row, rowCount + 1) - temp
                        a'' = Matrix.setElem newValue (row, rowCount + 1) a'
                        soln' = soln // [(row - 1, newValue / a'' ! (row, row))]
                    in (a'', soln'))
            (a, Vector.replicate rowCount 0)
            [rowCount, rowCount - 1, 1]
        in result
