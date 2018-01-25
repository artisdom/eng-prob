{-|
Module      : Main
Description : 0010-p24-chapter1_1
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program computes the distance between two points.
-}

module Main (main) where

import EngProb

main :: IO ()
main = do
    let x1 = 1
        y1 = 5
        x2 = 4
        y2 = 7

        -- Compute sides of a right-angled triangle
        side_1 = x2 - x1
        side_2 = y2 - y1

        -- Compute the hypotenuse
        distance = sqrt(side_1 ** 2 + side_2 ** 2)

    putStrLn $ printf "The distance between the two points is %5.2f" (distance :: Double)
