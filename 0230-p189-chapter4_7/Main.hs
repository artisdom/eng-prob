{-|
Module      : Main
Description : 0230-p189-chapter4_7
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program converts a temperature in Fahrenheit to Centigrade.
-}

module Main (main) where

import EngProb

-- |Convert degrees Fahrenheit to celsius
-- While Haskell does have a preprocessor (and, therefore, macros),
-- idiomatic Haskell prefers functions to macros in most cases
degreesC :: Double -> Double
degreesC x = (x - 32) * (5 / 9)

main :: IO ()
main = do
    temp <- prompt "Enter temperature in degrees Fahrenheit:\n"

    putStrLn $ printf "%f degrees Centigrade" (degreesC temp)

