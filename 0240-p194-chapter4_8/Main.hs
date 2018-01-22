{-|
Module      : Main
Description : 0240-p194-chapter4_8
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

Description of original C program: "This program compares a recursive function and a nonrecursive function for computing factorials."

Obviously, this doesn't make that much sense, so we'll present a recursive version only.
-}

module Main (main) where

import EngProb

-- This function computes a factorial recursively
factorial :: Integer -> Integer
factorial 0 = 1
factorial k = k * factorial (k - 1)

main :: IO ()
main = do
    -- Get user input
    n <- prompt "Enter positive integer:\n"

    -- Compute and print factorial
    putStrLn $ printf "Recursive: %d! = %d" n (factorial n)
