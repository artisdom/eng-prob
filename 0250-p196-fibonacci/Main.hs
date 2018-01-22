{-|
Module      : Main
Description : 0250-p196-fibonacci
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program demonstrates a function that computes the kth Fibonacci number using a recursive algorithm.
-}

module Main (main) where

import EngProb

-- |This function computes the @k@th Fibonacci number using a recursive algorithm
fibonacci :: Int -> Int
fibonacci k
    | k > 1 = fibonacci (k - 1) + fibonacci (k - 2)
    | otherwise = 1

main :: IO ()
main = do
    k <- prompt "Enter positive integer:\n"

    putStrLn $ printf "Fibonacci(%d) = %d" k (fibonacci k)
