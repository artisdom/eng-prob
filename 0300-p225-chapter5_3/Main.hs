{-|
Module      : Main
Description : 0300-p225-chapter5_3
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program computes a set of statistical measurements from a speech signal.
-}

module Main (main) where

import           Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as Vector
import           EngProb

readVector :: String -> Vector Double
readVector =
    (Vector.unfoldr
        (\ws -> case ws of
                    h : t -> Just (readDouble h, t)
                    [] -> Nothing)) . words

averagePower :: (Floating a, Unbox a) => Vector a -> Maybe a
averagePower xs
    | Vector.null xs = Nothing
    | otherwise = Just $ Vector.sum (Vector.map (** 2) xs) / fromIntegral (Vector.length xs)

averageMagnitude :: (Fractional a, Unbox a) => Vector a -> Maybe a
averageMagnitude xs
    | Vector.null xs = Nothing
    | otherwise = Just $ Vector.sum (Vector.map abs xs) / fromIntegral (Vector.length xs)

crossings :: (Num a, Ord a, Unbox a) => Vector a -> Int
crossings xs =
    Vector.foldl'
        (\n x -> if x < 0 then n + 1 else n)
        0
        (Vector.zipWith (*) xs (Vector.tail xs))

main :: IO ()
main =  do
    -- Open file, read content
    fileName <- getDataFileName "zero.dat"
    content <- readFile fileName
    let speech = readVector content

    -- Compute and print statistics
    putStrLn "Digit Statistics"
    putStrLn $ printf "     mean: %f" (fromJust $ mean speech)
    putStrLn $ printf "     standard deviation: %f" (fromJust $ standardDeviation speech)
    putStrLn $ printf "     variance: %f" (fromJust $ sampleVariance speech)
    putStrLn $ printf "     average power: %f" (fromJust $ averagePower speech)
    putStrLn $ printf "     average magnitude: %f" (fromJust $ averageMagnitude speech)
    putStrLn $ printf "     zero crossings: %d" (crossings speech)
