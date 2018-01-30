{-|
Module      : Main
Description : 0290-p216-stat_lib
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program demonstrates simple analysis functions.
-}

module Main (main) where

import qualified Data.Vector.Algorithms.Intro as Vector (sort)
import           Data.Vector.Unboxed ((!), Unbox, Vector)
import qualified Data.Vector.Unboxed as Vector
import           EngProb

-- |Or just use Vector.maximum
myMaximum :: (Ord a, Unbox a) => Vector a -> Maybe a
myMaximum xs
    | Vector.null xs = Nothing
    | otherwise = Just $ Vector.foldl' max (Vector.head xs) xs

-- |Or just use Vector.minimum
myMinimum :: (Ord a, Unbox a) => Vector a -> Maybe a
myMinimum xs
    | Vector.null xs = Nothing
    | otherwise = Just $ Vector.foldl' min (Vector.head xs) xs

mean :: (Fractional a, Unbox a) => Vector a -> Maybe a
mean xs
    | Vector.null xs = Nothing
    | otherwise = Just $ Vector.sum xs / fromIntegral (Vector.length xs)

sorted :: (Ord a, Unbox a) => Vector a -> Vector a
sorted xs = runST $ do
    mxs <- Vector.thaw xs
    Vector.sort mxs
    Vector.unsafeFreeze mxs

median :: (Fractional a, Ord a, Unbox a) => Vector a -> Maybe a
median xs
    | Vector.null xs = Nothing
    | otherwise =
        let xs' = sorted xs
            n = Vector.length xs'
            k = n `div` 2
        in Just $
            if n `mod` 2 == 0
                then (xs' ! (k - 1) + xs' ! k) / 2
                else xs' ! k

-- |Sample variance
variance :: (Floating a, Unbox a) => Vector a -> Maybe a
variance xs
    | Vector.null xs = Nothing
    | otherwise = do
        mu <- mean xs
        let temp = Vector.map (\x -> (x - mu) ** 2) xs
        return $ (Vector.sum temp) / (fromIntegral $ Vector.length xs - 1)

stdDev :: (Floating a, Unbox a) => Vector a -> Maybe a
stdDev xs = do
    var <- variance xs
    return $ sqrt var

main :: IO ()
main =  do
    -- Open file, read content
    fileName <- getDataFileName "lab.dat"
    content <- readFile fileName

    -- Read data into vector
    let ys = Vector.unfoldr
                (\ws -> case ws of
                            h : t -> Just (readDouble h, t)
                            [] -> Nothing)
                (words content)
        maxY = fromJust $ myMaximum ys
        minY = fromJust $ myMinimum ys
        meanY = fromJust $ mean ys
        medianY = fromJust $ median ys
        varianceY = fromJust $ variance ys
        stdDevY = fromJust $ stdDev ys

    putStrLn $ printf "Maximum: %f" maxY
    putStrLn $ printf "Minimum: %f" minY
    putStrLn $ printf "Mean: %f" meanY
    putStrLn $ printf "Median: %f" medianY
    putStrLn $ printf "Variance: %f" varianceY
    putStrLn $ printf "Standard deviation: %f" stdDevY
