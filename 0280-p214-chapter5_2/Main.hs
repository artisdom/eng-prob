{-|
Module      : Main
Description : 0280-p214-chapter5_2
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program reads values from a data file and determines the maximum value with a function.
-}

module Main (main) where

import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import           EngProb

-- |Or just use Vector.maximum
myMaximum :: (Ord a, Vector.Unbox a) => Vector a -> Maybe a
myMaximum xs
    | Vector.null xs = Nothing
    | otherwise = Just $ Vector.foldl' max (Vector.head xs) xs

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

    -- Find and print the maximum value
    putStrLn $ printf "Maximum value: %f" maxY
