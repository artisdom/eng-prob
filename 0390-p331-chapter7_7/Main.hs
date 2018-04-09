{-|
Module      : Main
Description : 0390-p331-chapter7_7
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program computes the molecular weight of an amino acid from its chemical formula.
-}

module Main (main) where

import           Data.Char (isDigit, toUpper)
import qualified Data.Map as Map (fromList, lookup)
import           EngProb (promptString, readInt, unfoldr)
import           Text.Printf (printf)

takeWhile' :: (a -> Bool) -> [a] -> ([a], [a])
takeWhile' f s = go s []
    where
        go ys@(x : xs) acc
            | f x = go xs  (acc ++ [x])
            | otherwise = (acc, ys)
        go _ acc = (acc, [])

atomicWeight :: Char -> Maybe Double
atomicWeight = (flip Map.lookup) atomicWeights
    where
        atomicWeights = Map.fromList
            [ ('H', 1.00794)
            , ('C', 12.011)
            , ('N', 14.00674)
            , ('O', 15.9994)
            , ('S', 32.066)
            ]

main :: IO ()
main = do
    formula <- promptString "Enter chemical formula for amino acid:\n"
    let total = sum $ unfoldr (\s ->
            case s of
                h : t0 ->
                    let Just weight = atomicWeight (toUpper h)
                        (c, t1) = takeWhile' isDigit t0
                        count = case c of [] -> 1; _ -> readInt c
                    in Just (weight * fromIntegral count, t1)
                _ -> Nothing) formula

    putStrLn $ printf "Molecular weight: %f" total
