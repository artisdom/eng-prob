{-|
Module      : EngProb.StatLib
Description : Simple analytical functions on vectors
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This module defines assorted simple analytical functions on vectors.
-}

module EngProb.StatLib
    ( maximum
    , mean
    , median
    , minimum
    , sampleVariance
    , sorted
    , standardDeviation
    ) where

import qualified Data.Vector.Algorithms.Intro as Vector (sort)
import           Data.Vector.Unboxed (Unbox)
import           Data.Vector.Generic ((!), Vector)
import qualified Data.Vector.Generic as Vector
                    ( length
                    , map
                    , null
                    , sum
                    , thaw
                    , unsafeFreeze
                    )
import           EngProb.Prelude

mean :: (Fractional a, Unbox a, Vector v a) => v a -> Maybe a
mean xs
    | Vector.null xs = Nothing
    | otherwise = Just $ Vector.sum xs / fromIntegral (Vector.length xs)

sorted :: (Ord a, Unbox a, Vector v a) => v a -> v a
sorted xs = runST $ do
    mxs <- Vector.thaw xs
    Vector.sort mxs
    Vector.unsafeFreeze mxs

median :: (Fractional a, Ord a, Unbox a, Vector v a) => v a -> Maybe a
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

sampleVariance :: (Floating a, Unbox a, Vector v a) => v a -> Maybe a
sampleVariance xs
    | Vector.null xs = Nothing
    | otherwise = do
        mu <- mean xs
        let temp = Vector.map (\x -> (x - mu) ** 2) xs
        return $ (Vector.sum temp) / (fromIntegral $ Vector.length xs - 1)

standardDeviation :: (Floating a, Unbox a, Vector v a) => v a -> Maybe a
standardDeviation xs = do
    var <- sampleVariance xs
    return $ sqrt var
