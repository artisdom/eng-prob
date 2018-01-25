{-|
Module      : Main
Description : 0220-p186-chapter4_6
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program estimates the real roots of a polynomial function using incremental search.
-}

module Main (main) where

import EngProb

type Interval = (Double, Double)
type Func = Double -> Double

epsilon :: Double
epsilon = 0.1e-4

poly :: Double -> Double -> Double -> Double -> Double -> Double
poly a0 a1 a2 a3 x =
    a0 * x ** 3 +
    a1 * x ** 2 +
    a2 * x +
    a3

searchRoots :: Func -> Interval -> Double -> [Double]
searchRoots f (a, b) step =
    foldr
        (\k roots ->
            let left = a + fromIntegral k * step
                right =  left + step
            in case checkRoots f left right of
                Nothing -> roots
                Just root -> root : roots)
        []
        ([0..ceiling $ (b - a) / step] :: [Int])

checkRoots :: Func -> Double -> Double -> Maybe Double
checkRoots f left right = checkRootsHelper left right (f left) (f right)

checkRootsHelper :: Double -> Double -> Double -> Double -> Maybe Double
checkRootsHelper left right fLeft fRight
    | abs fLeft < epsilon = Just left
    | abs fRight < epsilon = Nothing
    | fLeft * fRight < 0 = Just $ (left + right) / 2
    | otherwise = Nothing

main :: IO ()
main = do
    a0 <- prompt "Enter coefficient a0:\n"
    a1 <- prompt "Enter coefficient a1:\n"
    a2 <- prompt "Enter coefficient a2:\n"
    a3 <- prompt "Enter coefficient a3:\n"
    a <- prompt "Enter interval limit a:\n"
    b <- prompt "Enter interval limit b (a < b):\n"
    step <- prompt "Enter step size:\n"

    -- Should return -4, 2
    let roots = searchRoots (poly a0 a1 a2 a3) (a, b) step
    forM_ roots $ \root ->
        putStrLn $ printf "Root detected at %.3f" root

