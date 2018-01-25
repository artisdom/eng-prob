{-|
Module      : Main
Description : 0050-p72-degrees-radians
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program demonstrates reusable degree/radian conversion programs.
-}

module Main (main) where

import EngProb

radiansToDegrees :: (Floating a, Fractional a) => a -> a
--radiansToDegrees aRad = aRad * 180 / pi
radiansToDegrees = (/ pi) . (180 *)

degreesToRadians :: (Floating a, Fractional a) => a -> a
--degreesToRadians aDeg = aDeg * pi / 180
degreesToRadians = (/ 180) . (pi *)

showRadiansToDegrees :: Double -> IO ()
showRadiansToDegrees aRad = putStrLn $ printf "%g radians = %g degrees" aRad (radiansToDegrees aRad)

showDegreesToRadians :: Double -> IO ()
showDegreesToRadians aDeg = putStrLn $ printf "%g degrees = %g radians" aDeg (degreesToRadians aDeg)

main :: IO ()
main = do
    showRadiansToDegrees 0
    showRadiansToDegrees $ pi / 2
    showRadiansToDegrees pi
    showRadiansToDegrees $ 3 * pi / 2
    showRadiansToDegrees $ 2 * pi

    showDegreesToRadians 0
    showDegreesToRadians 90
    showDegreesToRadians 180
    showDegreesToRadians 270
    showDegreesToRadians 360
