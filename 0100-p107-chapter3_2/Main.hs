{-|
Module      : Main
Description : 0100-p107-chapter3_2
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program prints a degree-to-radian table using @forM_@ only and @do@-notation.
-}

module Main (main) where

import EngProb.Prelude

main :: IO ()
main = do
    -- Print degrees and radians over a range
    putStrLn "Degrees to Radians"
    forM_ [0 :: Int, 10..360] $ \d -> do
        let degrees = fromIntegral d
            radians = degrees * pi / 180
        putStrLn $ printf "%6f %9.6f" (degrees :: Double) radians
