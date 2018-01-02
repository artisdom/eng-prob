{-|
Module      : Main
Description : 0110-p116-chapter3_4
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program prints a table of height and velocity values for a weather balloon.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (forM_)
import Data.List (foldl')
import EngProb (prompt)
import Text.Printf (printf)

main :: IO ()
main = do
    -- Get user input
    initial :: Double <- prompt "Enter initial value for table (in hours) "
    increment :: Double <- prompt "Enter increment between lines (in hours) "
    final :: Double <- prompt "Enter final value for table (in hours) "

    putStrLn "\n\nWeather Ballon Information"
    putStrLn "Time    Height    Velocity"
    putStrLn "(hrs)   (metres)  (metres/s)"

    -- TODO: Something feels wrong about ranges with non-integral limits
    -- Think about this some more!
    let times = [initial, initial + increment..final]
        readings = map (\time ->
                        let height = (-0.12) * time ** 4
                                + 12 * time ** 3
                                + (-380) * time ** 2
                                + 4100 * time
                                + 220
                            velocity = (-0.48) * time ** 3
                                + 36 * time ** 2
                                + (-760) * time
                                + 4100
                        in (time, height, velocity)) times
        (maxTime, maxHeight) = foldl' (\p@(_, maxH) (t, h, _) ->
                                if h > maxH then (t, h) else p) (0, 0) readings

    forM_ readings $ \(time, height, velocity) ->
        putStrLn $ printf "%6.2f  %8.2f   %7.2f" time height (velocity / 3600)

    putStrLn ""
    putStrLn $ printf "Maximum balloon height was %8.2f metres" maxHeight
    putStrLn $ printf "and it occurred at %6.2f hours" maxTime
