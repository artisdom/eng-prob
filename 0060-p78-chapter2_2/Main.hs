{-|
Module      : Main
Description : 0060-p78-chapter2_2
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This programs estimates new velocity and acceleration values for a specified time.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import EngProb

main :: IO ()
main = do
    -- Get time value from the keyboard
    time :: Double <- prompt "Enter new time value in seconds: "

    -- Compute velocity and acceleration
    let velocity = 0.00001 * time ** 3 - 0.00488 * time ** 2 + 0.75795 * time + 181.3566
        acceleration = 3 - 0.000062 * velocity ** 2

    -- Print velocity and acceleration
    putStrLn $ printf "Velocity = %8.3f m/s" velocity
    putStrLn $ printf "Acceleration = %8.3f m/s^2" acceleration
