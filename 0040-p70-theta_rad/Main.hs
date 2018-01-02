{-|
Module      : Main
Description : 0040-p70-theta_rad
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program converts degrees to radians and evaluates the @sin@ function.
-}

module Main (main) where

import EngProb (prompt)
import Text.Printf (printf)

main :: IO ()
main = do
    -- Get user input from the keyboard
    theta <- prompt "Enter theta in degrees: "

    -- Convert angle in degrees to radians
    let theta_rad = theta * pi / 180

    -- Compute sine of angle
    let b = sin theta_rad

    -- Print sine of angle
    putStrLn $ printf "Sine of %g degrees (%g rad) = %g" theta (theta_rad :: Double) b
