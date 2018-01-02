{-|
Module      : Main
Description : 0030-p68-chapter2_1
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program uses linear interpolation to compute the coefficient of lift for an angle.
-}

module Main (main) where

import EngProb (prompt)
import Text.Printf (printf)

main :: IO ()
main = do
    -- Get user input from the keyboard
    putStrLn "Use degrees for all angle measurements."
    a <- prompt "Enter first angle: "
    f_a <- prompt "Enter first lift coefficient: "
    c <- prompt "Enter second angle: "
    f_c <- prompt "Enter second lift coefficient: "
    b <- prompt "Enter new angle: "

    -- Use linear interpolation to compute lift
    let f_b = f_a + (b - a) / (c - a) * (f_c - f_a)

    -- Print new lift value
    putStrLn $ printf "New lift coefficient: %6.3f" (f_b :: Double)
