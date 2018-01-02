{-|
Module      : Main
Description : 0080-p104-case
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program demonstrates pattern matching and compares it to nested @if@s.
-}

module Main (main) where

demoIf :: Int -> String
demoIf code =
    if code == 10 then "Too hot - turn equipment off"
    else if code == 11 then "Caution - recheck in 5 minutes"
    else if code == 13 then "Turn on circulating fan"
    else "Normal mode of operation"

demoCase :: Int -> String
demoCase code = case code of
    10 -> "Too hot - turn equipment off"
    11 -> "Caution - recheck in 5 minutes"
    13 -> "Turn on circulating fan"
    _ -> "Normal mode of operation"

demoArgPattern :: Int -> String
demoArgPattern 10 = "Too hot - turn equipment off"
demoArgPattern 11 = "Caution - recheck in 5 minutes"
demoArgPattern 13 = "Turn on circulating fan"
demoArgPattern _ = "Normal mode of operation"

main :: IO ()
main = do
    putStrLn $ demoIf 10
    putStrLn $ demoIf 11
    putStrLn $ demoIf 13
    putStrLn $ demoIf 0

    putStrLn $ demoCase 10
    putStrLn $ demoCase 11
    putStrLn $ demoCase 13
    putStrLn $ demoCase 0

    putStrLn $ demoArgPattern 10
    putStrLn $ demoArgPattern 11
    putStrLn $ demoArgPattern 13
    putStrLn $ demoArgPattern 0
