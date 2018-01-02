{-|
Module      : Main
Description : 0070-p79-limits
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program prints the system limitations.
-}

module Main (main) where

import Data.Int (Int8, Int16, Int32, Int64)
import Text.Printf (printf)

main :: IO ()
main = do
    -- Print integer type minima and maxima
    putStrLn $ printf "Int minimum: %d" (minBound :: Int)
    putStrLn $ printf "Int maximum: %d" (maxBound :: Int)
    putStrLn $ printf "Int8 minimum: %d" (minBound :: Int8)
    putStrLn $ printf "Int8 maximum: %d" (maxBound :: Int8)
    putStrLn $ printf "Int16 minimum: %d" (minBound :: Int16)
    putStrLn $ printf "Int16 maximum: %d" (maxBound :: Int16)
    putStrLn $ printf "Int32 minimum: %d" (minBound :: Int32)
    putStrLn $ printf "Int32 maximum: %d" (maxBound :: Int32)
    putStrLn $ printf "Int64 minimum: %d" (minBound :: Int64)
    putStrLn $ printf "Int64 maximum: %d" (maxBound :: Int64)

    let fRadix = floatRadix (0 :: Float)
        fDigits = floatDigits (0 :: Float)
        (fMinExp, fMaxExp) = floatRange (0 :: Float)
    putStrLn $ printf "FLT_RADIX=%d" fRadix
    putStrLn $ printf "FLT_MANT_DIG=%d" fDigits
    putStrLn $ printf "FLT_MIN_EXP=%d" fMinExp
    putStrLn $ printf "FLT_MAX_EXP=%d" fMaxExp

    let dRadix = floatRadix (0 :: Double)
        dDigits = floatDigits (0 :: Double)
        (dMinExp, dMaxExp) = floatRange (0 :: Double)
    putStrLn $ printf "DBL_RADIX=%d" dRadix
    putStrLn $ printf "DBL_MANT_DIG=%d" dDigits
    putStrLn $ printf "DBL_MIN_EXP=%d" dMinExp
    putStrLn $ printf "DBL_MAX_EXP=%d" dMaxExp
