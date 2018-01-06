{-|
Module      : EngProb.Util
Description : Utility functions
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This module defines assorted utility functions.
-}

module EngProb.Util
    ( expandRange
    , prompt
    ) where

-- |Display a prompt and strictly read a value from terminal
prompt :: Read a =>
    String -- ^ Prompt text
    -> IO a -- ^ Action
prompt s = do
    putStr s
    line <- getLine
    return $! read line -- Use '$!' to force evaluation read expression

-- Expand minimum/maximum range
expandRange ::
    Int                 -- ^ Iteration index
    -> Double           -- ^ Current minimum
    -> Double           -- ^ Current maximum
    -> Double           -- ^ New sample
    -> (Double, Double) -- ^ New minimum and maximum
expandRange i mn mx x
    | i == 0 = (x, x)
    | otherwise = (if x < mn then x else mn, if x > mx then x else mx)
