{-|
Module      : EngProb
Description : Support functions
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This modules defines assorted support functions.
-}

module EngProb
    ( prompt
    , readValue
    , readValueMaybe
    ) where

-- |Display a prompt and strictly read a value from terminal
prompt :: Read a =>
    String -- ^ Prompt text
    -> IO a -- ^ Action
prompt s = do
    putStr s
    line <- getLine
    return $! read line -- Use '$!' to force evaluation read expression

-- |Attempt to parse string
-- Simplifies the 'reads' API which only ever returns zero or one elements in practice
readValueMaybe :: Read a =>
    String -- ^ Input string
    -> Maybe (a, String) -- ^ Parse result
readValueMaybe s = case reads s of
    [(value, t)] -> Just (value, t)
    _ -> Nothing

-- |Parse string
-- Simplifies the 'reads' API which only ever returns zero or one elements in practice
readValue :: Read a =>
    String -- ^ Input string
    -> (a, String) -- ^ Parse result
readValue s = case readValueMaybe s of
    Just result -> result
    _ -> error "Parse failed"
