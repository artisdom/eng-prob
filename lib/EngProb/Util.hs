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
    ( prompt
    ) where

-- |Display a prompt and strictly read a value from terminal
prompt :: Read a =>
    String -- ^ Prompt text
    -> IO a -- ^ Action
prompt s = do
    putStr s
    line <- getLine
    return $! read line -- Use '$!' to force evaluation read expression
