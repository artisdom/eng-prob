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
    ) where

-- |Display a prompt and strictly read a value from terminal
prompt :: Read a => String -> IO a
prompt s = do
    putStr s
    line <- getLine
    return $! read line -- Use @$!@ to force evaluation read expression
