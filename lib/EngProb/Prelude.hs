{-|
Module      : EngProb.Prelude
Description : Re-exports of common functions
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This module imports and re-exports commonly used functions.
-}

module EngProb.Prelude
    ( exitFailure
    , exitSuccess
    , foldl'
    , forM_
    , printf
    , unfoldr
    , void
    ) where

import           Control.Monad
import           Data.List
import           System.Exit
import           Text.Printf