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
    , foldM
    , foldl'
    , forM_
    , fromJust
    , getDataFileName
    , printf
    , runST
    , unfoldr
    , void
    , when
    ) where

import           Control.Monad
import           Control.Monad.ST
import           Data.List
import           Data.Maybe
import           Paths_eng_prob
import           System.Exit
import           Text.Printf