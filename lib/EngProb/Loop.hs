{-|
Module      : EngProb.Loop
Description : Looping functions
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This module supplies functions for implementing loops.
-}

module EngProb.Loop
    ( IterationResult(..)
    , abortWith
    , breakWith
    , continueWith
    , loopM
    , loopNM
    ) where

-- |Iteration result
data IterationResult a
    = ContinueWith a    -- ^ Continue
    | BreakWith a       -- ^ Break
    | AbortWith String  -- ^ Abort
    deriving Show

-- |Continue iteration with specified accumulator value
continueWith :: Monad m =>
    a                           -- ^ Accumulator
    -> m (IterationResult a)    -- ^ Result
continueWith = return . ContinueWith

-- |Break iteration with specified accumulator value
breakWith :: Monad m =>
    a                           -- ^ Accumulator
    -> m (IterationResult a)    -- ^ Result
breakWith = return . BreakWith

-- |Abort iteration with specified error message
abortWith :: Monad m =>
    String                      -- ^ Error message
    -> m (IterationResult a)    -- ^ Result
abortWith = return . AbortWith

-- |Monadic fold from left to right supporting error case and early termination
loopFoldM :: Monad m =>
    (a -> b -> m (IterationResult a))   -- ^ Iteration function
    -> a                                -- ^ Start accumulator
    -> [b]                              -- ^ Input values
    -> m (IterationResult a)            -- ^ Result
loopFoldM _ a [] =  return $ BreakWith a
loopFoldM f a (x : xs) = do
    iterResult <- f a x
    case iterResult of
        ContinueWith a' -> loopFoldM f a' xs
        _ -> return iterResult

-- |Monadic loop supporting error case and early termination
loopM :: Monad m =>
    a                               -- ^ Start accumulator
    -> (a -> m (IterationResult a)) -- ^ Iteration function
    -> m (Either String a)          -- ^ Result
loopM a f = do
    result <- loopFoldM (\a' _ -> f a') a ([0..] :: [Int])
    return $ case result of
                ContinueWith a' -> Right a'
                BreakWith a' -> Right a'
                AbortWith s -> Left s

-- |Monadic @n@-iteration loop supporting error case
loopNM :: Monad m =>
    Int                             -- ^ Number of iterations
    -> a                            -- ^ Start accumulator
    -> (a -> m (IterationResult a)) -- ^ Iteration function
    -> m (Either String a)          -- ^ Result
loopNM n a f = do
    result <-
        loopFoldM
            (\a' i ->
                if i < n
                    then f a'
                    else do
                        iterResult <- f a'
                        case iterResult of
                            BreakWith _ -> abortWith ("end of input: expected " ++ show n ++ " records")
                            _ -> return iterResult)
            a
            ([1..n] :: [Int])
    return $ case result of
                ContinueWith a' -> Right a'
                BreakWith a' -> Right a'
                AbortWith s -> Left s

-- TODO: See -- https://stackoverflow.com/questions/24226074/terminating-a-monadic-fold-early
