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
    -> m (IterationResult a)    -- ^ Result of iteration
continueWith = return . ContinueWith

-- |Break iteration with specified accumulator value
breakWith :: Monad m =>
    a                           -- ^ Accumulator
    -> m (IterationResult a)    -- ^ Result of iteration
breakWith = return . BreakWith

-- |Abort iteration with specified error message
abortWith :: Monad m =>
    String                      -- ^ Error message
    -> m (IterationResult a)    -- ^ Result of iteration
abortWith = return . AbortWith

-- |Monadic fold from left to right supporting error case and early termination
loopFoldM :: Monad m =>
    (a -> b -> m (IterationResult a))   -- ^ Iteration function
    -> a                                -- ^ Initial accumulator value
    -> [b]                              -- ^ Input values
    -> m (IterationResult a)            -- ^ Result
loopFoldM _ a [] =  return $ BreakWith a
loopFoldM f a (x : xs) = do
    iterResult <- f a x
    case iterResult of
        ContinueWith a' -> loopFoldM f a' xs
        _ -> return iterResult

-- |A monadic loop supporting an error case and early termination
-- The overall computation is considered to have succeeded if all iterations
-- return 'ContinueWith' or 'BreakWith' with an overall result of type 'a'.
-- In the case of 'ContinueWith', the iteration will continue; in the case
-- of 'BreakWith', the computation is considered to have terminated
-- successfully. 'AbortWith' can be returned by an individual iteration to
-- report an error along with a 'String' message which will terminate the
-- loop early.
loopM :: Monad m =>
    a                               -- ^ Initial accumulator value
    -> (a -> m (IterationResult a)) -- ^ Iteration function
    -> m (Either String a)          -- ^ Overall result of computation
loopM a f = do
    result <- loopFoldM (\a' _ -> f a') a ([0..] :: [Int])
    return $ case result of
                ContinueWith a' -> Right a'
                BreakWith a' -> Right a'
                AbortWith s -> Left s

-- |A monadic @n@-iteration loop supporting an error case
-- The overall computation is considered to have succeeded if @n@ iterations
-- can be performed. In all other cases, the computation is considered to
-- have failed.
loopNM :: Monad m =>
    Int                             -- ^ Number of iterations
    -> a                            -- ^ Initial accumulator value
    -> (a -> m (IterationResult a)) -- ^ Iteration function
    -> m (Either String a)          -- ^ Overall result of computation
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
