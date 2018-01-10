{-|
Module      : EngProb.Parser.Funcs
Description : Functions used to parse data files
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This module supplies functions used to parse data files.
-}

module EngProb.Parser.Funcs
    ( double
    , int
    , process
    , processM
    , processTill
    , processTillM
    , processWithLeadingCount
    , processWithLeadingCountM
    , tokenize
    ) where

import           EngProb.Loop
                    ( abortWith
                    , breakWith
                    , continueWith
                    , loopM
                    , loopNM
                    )
import           EngProb.Parser.Types
                    ( Parser(..)
                    , Result(..)
                    , TokenStream
                    )
import           Text.Read (readMaybe)

-- |Tokenize string
tokenize ::
    String          -- ^ String
    -> TokenStream  -- ^ Stream of tokens
tokenize = words

primitive :: Read a => Parser a
primitive = Parser p
    where
        p (h : t) = case readMaybe h of
                            Nothing -> Invalid $ "failed to parse \"" ++ h ++ "\""
                            Just x -> Valid x t
        p _ = EndOfInput

-- |Parser for @Int@ type
int :: Parser Int
int = primitive

-- |Parser for @Double@ type
double :: Parser Double
double = primitive

-- |Process tokens with given accumulator function until given predicate is satisfied
processTill ::
    (TokenStream -> Maybe TokenStream)  -- ^ Predicate
    -> Parser a                         -- ^ Parser
    -> TokenStream                      -- ^ Tokens
    -> b                                -- ^ Initial accumulator value
    -> (b -> a -> b)                    -- ^ Accumulator function
    -> Result b                         -- ^ Result
processTill endFunc p ts acc f = case endFunc ts of
    Just ts' -> Valid acc ts'
    Nothing ->
        case parse p ts of
            EndOfInput -> EndOfInput
            Invalid s -> Invalid s
            Valid m ts' -> processTill endFunc p ts' (f acc m) f

-- |Process tokens with given accumulator function until end of stream
process ::
    Parser a            -- ^ Parser
    -> TokenStream      -- ^ Tokens
    -> b                -- ^ Initial accumulator value
    -> (b -> a -> b)    -- ^ Accumulator function
    -> Result b         -- ^ Result
process = processTill (\ts -> case ts of [] -> Just []; _ -> Nothing)

-- |Process tokens with given accumulator function using leading count header
processWithLeadingCount ::
    Parser a            -- ^ Parser
    -> TokenStream      -- ^ Tokens
    -> b                -- ^ Initial accumulator value
    -> (b -> a -> b)    -- ^ Accumulator function
    -> Result b         -- ^ Result
processWithLeadingCount p ts acc f =
    case parse int ts of
        EndOfInput -> EndOfInput
        Invalid s -> Invalid s
        Valid n ts' -> processWithLeadingCountHelper 0 n p ts' acc f

processWithLeadingCountHelper ::
    Int
    -> Int
    -> Parser a
    -> TokenStream
    -> b
    -> (b -> a -> b)
    -> Result b
processWithLeadingCountHelper i n p ts acc f
    | i == n = Valid acc ts
    | otherwise = case parse p ts of
                    EndOfInput -> EndOfInput
                    Invalid s -> Invalid s
                    Valid m ts' -> processWithLeadingCountHelper (i + 1) n p ts' (f acc m) f

-- |Process tokens with given accumulator action using leading count header in I/O
processWithLeadingCountM :: Monad m =>
    Parser a                                -- ^ Parser
    -> TokenStream                          -- ^ Tokens
    -> b                                    -- ^ Initial accumulator value
    -> (b -> a -> m b)                      -- ^ Action
    -> m (Either String (b, TokenStream))   -- ^ Result
processWithLeadingCountM p ts acc f =
    case parse int ts of
        EndOfInput -> return $ Left "end of input: expected count header"
        Invalid s -> return $ Left s
        Valid n ts' -> do
            result <- loopNM n (acc, ts') $ \temp@(acc', ts'') ->
                case parse p ts'' of
                    Valid x ts''' -> do y <- f acc' x; continueWith (y, ts''')
                    Invalid s -> abortWith s
                    EndOfInput -> breakWith temp
            return result

-- |Process tokens with given accumulator action in I/O until given condition is satisfied
processTillM :: Monad m =>
    (TokenStream -> Maybe TokenStream)      -- ^ Predicate
    -> Parser a                             -- ^ Parser
    -> TokenStream                          -- ^ Tokens
    -> b                                    -- ^ Initial accumulator value
    -> (b -> a -> m b)                      -- ^ Action
    -> m (Either String (b, TokenStream))   -- ^ Result
processTillM endFunc p ts acc f = do
    result <- loopM (acc, ts) $ \(acc', ts') ->
        case endFunc ts' of
            Just ts'' -> breakWith (acc', ts'')
            Nothing ->
                case parse p ts' of
                    Valid x ts'' -> do y <- f acc' x; continueWith (y, ts'')
                    Invalid s -> abortWith s
                    EndOfInput -> abortWith "end of input: terminating condition not satisfied"
    return result

-- |Process tokens with given accumulator action in I/O until end of stream
processM :: Monad m =>
    Parser a                                -- ^ Parser
    -> TokenStream                          -- ^ Tokens
    -> b                                    -- ^ Initial accumulator value
    -> (b -> a -> m b)                      -- ^ Action
    -> m (Either String (b, TokenStream))   -- ^ Result
processM p ts acc f = do
    result <- loopM (acc, ts) $ \temp@(acc', ts') ->
        case parse p ts' of
            Valid x ts'' -> do y <- f acc' x; continueWith (y, ts'')
            Invalid s -> abortWith s
            EndOfInput -> breakWith temp
    return result
