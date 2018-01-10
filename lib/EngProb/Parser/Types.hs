{-|
Module      : EngProb.Parser.Types
Description : Types used to parse data files
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This module supplies types used to parse data files.
-}

module EngProb.Parser.Types
    ( Parser(..)
    , Result(..)
    , Token
    , TokenStream
    ) where

-- |A token to be parsed
type Token = String

-- |A stream of tokens to be parsed
type TokenStream = [Token]

-- |Result of parse operation
data Result a
    = Valid a TokenStream
    | Invalid String
    | EndOfInput
    deriving (Eq, Show)

-- |A parser
newtype Parser a = Parser { parse :: TokenStream -> Result a }

instance Functor Parser where
    fmap f o =
        Parser $ \ts ->
            case parse o ts of
                EndOfInput -> EndOfInput
                Invalid s -> Invalid s
                Valid x ts' -> Valid (f x) ts'

instance Applicative Parser where
    pure x = Parser $ Valid x
    o0 <*> o1 =
        Parser $ \ts ->
            case parse o0 ts of
                EndOfInput -> EndOfInput
                Invalid s -> Invalid s
                Valid f ts' ->
                    case parse o1 ts' of
                        EndOfInput -> EndOfInput
                        Invalid s -> Invalid s
                        Valid y ts'' -> Valid (f y) ts''

instance Monad Parser where
    o >>= f =
        Parser $ \ts ->
            case parse o ts of
                EndOfInput -> EndOfInput
                Invalid s -> Invalid s
                Valid x ts' -> parse (f x) ts'
