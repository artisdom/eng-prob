{-|
Module      : Main
Description : 0260-p209-vectors
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

There are many Haskell packages that provide array-like data structures with efficient indexing etc. This program demonstrates the use of the @Vector@-family of data structures.
-}

module Main (main) where

import           Control.Monad.Extra (unfoldM)
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import           EngProb
import           Paths_eng_prob
import           Test.Hspec
import           Text.Read (readMaybe)

type Token = String
type TokenStream = [Token]

data Result a
    = Valid a
    | Invalid String
    | EndOfInput
    deriving (Eq, Show)

type Parser a = TokenStream -> Result (a, TokenStream)

instance Functor Result where
    fmap f (Valid a) = Valid (f a)
    fmap _ (Invalid s) = Invalid s
    fmap _ EndOfInput = EndOfInput

instance Applicative Result where
    pure = Valid
    (Valid f) <*> (Valid a) = Valid (f a)
    _ <*> (Invalid s) = Invalid s
    (Invalid s) <*> _ = Invalid s
    EndOfInput <*> _ = EndOfInput
    _ <*> EndOfInput = EndOfInput

instance Monad Result where
    ma >>= f =
        case ma of
            EndOfInput -> EndOfInput
            Invalid s -> Invalid s
            Valid a -> f a

failWith :: String -> Maybe a -> Result a
failWith message mb =
    case mb of
        Nothing -> Invalid message
        Just a -> Valid a

tillEndOfInput :: Parser a -> TokenStream -> Result (Maybe (a, TokenStream))
tillEndOfInput p ts =
    case p ts of
        EndOfInput -> Valid Nothing
        Invalid s -> Invalid s
        Valid (a, ts') -> Valid (Just (a, ts'))

spec :: Spec
spec =
    describe "Result" $ do
        it "has a sensible Functor instance" $
            (+10) <$> Valid 5 `shouldBe` Valid (15 :: Int)
        it "has a sensible Applicative instance" $
            Valid (+10) <*> Valid 5 `shouldBe` Valid (15 :: Int)
        it "has a sensible Monad instance" $
            (Valid 5 >>= \x -> return $ x + 10) `shouldBe` Valid (15 :: Int)

double :: Token -> Result Double
double s = failWith ("failed to parse " ++ show s) $ readMaybe s

coords :: Parser (Double, Double)
coords (tx : ty : ts) = do
    x <- double tx
    y <- double ty
    return $ ((x, y), ts)
coords (_ : _) = Invalid "not enough tokens"
coords _ = EndOfInput

main :: IO ()
main = do
    hspec spec

    let g :: Vector Double
        g = Vector.generate 21 $ \k -> fromIntegral k * 0.5
    print g

    fileName <- getDataFileName "sensor3.dat"
    stream <- readFile fileName

    let pairs =
            case unfoldM (tillEndOfInput coords) (words stream) of
                Valid x -> x
                Invalid s -> error s
                EndOfInput -> error "Logic error"
        (times', motions') = unzip pairs
        times = Vector.fromList times'
        motions = Vector.fromList motions'
    print times
    print motions
