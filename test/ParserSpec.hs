{-|
Module      : ParserSpec
Description : Tests for parsers
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module ParserSpec (spec) where

import           EngProb.Parser
                    ( Parser(..)
                    , Result(..)
                    , double
                    , int
                    , process
                    , processM
                    , processTill
                    , processTillM
                    , processWithLeadingCount
                    , processWithLeadingCountM
                    , tokenize
                    )
import           EngProb.Util (expandRange)
import           Test.Hspec

newtype Item = Item Double deriving (Eq, Show)

data Record = Record Double Int deriving (Eq, Show)

-- Data using header count
sensor1Dat :: String
sensor1Dat = "10\n\
                \0.0  132.5\n\
                \0.1  147.2\n\
                \0.2  148.3\n\
                \0.3  157.3\n\
                \0.4  163.2\n\
                \0.5  158.2\n\
                \0.6  169.3\n\
                \0.7  148.2\n\
                \0.8  137.6\n\
                \0.9  135.9\n"

-- Data using trailer record
sensor2Dat :: String
sensor2Dat = "0.0  132.5\n\
                \0.1  147.2\n\
                \0.2  148.3\n\
                \0.3  157.3\n\
                \0.4  163.2\n\
                \0.5  158.2\n\
                \0.6  169.3\n\
                \0.7  148.2\n\
                \0.8  137.6\n\
                \0.9  135.9\n\
                \-99  -99\n"

-- Data terminated by end of file
sensor3Dat :: String
sensor3Dat = "0.0  132.5\n\
                \0.1  147.2\n\
                \0.2  148.3\n\
                \0.3  157.3\n\
                \0.4  163.2\n\
                \0.5  158.2\n\
                \0.6  169.3\n\
                \0.7  148.2\n\
                \0.8  137.6\n\
                \0.9  135.9\n"

doublePair :: Parser (Double, Double)
doublePair = (,) <$> double <*> double

spec :: Spec
spec = do
    describe "Parser" $ do
        it "has sensible Functor instance" $ do
            let item = Item <$> double
                r = parse item ["1.0", "token"]
            r `shouldBe` Valid (Item 1.0) ["token"]
        it "has sensible Applicative instance" $ do
            let record = Record <$> double <*> int
                r = parse record ["1.0", "2", "token"]
            r `shouldBe` Valid (Record 1.0 2) ["token"]
        it "has sensible Monad instance" $ do
            let pair px py = px >>= \x -> py >>= \y -> return (x, y)
                r = parse (pair double double) ["1.0", "2.0", "token"]
            r `shouldBe` Valid (1.0, 2.0) ["token"]

    describe "tokenize" $ do
        it "yields all tokens" $ do
            let ts = tokenize sensor3Dat
            ts `shouldBe`
                [ "0.0", "132.5"
                , "0.1", "147.2"
                , "0.2", "148.3"
                , "0.3", "157.3"
                , "0.4", "163.2"
                , "0.5", "158.2"
                , "0.6", "169.3"
                , "0.7", "148.2"
                , "0.8", "137.6"
                , "0.9", "135.9"
                ]

    describe "processWithLeadingCount" $ do
        it "should accumulate values" $ do
            let ts = tokenize sensor1Dat
                result =
                    processWithLeadingCount doublePair ts (0.0, 0.0, 0)
                        (\(sx, sy, n) (x, y) -> (sx + x, sy + y, n + 1))
            result `shouldBe` Valid (4.5, 1497.7, 10 :: Int) []
        it "should get minimum and maximum" $ do
            let ts = tokenize "5 4.0 5.0 6.0 7.0 8.0 token0 token1"
                result =
                    processWithLeadingCount double ts (0.0, True, 0.0, 0.0, 0 :: Int)
                        (\(sx, isFirstIteration, mnx, mxx, n) x ->
                            let (mnx', mxx') = if isFirstIteration then (x, x) else (if x < mnx then x else mnx, if x > mxx then x else mxx)
                            in (sx + x, False, mnx', mxx', n + 1))
            result `shouldBe` Valid (30.0, False, 4.0, 8.0, 5) ["token0", "token1"]

    describe "processTill" $ do
        it "should accumulate values" $ do
            let ts = tokenize sensor2Dat
                result =
                    processTill (\ts' -> case ts' of "-99" : "-99" : t -> Just t; _ -> Nothing) doublePair ts (0.0, 0.0, 0 :: Int)
                        (\(sx, sy, n) (x, y) -> (sx + x, sy + y, n + 1))
            result `shouldBe` Valid (4.5, 1497.7, 10 :: Int) []

    describe "process" $ do
        it "should accumulate values" $ do
            let ts = tokenize sensor3Dat
                result =
                    process doublePair ts (0.0, 0.0, 0)
                        (\(sx, sy, n) (x, y) -> (sx + x, sy + y, n + 1))
            result `shouldBe` Valid (4.5, 1497.7, 10 :: Int) []

    describe "processWithLeadingCountM" $ do
        it "yields expected result" $ do
            result <-
                processWithLeadingCountM doublePair
                    (tokenize sensor1Dat)
                    (0, 0.0, 0.0, 0.0, []) $ \(i, sm, mn, mx, xs) (t, m) -> do
                        let (mn', mx') = expandRange i mn mx m
                        return (i + 1, sm + m, mn', mx', xs ++ [(t, m)]) -- Don't append like this in real use!
            result `shouldBe`
                    Right
                        ( (10, 1497.7, 132.5, 169.3,
                            [ (0.0, 132.5)
                            , (0.1, 147.2)
                            , (0.2, 148.3)
                            , (0.3, 157.3)
                            , (0.4, 163.2)
                            , (0.5, 158.2)
                            , (0.6, 169.3)
                            , (0.7, 148.2)
                            , (0.8, 137.6)
                            , (0.9, 135.9)
                            ]), [])
        it "yields expected result and remaining tokens" $ do
            result <-
                processWithLeadingCountM doublePair
                    (tokenize $ sensor1Dat ++ " token0 token1")
                    (0, 0.0, 0.0, 0.0, []) $ \(i, sm, mn, mx, xs) (t, m) -> do
                        let (mn', mx') = expandRange i mn mx m
                        return (i + 1, sm + m, mn', mx', xs ++ [(t, m)]) -- Don't append like this in real use!
            result `shouldBe`
                    Right
                        ( (10, 1497.7, 132.5, 169.3,
                            [ (0.0, 132.5)
                            , (0.1, 147.2)
                            , (0.2, 148.3)
                            , (0.3, 157.3)
                            , (0.4, 163.2)
                            , (0.5, 158.2)
                            , (0.6, 169.3)
                            , (0.7, 148.2)
                            , (0.8, 137.6)
                            , (0.9, 135.9)
                            ]), ["token0", "token1"])
        it "yields error on insufficient data" $ do
            result <-
                processWithLeadingCountM doublePair
                    (tokenize $ "2 1.0 2.0")
                    (0, 0.0, 0.0, 0.0, []) $ \(i, sm, mn, mx, xs) (t, m) -> do
                        let (mn', mx') = expandRange i mn mx m
                        return (i + 1, sm + m, mn', mx', xs ++ [(t, m)]) -- Don't append like this in real use!
            result `shouldBe` Left "end of input: expected 2 records"
        it "yields error on no count header" $ do
            result <-
                processWithLeadingCountM doublePair
                    (tokenize "")
                    (0, 0.0, 0.0, 0.0, []) $ \(i, sm, mn, mx, xs) (t, m) -> do
                        let (mn', mx') = expandRange i mn mx m
                        return (i + 1, sm + m, mn', mx', xs ++ [(t, m)]) -- Don't append like this in real use!
            result `shouldBe` Left "end of input: expected count header"
        it "yields error on invalid data" $ do
            result <-
                processWithLeadingCountM doublePair
                    (tokenize $ "1 1.0 two")
                    (0, 0.0, 0.0, 0.0, []) $ \(i, sm, mn, mx, xs) (t, m) -> do
                        let (mn', mx') = expandRange i mn mx m
                        return (i + 1, sm + m, mn', mx', xs ++ [(t, m)]) -- Don't append like this in real use!
            result `shouldBe` Left "failed to parse \"two\""

    describe "processTillM" $ do
        it "yields expected result" $ do
            result <-
                processTillM (\ts -> case ts of "-99" : "-99" : ts' -> Just ts'; _ -> Nothing) doublePair
                    (tokenize sensor2Dat)
                    (0, 0.0, 0.0, 0.0, []) $ \(i, sm, mn, mx, xs) (t, m) -> do
                        let (mn', mx') = expandRange i mn mx m
                        return (i + 1, sm + m, mn', mx', xs ++ [(t, m)]) -- Don't append like this in real use!
            result `shouldBe`
                    Right
                        ( (10, 1497.7, 132.5, 169.3,
                            [ (0.0, 132.5)
                            , (0.1, 147.2)
                            , (0.2, 148.3)
                            , (0.3, 157.3)
                            , (0.4, 163.2)
                            , (0.5, 158.2)
                            , (0.6, 169.3)
                            , (0.7, 148.2)
                            , (0.8, 137.6)
                            , (0.9, 135.9)
                            ]), [])
        it "yields expected result and remaining tokens" $ do
            result <-
                processTillM (\ts -> case ts of "-99" : "-99" : ts' -> Just ts'; _ -> Nothing) doublePair
                    (tokenize $ sensor2Dat ++ " token0 token1")
                    (0, 0.0, 0.0, 0.0, []) $ \(i, sm, mn, mx, xs) (t, m) -> do
                        let (mn', mx') = expandRange i mn mx m
                        return (i + 1, sm + m, mn', mx', xs ++ [(t, m)]) -- Don't append like this in real use!
            result `shouldBe`
                    Right
                        ( (10, 1497.7, 132.5, 169.3,
                            [ (0.0, 132.5)
                            , (0.1, 147.2)
                            , (0.2, 148.3)
                            , (0.3, 157.3)
                            , (0.4, 163.2)
                            , (0.5, 158.2)
                            , (0.6, 169.3)
                            , (0.7, 148.2)
                            , (0.8, 137.6)
                            , (0.9, 135.9)
                            ]), ["token0", "token1"])
        it "yields error on no trailer record" $ do
            result <-
                processTillM (\ts -> case ts of "-99" : "-99" : ts' -> Just ts'; _ -> Nothing) doublePair
                    (tokenize "1.0 2.0 3.0 4.0")
                    (0, 0.0, 0.0, 0.0, []) $ \(i, sm, mn, mx, xs) (t, m) -> do
                        let (mn', mx') = expandRange i mn mx m
                        return (i + 1, sm + m, mn', mx', xs ++ [(t, m)]) -- Don't append like this in real use!
            result `shouldBe` Left "end of input: terminating condition not satisfied"
        it "yields error on invalid data" $ do
            result <-
                processTillM (\ts -> case ts of "-99" : "-99" : ts' -> Just ts'; _ -> Nothing) doublePair
                    (tokenize $ "1.0 two")
                    (0, 0.0, 0.0, 0.0, []) $ \(i, sm, mn, mx, xs) (t, m) -> do
                        let (mn', mx') = expandRange i mn mx m
                        return (i + 1, sm + m, mn', mx', xs ++ [(t, m)]) -- Don't append like this in real use!
            result `shouldBe` Left "failed to parse \"two\""

    describe "processM" $ do
        it "yields expected result" $ do
            result <-
                processM doublePair
                    (tokenize sensor3Dat)
                    (0, 0.0, 0.0, 0.0, []) $ \(i, sm, mn, mx, xs) (t, m) -> do
                        let (mn', mx') = expandRange i mn mx m
                        return (i + 1, sm + m, mn', mx', xs ++ [(t, m)]) -- Don't append like this in real use!
            result `shouldBe`
                    Right
                        ( (10, 1497.7, 132.5, 169.3,
                            [ (0.0, 132.5)
                            , (0.1, 147.2)
                            , (0.2, 148.3)
                            , (0.3, 157.3)
                            , (0.4, 163.2)
                            , (0.5, 158.2)
                            , (0.6, 169.3)
                            , (0.7, 148.2)
                            , (0.8, 137.6)
                            , (0.9, 135.9)
                            ]), [])
        it "yields error on invalid data" $ do
            result <-
                processM doublePair
                    (tokenize $ sensor3Dat ++ " token0 token1")
                    (0, 0.0, 0.0, 0.0, []) $ \(i, sm, mn, mx, xs) (t, m) -> do
                        let (mn', mx') = expandRange i mn mx m
                        return (i + 1, sm + m, mn', mx', xs ++ [(t, m)]) -- Don't append like this in real use!
            result `shouldBe` Left "failed to parse \"token0\""
