{-|
Module      : EngProbSpec
Description : Tests
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}

module EngProbSpec (spec) where

import           Data.Attoparsec.Text
import           EngProb
import           Test.Hspec

spec :: Spec
spec = do
    describe "skipSpace" $ do
        it "should handle zero spaces" $ do
            let Partial c = parse skipSpace ""
                Done i r = c ""
            i `shouldBe` ""
            r `shouldBe` ()
        it "should handle one spaces" $ do
            let Partial c = parse skipSpace " "
                Done i r = c ""
            i `shouldBe` ""
            r `shouldBe` ()
        it "should handle multiple spaces" $ do
            let Partial c = parse skipSpace "     "
                Done i r = c ""
            i `shouldBe` ""
            r `shouldBe` ()

    describe "skipSpace1" $ do
        it "should fail on zero spaces" $ do
            let Partial c = parse skipSpace1 ""
                Fail i _ _ = c ""
            i `shouldBe` ""
        it "should handle one spaces" $ do
            let Partial c = parse skipSpace1 " "
                Done i r = c ""
            i `shouldBe` ""
            r `shouldBe` ()
        it "should handle multiple spaces" $ do
            let Partial c = parse skipSpace1 "     "
                Done i r = c ""
            i `shouldBe` ""
            r `shouldBe` ()

    describe "countSepBy" $ do
        it "should parse zero items" $ do
            let Done i r = parse (countSepBy 0 double skipSpace1) ""
            i `shouldBe` ""
            r `shouldBe` []
        it "should parse one item" $ do
            let Partial c = parse (countSepBy 1 double skipSpace1) "1.0"
                Done i r = c ""
            i `shouldBe` ""
            r `shouldBe` [1.0]
        it "should parse multiple items" $ do
            let Partial c = parse (countSepBy 5 double skipSpace1) "1.0 2.0 3.0 4.0 5.0"
                Done i r = c ""
            i `shouldBe` ""
            r `shouldBe` [1.0, 2.0, 3.0, 4.0, 5.0]

    describe "listWithLeadingCount" $ do
        it "should parse zero items" $ do
            let Partial c = parse (listWithLeadingCount double skipSpace1) "0"
                Done i r = c ""
            i `shouldBe` ""
            r `shouldBe` []
        it "should parse one item" $ do
            let Partial c = parse (listWithLeadingCount double skipSpace1) "1 1.0"
                Done i r = c ""
            i `shouldBe` ""
            r `shouldBe` [1.0]
        it "should parse multiple items" $ do
            let Partial c = parse (listWithLeadingCount double skipSpace1) "5 1.0 2.0 3.0 4.0 5.0"
                Done i r = c ""
            i `shouldBe` ""
            r `shouldBe` [1.0, 2.0, 3.0, 4.0, 5.0]
