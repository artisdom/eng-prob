{-|
Module      : Main
Description : 0130-p125-chapter3_6
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program generates a summary report from a data file that has a trailer record with negative values.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Monad (forM_)
import           Data.Attoparsec.Text
                    ( Parser
                    , choice
                    , double
                    , parseOnly
                    , skipSpace
                    , string
                    , manyTill'
                    )
import           Data.List (foldl')
import           Data.Text (Text)
import qualified Data.Text.IO as Text (readFile)
import           EngProb (skipSpace1)
import           Paths_eng_prob (getDataFileName)
import           Text.Printf (printf)

-- Data type representing a (time, motion) pair from the data file
data TimeMotion = TimeMotion Double Double deriving Show

-- A parser for (time, motion) pairs
timeMotion :: Parser TimeMotion
timeMotion = TimeMotion <$> double <* skipSpace1 <*> double

untilTrailerLine :: Parser [TimeMotion]
untilTrailerLine = choice [none, oneOrMore]
    where
        trailer = string "-99" *> skipSpace1 *> string "-99" *> pure ()

        none = trailer *> pure []

        oneOrMore = do
            value <- timeMotion
            values <- manyTill' (skipSpace1 *> timeMotion) (skipSpace *> trailer)
            return $ value : values

parseDataText :: Text -> Either String [TimeMotion]
parseDataText = parseOnly untilTrailerLine

main :: IO ()
main = do
    -- Open file
    fileName <- getDataFileName "sensor2.dat"
    s <- Text.readFile fileName

    -- Parse data from file
    -- Use "!" (bang pattern) in order to error out early
    let !dataPoints = case parseDataText s of
                        Left m -> error $ "Parse failed: " ++ m
                        Right ps -> ps

    -- Get initial motion in order to compute minimum and maximum
    -- Use "!" (bang pattern) in order to error out early
    let !initMotion = case dataPoints of
                        [] -> error $ "No data in file " ++ fileName
                        (TimeMotion _ motion) : _ -> motion

    -- Read data and compute summary information
    let (n, sumMotion, minMotion, maxMotion) =
            foldl'
                (\(i, t, mn, mx) (TimeMotion _ motion) ->
                    let mn' = if motion < mn then motion else mn
                        mx' = if motion > mx then motion else mx
                    in (i + 1, t + motion, mn', mx'))
                (0, 0, initMotion, initMotion)
                dataPoints

    forM_ dataPoints $ \(TimeMotion time motion) ->
        putStrLn $ printf "%.1f %.1f" time motion

    putStrLn $ printf "Number of sensor readings: %d" (n :: Int)
    putStrLn $ printf "Average reading:           %.2f" (sumMotion / fromIntegral n)
    putStrLn $ printf "Maximum reading:           %.2f" maxMotion
    putStrLn $ printf "Minimum reading:           %.2f" minMotion
