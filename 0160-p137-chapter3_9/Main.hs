{-|
Module      : Main
Description : 0160-p137-chapter3_9
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program computes a linear model for a set of altitude and ozone mixing ratio values.
-}

{-# LANGUAGE BangPatterns #-}

module Main (main) where

import           Data.Attoparsec.Text
                    ( Parser
                    , double
                    )
import           Data.Text (Text)
import           Data.Text.IO as Text (readFile)
import           EngProb
                    ( parseAll
                    , sepBySpaceUntilEndOfInput
                    , skipSpace1
                    )
import           Paths_eng_prob (getDataFileName)
import           Text.Printf (printf)

data Record = Record Double Double

record :: Parser Record
record = Record <$> double <* skipSpace1 <*> double

parseDataText :: Text -> Either String [Record]
parseDataText = parseAll (sepBySpaceUntilEndOfInput record)

main :: IO ()
main = do
    -- Open and read file
    fileName <- getDataFileName "zone1.dat"
    s <- Text.readFile fileName

    -- Parse pairs of values until end of input is reached
    let !dataPoints = case parseDataText s of
                        Left m -> error $ "Parse failed: " ++ m
                        Right ps -> ps

    -- Get initial x-values in order to compute minimum and maximum
    -- Use "!" (bang pattern) in order to error out early
    let !initX = case dataPoints of
                    [] -> error $ "No data in file " ++ fileName
                    (Record x _) : _ -> x

    -- Accumulate information
    let (sumx, sumy, sumx2, sumxy, count, minx, maxx) = foldr
                (\(Record x y) (sx, sy, sx2, sxy, count', mnx, mxx) ->
                    let mnx' = if x < mnx then x else mnx
                        mxx' = if x > mxx then x else mxx
                    in (sx + x, sy + y, sx2 + x * x, sxy + x * y, count' + 1, mnx', mxx'))
                (0, 0, 0, 0, 0, initX, initX)
                dataPoints
        denominator = sumx * sumx - count * sumx2
        m = (sumx * sumy - count * sumxy) / denominator
        b = (sumx * sumxy - sumx2 * sumy) / denominator

    -- Print summary of information
    putStrLn "Range of altitudes in km:"
    putStrLn $ printf "%.2f to %.2f\n" minx maxx
    putStrLn "Linear model:"
    putStrLn $ printf "ozone-mix-ratio = %.2f altitude + %.2f" m b
