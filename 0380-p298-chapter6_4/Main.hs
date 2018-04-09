{-|
Module      : Main
Description : 0380-p298-chapter6_4
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program reads a seismic data file and then determines the times of possible seismic events.
-}

module Main (main) where

import           Control.Monad (replicateM)
import           Control.Monad.Trans.State.Lazy (State, evalState, get, put)
import           Data.Array ((!), listArray)
import qualified Data.Array (Array)
import           Data.Foldable (for_)
import           EngProb (getDataFileName, prompt, readDouble, readInt)
import           Text.Printf (printf)

type Array = Data.Array.Array Int Double

popWord :: State [String] String
popWord = do
    w : ws <- get
    put ws
    return w

readArray :: Int -> State [String] Array
readArray n = (listArray (0, n - 1) . map readDouble) <$> replicateM n popWord

fileName :: FilePath
fileName = "seismic.dat"

threshold :: Double
threshold = 1.5

main :: IO ()
main = do
    -- Read data file
    fn <- getDataFileName fileName
    content <- readFile fn
    let ws = words content
        (npts, timeIncr, sensor) = (flip evalState) ws $ do
                    n <- readInt <$> popWord
                    incr <- readDouble <$> popWord
                    xs <- readArray n
                    return $ (n, incr, xs)

    -- Read window sizes from the keyboard
    shortWindow <- prompt "Enter number of points for short window:\n"
    longWindow <- prompt "Enter number of points for long window:\n"

    print sensor
    let ks = [longWindow - 1 ..npts - 1]
    let shortPowers = map (powerW sensor shortWindow) ks
        longPowers = map (powerW sensor longWindow) ks
        ratios = (zipWith (/) shortPowers longPowers)
        times = map (\k -> fromIntegral k * timeIncr) ks
        ratiosWithTimes = filter (\(r, _) -> r > threshold) $ zip ratios times
    for_ ratiosWithTimes $ \(_, t) -> do
        putStrLn $ printf "Possible event at %f seconds" (t :: Double)

powerW :: Array -> Int -> Int -> Double
powerW xs shortWindow k =
    (sum $ map
        (\i -> let idx = k - i; x = xs ! idx in x * x)
        [0..shortWindow - 1]) / (fromIntegral shortWindow)
