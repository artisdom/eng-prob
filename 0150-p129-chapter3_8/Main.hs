{-|
Module      : Main
Description : 0150-p129-chapter3_8
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program generates a file of height and velocity values for a weather balloon. The information is also printed in a report and output to a PNG file.
-}

module Main (main) where

import           Control.Monad (forM_)
import           EngProb (prompt)
import           Graphics.Rendering.Chart.Easy
                    ( (.=)
                    , def
                    , layoutlr_title
                    , line
                    , plotLeft
                    , plotRight
                    )
import           Graphics.Rendering.Chart.Backend.Cairo (toFile)
import           System.IO
                    ( IOMode(..)
                    , hPutStrLn
                    , withFile
                    )
import           Text.Printf (printf)

type DataPoint = (Double, Double, Double)

createPlot :: FilePath -> [DataPoint] -> IO ()
createPlot fileName dataPoints = toFile def fileName $ do
    layoutlr_title .= "Weather balloon altitude and velocity"
    plotLeft $ line "Balloon Altitude (metres)"
                [ [ (d, h) | (d, h, _) <- dataPoints] ]
    plotRight $ line "Balloon Velocity (metres/s)"
                [ [ (d, v) | (d, _, v) <- dataPoints ] ]

main :: IO ()
main = do
    -- Get user input
    initial <- prompt "Enter initial value for table (in hours)\n"
    increment <- prompt "Enter increment between lines (in hours)\n"
    final <- prompt "Enter final value for table (in hours)\n"

    -- Print report heading
    putStrLn "\n\nWeather Balloon Information"
    putStrLn "Time   Height    Velocity"
    putStrLn "(hrs)  (metres)  (metres/s)"

    -- Compute and print report information and write data to a file
    let result = foldr (\time (mt, mh, items) ->
                    let height = (-0.12) * time ** 4
                                    + 12 * time ** 3
                                    + (-380) * time ** 2
                                    + 4100 * time
                                    + 220
                        velocity = (-0.48) * time ** 3
                                    + 36 * time ** 2
                                    + (-760) * time
                                    + 4100
                        (mt', mh') = if height > mh then (time, height) else (mt, mh)
                    in (mt', mh', (time, height, velocity) : items))
                    (0, 0, [])
                    [initial, initial + increment .. final]
        (maxTime, maxHeight, dataPoints) = result

    withFile "balloon.dat" WriteMode $ \h ->
        forM_ dataPoints $ \(time, height, velocity) -> do
            putStrLn $ printf "%6.2f  %8.2f   %7.2f" time height (velocity / 3600)
            hPutStrLn h $ printf "%.2f %.2f %.2f" time height (velocity / 3600)

    -- Print maximum height and corresponding time
    putStrLn $ printf "\nMaximum balloon height was %8.2f metres" maxHeight
    putStrLn $ printf "and it occurred at %6.2f hours" maxTime

    createPlot "../balloon.png" dataPoints
