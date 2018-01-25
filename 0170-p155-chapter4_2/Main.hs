{-|
Module      : Main
Description : 0170-p155-chapter4_2
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program computes a linear model for a set of altitude and ozone mixing ratio values. A plot is also output to a PNG file.
-}

module Main (main) where

import           EngProb (prompt)
import           EngProb.Prelude
import           Graphics.Rendering.Chart.Backend.Cairo (toFile)
import           Graphics.Rendering.Chart.Easy
                    ( (.=)
                    , def
                    , layout_title
                    , line
                    , plot
                    )

-- |Evaluates the @sinc@ function
sinc :: Double -> Double
sinc x
    | abs x < 0.0001 = 1.0
    | otherwise = (sin x) / x

createPlot :: FilePath -> Double -> Double -> [(Double, Double)] -> IO ()
createPlot fileName a b dataPoints = toFile def fileName $ do
    layout_title .= printf "Interval [%f, %f]" a b
    plot $ line "sinc(x)" [ dataPoints ]

main :: IO ()
main = do
    -- Get interval endpoints from the user
    a <- prompt "Enter endpoint a: "
    b <- prompt "Enter endpoint b (b > a): "

    let x_incr = (b - a) / 20

    -- Compute table of sinc(x) values
    let dataPoints = map (\k -> let x = a + k * x_incr in (x, sinc x)) [0..20]

    -- Print table
    putStrLn "x and sinc(x)"
    forM_ dataPoints $ \(x, value) ->
        putStrLn $ printf "%f %f" x value

    -- Create plot
    createPlot "sinc.png" a b dataPoints
