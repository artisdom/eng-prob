{-|
Module      : Main
Description : 0000-p17-table-1-1-app
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module Main (main) where

main :: IO ()
main = do
    let diameter = 10
        area = pi * (diameter / 2) ** 2
    print (area :: Double)
