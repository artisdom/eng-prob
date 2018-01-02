{-|
Module      : Main
Description : 0020-p45-practice
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

Physical constants: Haskell has a preprocessor but values (which are always immutable in Haskell anyway) is the preferred way to define constants
-}

module Main (main) where

import Text.Printf (printf)

-- Speed of light, c
c :: Double
c = 2.99792 * 10 ** 8

-- Charge of an electron, e
e :: Double
e = 1.602177 * 10 ** (-19)

-- Avogadro's number, N_a
na :: Double
na = 6.022 * 10 ** 23

-- Acceleration of gravity, g (SI units)
gSI :: Double
gSI = 9.8

-- Acceleration of gravity, g (US customary system)
gCustomary :: Double
gCustomary = 32

-- Mass of the Earth, M_e
me :: Double
me = 5.98 * 10 ** 24

-- Radius of the Moon, r
r :: Double
r = 1.74 * 10 ** 6

main :: IO ()
main = do
    putStrLn $ printf "Speed of light, c = %g m/s" c
    putStrLn $ printf "Charge of an electron, e = %g C" e
    putStrLn $ printf "Avogadro's number, N_a = %g mol^(-1)" na
    putStrLn $ printf "Acceleration of gravity, g = %g m/s^2" gSI
    putStrLn $ printf "Acceleration of gravity, g = %g ft/s^2" gCustomary
    putStrLn $ printf "Mass of the Earth, M_e = %g kg" me
    putStrLn $ printf "Radius of the Moon, r = %g m" r
