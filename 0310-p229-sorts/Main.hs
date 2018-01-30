{-|
Module      : Main
Description : 0310-p229-sorts
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This program demonstrates sort.
-}

module Main (main) where

import           Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as Vector (fromList, thaw, unsafeFreeze)
import qualified Data.Vector.Unboxed.Mutable as MVector (length, read, write)
import           EngProb

sort :: (Ord a, Unbox a) => Vector a -> Vector a
sort xs = runST $ do
    mxs <- Vector.thaw xs
    let n = MVector.length mxs
    forM_ [0..n - 2] $ \k -> do
        m <- foldM
                (\m j -> do
                    xj <- MVector.read mxs j
                    xm <- MVector.read mxs m
                    return $ if xj < xm then j else m)
                k
                [k + 1..n - 1]
        hold <- MVector.read mxs m
        xk <- MVector.read mxs k
        MVector.write mxs m xk
        MVector.write mxs k hold
        return m
    Vector.unsafeFreeze mxs

main :: IO ()
main = print $ sort (Vector.fromList ([5, 3, 12, 8, 1, 9] :: [Double]))
