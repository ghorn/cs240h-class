-- RunTests.hs

{-# OPTIONS_GHC -Wall #-}

module Main where

import Lab2.HilbertCurve
import qualified Lab2.SlowHilbertCurve as S
import qualified Lab2.WikipediaHilbertCurve as W
import Test.QuickCheck


maxHilbertDim :: Int
maxHilbertDim = 5

-- make a random hilbert dimension n and a random hilbert value within [0..2^(2*n)-1]
data HilbertVal = HilbertVal Int Int deriving Show
instance Arbitrary HilbertVal where
  arbitrary = do
    n' <- arbitrary
    d' <- arbitrary
    let n = mod n' maxHilbertDim + 1
        d = mod d' (2^(2*n))
    return $ HilbertVal n d

-- make a random hilbert dimension n and random (x, y) coordinates both within [0..2^n-1]
data HilbertXy = HilbertXy Int (Int, Int) deriving Show
instance Arbitrary HilbertXy where
  arbitrary = do
    n' <- arbitrary
    x' <- arbitrary
    y' <- arbitrary
    let n = mod n' maxHilbertDim + 1
        x = mod x' (2^n)
        y = mod y' (2^n)
    return $ HilbertXy n (x,y)

main :: IO ()
main = do
  putStr "make sure d2xy is the inverse of xy2d: "
  quickCheck $ \(HilbertVal n d) -> d == W.xy2d n (W.d2xy n d)
  
  putStr "make sure xy2d is the inverse of d2xy: "
  quickCheck $ \(HilbertXy n xy) -> xy == W.d2xy n (W.xy2d n xy)
                   
  putStr "make sure W.d2xy is the same as d2xy:  "
  quickCheck $ \(HilbertVal n d) -> W.d2xy n d == d2xy n d
  
  putStr "make sure W.xy2d is the same as xy2d:  "
  quickCheck $ \(HilbertXy n xy) -> W.xy2d n xy == xy2d n xy
  
  -- compare new and old makeHibertCurve implementations
  let testMakeHilberts nMax = all (\n -> S.makeHilbertCurve n == makeHilbertCurve n) [1..nMax]
  putStrLn $ "\nmakeHilbertCurve matches Slow.makeHilbertCurve?: " ++ show (testMakeHilberts 5)

--  sample (arbitrary :: Gen HilbertVal)
--  sample (arbitrary :: Gen HilbertXy)
  
