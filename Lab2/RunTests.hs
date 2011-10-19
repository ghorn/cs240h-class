-- RunTests.hs

{-# OPTIONS_GHC -Wall #-}

module Main where

--import Lab2.HilbertCurve
import qualified Lab2.WikipediaHilbertCurve as W
import Test.QuickCheck


data Mytest = Mytest Int Int deriving Show

maxHilbertDim :: Int
maxHilbertDim = 5

instance Arbitrary Mytest where
  arbitrary = do
    n' <- arbitrary
    d' <- arbitrary
    let n = (mod n' maxHilbertDim) + 1
        d = mod d' (2^(n+1))
    return $ Mytest n d

t_d2xy_xy2d :: Mytest -> Bool
t_d2xy_xy2d (Mytest n d) = d == W.xy2d n (W.d2xy n d)

main :: IO ()
main = do
  quickCheck t_d2xy_xy2d
--  verboseCheck t_d2xy_xy2d
--  sample (arbitrary :: Gen Mytest)
  
