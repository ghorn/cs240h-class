-- Lab2.hs

{-# OPTIONS_GHC -Wall #-}

module Main where

import System.Environment(getArgs)
import Lab2.LoadRects
import Lab2.Rect
import Lab2.HilbertCurve(d2xy)
import Lab2.HilbertRTree

imageSize :: Num a => a
imageSize = 500

hilbertDim :: Int
hilbertDim = 16

toRect :: Int -> Rect
toRect hi = Rect { rectMinX = hx - 1
                 , rectMaxX = hx + 1
                 , rectMinY = hy - 1
                 , rectMaxY = hy + 1
                 }
  where
    (hx,hy) = d2xy hilbertDim hi


main :: IO ()
main = do
  args <- getArgs
  rects <- loadRects args
  
  let rectsIn = rects
--  let rectsIn = take 7 rects
--  let rectsIn = map toRect [1..10]
--  let rectsIn = take 7 rects
  let fullHRTree = newTree rectsIn
  print fullHRTree
  
----  drawRects rects imageSize
