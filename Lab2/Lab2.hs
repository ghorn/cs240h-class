-- Lab2.hs

{-# OPTIONS_GHC -Wall #-}

module Main where

import System.Environment(getArgs)
import Lab2.Rect

imageSize :: Num a => a
imageSize = 500

main :: IO ()
main = do
  args <- getArgs
  rects <- loadRects args
  
  drawRects rects imageSize
