-- DrawHilbertCurve.hs

{-# OPTIONS_GHC -Wall #-}

module Main where

import System.Environment(getArgs)
import Lab2.HilbertCurve

imageSize :: Num a => a
imageSize = 500

main :: IO ()
main = do
  args <- getArgs
  let nLevels
        | length(args) > 0 = read (head args)
        | otherwise        = 3 :: Int
  drawHilbertCurve imageSize $ makeHilbertCurve nLevels
