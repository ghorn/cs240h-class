-- DrawSomeRects.hs

{-# OPTIONS_GHC -Wall #-}

module Main where

import Lab2.LoadRects
import Lab2.DrawRects
import Lab2.Config

import System.Environment(getArgs)

main :: IO ()
main = do
  args <- getArgs
  rects <- loadRects args
  drawRects rects imageSize
