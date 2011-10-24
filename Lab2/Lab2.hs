-- Lab2.hs

{-# OPTIONS_GHC -Wall #-}

module Main where

import System.Environment(getArgs)
import Lab2.LoadRects
import Lab2.Rect
import Lab2.ZNode(ZNode)
import Lab2.HilbertRTree


query :: ZNode -> Rect -> IO ()
query tree rect = do
  let (numIntersect, rIntersect) = search tree rect
  if numIntersect > 0
    then do putStrLn $ "Found " ++ show numIntersect ++ " matches. Here are some of them:"
            mapM_ print $ take 5 rIntersect
    else do putStrLn "No matches found"
  

main :: IO ()
main = do
  args <- getArgs
  rects <- loadRects args
  
  tree <- makeNewTree rects
  query tree $ Rect { rectMinX = 424
                    , rectMaxX = 2433
                    , rectMinY = 234
                    , rectMaxY = 1233
                    }

--  drawRects rects imageSize
