-- Lab2.hs

{-# OPTIONS_GHC -Wall #-}

module Main where

import Lab2.LoadRects
import Lab2.ZNode(ZNode)
import Lab2.HilbertRTree

import System.IO
import System.Environment(getArgs)
import Control.Monad
import System.Exit

query :: ZNode -> String -> IO ()
query tree queryString = do
  let rect = stringToRect True (queryString, -1)
  putStrLn $ "   Searching for " ++ show rect
  let (numIntersect, rIntersect) = search tree rect
  if numIntersect > 0
    then do putStrLn $ "\n   Found " ++ show numIntersect ++ " matches. Here are some of them:"
            mapM_ (\x -> putStrLn $ "   "++show x) $ take 5 rIntersect
    else do putStrLn "\n   No matches found"


main :: IO ()
main = do
  args <- getArgs
  rects <- loadRects args
  
  tree <- makeNewTree rects
  forever $ do putStr ">> "
               hFlush stdout
               eof <- hIsEOF stdin
               if eof
                 then do exitSuccess
                 else do line <- getLine
                         putStrLn ""
                         query tree line
