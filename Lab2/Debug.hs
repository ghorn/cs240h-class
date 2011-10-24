-- Debug.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.Debug( dbgPutStrLn
                 , trace
                 ) where

import System.IO.Unsafe

debuggingInfoOn :: Bool
debuggingInfoOn = False

dbgPutStrLn :: String -> IO ()
dbgPutStrLn
  | debuggingInfoOn = putStrLn
  | otherwise       = (\_ -> return ())

trace :: String -> a -> a
trace msg x = blah `seq` x
  where
    blah = unsafePerformIO $ do
      dbgPutStrLn msg
