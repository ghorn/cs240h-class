-- Config.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.Config( cL
                  , cN
                  , hilbertDim
                  , imageSize
                  ) where

hilbertDim :: Int
hilbertDim = 16 -- for max value of 65535

cL, cN :: Int
cL = 3
cN = 4

imageSize :: Num a => a
imageSize = 500
