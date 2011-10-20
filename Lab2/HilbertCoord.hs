-- HilbertCoord.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.HilbertCoord( HilbertCoord(..)
                        ) where

data HilbertCoord = HilbertCoord { hcX :: Int
                                 , hcY :: Int
                                 , hcV :: Int
                                 } deriving Eq

instance Show HilbertCoord where
  show hc = show (hcX hc, hcY hc, hcV hc)
