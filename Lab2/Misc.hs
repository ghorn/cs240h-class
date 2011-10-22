-- Misc.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.Misc( evenlyDistribute
                ) where

---- take an int n and a list, break it into list of lists of n (last list may be shorter)
--groupsOf :: Int -> [a] -> [[a]]
--groupsOf _ [] = []
--groupsOf n xs = firstN:(groupsOf n remainder)
--  where
--    (firstN,remainder) = splitAt n xs    
evenlyDistribute :: Show a => Int -> [a] -> [[a]]
evenlyDistribute _ [] = error "can't evenly distribute 0 items into anything"
evenlyDistribute 0 _ = error "can't evenly distribute into 0 bins"
evenlyDistribute n xs 
  | length xs < n = error "not enough items to evenly distribute"
  | otherwise = first:(others rest)
  where
    others [] = []
    others blah = evenlyDistribute (n-1) blah
    (first, rest) = splitAt nsplit xs
    nsplit = (length xs `div` n) + min (length xs `mod` n) 1
