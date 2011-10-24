-- Utils.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.Utils( evenlyDistribute
                 , insertNodeByLhv
                 , insertHRectByLhv
                 , insertNodeIntoZNodeByLhv
                 ) where

import Lab2.Node
import Lab2.ZNode
import Lab2.Rect

import Data.List(insertBy)
import Data.Function(on)


---- take an int n and a list, break it into list of lists of n (last list may be shorter)
evenlyDistribute :: Show a => Int -> [a] -> [[a]]
evenlyDistribute _ [] = error "can't evenly distribute 0 items into anything"
evenlyDistribute 0 _ = error "can't evenly distribute into 0 bins"
evenlyDistribute n xs 
  | length xs < n = error "not enough items to evenly distribute"
  | otherwise = first:others rest
  where
    others [] = []
    others blah = evenlyDistribute (n-1) blah
    (first, rest) = splitAt nsplit xs
    nsplit = (length xs `div` n) + min (length xs `mod` n) 1


insertNodeByLhv :: Node -> [Node] -> [Node]
insertNodeByLhv = insertBy (compare `on` nodeLhv)

insertHRectByLhv :: HRect -> [HRect] -> [HRect]
insertHRectByLhv = insertBy (compare `on` (\(HRect h _) -> h))


insertNodeIntoZNodeByLhv :: Node -> ZNode -> ZNode
insertNodeIntoZNodeByLhv node znode = makeZNode (getParentZNode znode) ([], rsibs) self
  where
    self:rsibs = insertNodeByLhv node (getAllSiblings znode)

