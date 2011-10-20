-- HilbertRTree.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.HilbertRTree( emptyHRTree
                        , newHRTree
--                        , insert
                        ) where

import Lab2.Rect
import Lab2.HilbertCurve(xy2d)
import Data.List(foldl', insertBy)
import Debug.Trace

--cL,cN :: Int
--cL = 3
--cN = 4
hilbertDim :: Int
hilbertDim = 16 -- for max value of 65535

newtype LHV = LHV Int deriving Show

data HRect = HRect Int Rect deriving Show
fromRect :: Rect -> HRect
fromRect rect = HRect (hilbertValue rect) rect


data Node = Empty |
            NonLeaf { lhv   :: Int
                    , mbr   :: Rect
                    , nodes :: [Node]
                    } |
            Leaf { lhv :: Int
                 , hrects ::[HRect]
                 } deriving Show


hilbertValue :: Rect -> Int
hilbertValue rect = xy2d hilbertDim (hx, hy)
  where
    hx = rectMinX rect + ((rectMaxX rect - rectMinX rect) `div` 2)
    hy = rectMinY rect + ((rectMaxY rect - rectMinY rect) `div` 2)


newHRTree :: [Rect] -> Node
newHRTree = foldl' (\acc x -> insert x acc) emptyHRTree

emptyHRTree :: Node
emptyHRTree = Empty

insert :: Rect -> Node -> Node
insert rect Empty = Leaf {lhv = hilbertValue rect, hrects = [fromRect rect]}
insert rect nonleaf@(NonLeaf _ _ _) = insert rect (chooseLeaf rect nonleaf)
insert rect leaf = Leaf { lhv = maximum $ map (\(HRect h _) -> h) newHRects
                        , hrects = newHRects
                        }
  where
    newHRects = insertBy (\(HRect h0 _) (HRect h1 _) -> compare h0 h1) (fromRect rect) (hrects leaf)
    
---- returns the Node
chooseLeaf :: Rect -> Node -> Node
chooseLeaf _ leaf@(Leaf _ _) = leaf
chooseLeaf rect nonleaf 
  | length bestLeaves == 0 = trace "aha - length bestLeaves == 0" $ last (nodes nonleaf)
  | otherwise              = chooseLeaf rect $ head bestLeaves
  where
    bestLeaves = dropWhile (\x -> lhv x <= hilbertValue rect) (nodes nonleaf)
