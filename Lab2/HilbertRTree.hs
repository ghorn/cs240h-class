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

cL,cN :: Int
cL = 3
cN = 4

hilbertDim :: Int
hilbertDim = 16 -- for max value of 65535

newtype LHV = LHV Int deriving Show

data HRect = HRect Int Rect deriving Show
fromRect :: Rect -> HRect
fromRect rect = HRect (hilbertValue rect) rect

data Node = Root |
            NonLeaf { supertree :: [Node]
                    , lhv       :: Int
                    , mbr       :: Rect
                    , children  :: [Node]
                    } |
            Leaf { parent :: Node
                 , lhv    :: Int
                 , mbr    :: Rect
                 , hrects ::[HRect]
                 } deriving Show



hilbertValue :: Rect -> Int
hilbertValue rect = xy2d hilbertDim (hx, hy)
  where
    hx = rectMinX rect + ((rectMaxX rect - rectMinX rect) `div` 2)
    hy = rectMinY rect + ((rectMaxY rect - rectMinY rect) `div` 2)

emptyHRTree :: Node
emptyHRTree = Root

newHRTree :: [Rect] -> Node
newHRTree = foldl' (\acc x -> insert x acc) emptyHRTree

--makeBalancedNode :: [HRect] -> Node
--makeBalancedNode hrects = aNode
--  where
--    n = length hrects
--    while
--    logbase cN
    
--largestHilbertValue :: Node -> Int
--largestHilbertValue leaf@(Leaf {})

isFull :: Node -> Bool
isFull leaf@(Leaf {})
  | length (hrects leaf) > cL  = error "leaf somehow got too many entries, this is a bug"
  | length (hrects leaf) == cL = True
  | otherwise                  = False
isFull nonleaf@(NonLeaf {}) = all isFull $ children nonleaf
isFull (Root {}) = error "calling isFull on Root"


insert :: Rect -> Node -> Node
insert rect Root = Leaf {parent = Root, mbr = rect, lhv = hilbertValue rect, hrects = [fromRect rect]}
insert rect nonleaf@(NonLeaf {}) = insert rect (chooseLeaf rect nonleaf)
insert rect leaf@(Leaf {mbr = oldMbr})
  | isFull leaf = error "handleOverflow" -- handleOverflow $ parent leaf
  | otherwise = Leaf { parent = parent leaf
                     , mbr = newMbr
                     , lhv = maximum $ map (\(HRect h _) -> h) newHRects
                     , hrects = newHRects
                     }
  where
    newHRects = insertBy (\(HRect h0 _) (HRect h1 _) -> compare h0 h1) (fromRect rect) (hrects leaf)
    newMbr = Rect { rectMinX = min (rectMinX rect) (rectMinX oldMbr)
                  , rectMinY = min (rectMinY rect) (rectMinY oldMbr)
                  , rectMaxX = max (rectMaxX rect) (rectMaxX oldMbr)
                  , rectMaxY = max (rectMaxY rect) (rectMaxY oldMbr)
                  }
    
--handleOverflow :: Node -> Rect -> (Node, Maybe Node)
--handleOverflow parentNode rect = newParent


chooseLeaf :: Rect -> Node -> Node
chooseLeaf _ leaf@(Leaf {}) = leaf
chooseLeaf rect nonleaf 
  | length bestLeaves == 0 = trace "aha - length bestLeaves == 0" $ last (children nonleaf)
  | otherwise              = chooseLeaf rect $ head bestLeaves
  where
    bestLeaves = dropWhile (\x -> lhv x <= hilbertValue rect) (children nonleaf)
