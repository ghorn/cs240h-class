-- HilbertRTree.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.HilbertRTree( newHRTree
                        , insert
                        ) where

import Lab2.Rect
import Lab2.HilbertCurve(xy2d)
import Data.List(insertBy)
import Debug.Trace

cL,cN :: Int
cL = 3
cN = 4

hilbertDim :: Int
hilbertDim = 16 -- for max value of 65535


newtype LHV = LHV Int deriving Show

data HRect = HRect Int Rect deriving Show

data Node = Leaf { lhv      :: Int
                 , mbr      :: Rect
                 , hrects   :: [HRect]
                 }
          | Nonleaf { lhv   :: Int
                    , mbr     :: Rect
                    , children  :: [Node]
                    } deriving Show

data Root = Root [Node] deriving Show

data ZNode = ZNode (Maybe ZNode) [Node] Node [Node] deriving Show

hilbertValue :: Rect -> Int
hilbertValue rect = xy2d hilbertDim (hx, hy)
  where
    hx = rectMinX rect + ((rectMaxX rect - rectMinX rect) `div` 2)
    hy = rectMinY rect + ((rectMaxY rect - rectMinY rect) `div` 2)

fromRect :: Rect -> HRect
fromRect rect = HRect (hilbertValue rect) rect

isFull :: Node -> Bool
isFull nl@(Nonleaf {}) = all isFull $ children nl
isFull (Leaf {hrects = hrects'})
  | length hrects'  > cL = error "leaf somehow got too many entries, this is a bug"
  | length hrects' == cL = True
  | otherwise            = False

newHRTree :: [Rect] -> Root
newHRTree [] = error "sorry, you have to insert something in me with non-zero length"
newHRTree (rect0:rects) = foldr (\x acc -> insert x acc) firstRoot rects
  where
    firstRoot = zipup $ ZNode Nothing [] firstLeaf []
    firstLeaf = Leaf { lhv = hilbertValue rect0
                     , mbr = rect0
                     , hrects = [fromRect rect0]
                     }
                                           
zipup :: ZNode -> Root
zipup (ZNode Nothing lsibs focus rsibs) = Root $ lsibs ++ (focus:rsibs)
zipup z = zipup $ zipupOnce z
                                           
-- take focus ZNode, return parent's ZNode updating new focus's mbr and lhv
zipupOnce :: ZNode -> ZNode
zipupOnce (ZNode Nothing _ _ _) = error "can't zip up nothing"
zipupOnce (ZNode (Just oldParentZNode) lsibs focus rsibs) = parentZipper
  where
    ZNode grandparent parentLsibs parentFocus parentRsibs = oldParentZNode
    parentZipper = ZNode grandparent parentLsibs newParent parentRsibs
      
      where
        newParent = Nonleaf { lhv      = newParentLhv `seq` newParentLhv
                            , mbr      = newParentMbr
                            , children = allSiblings
                            }
          where
            allSiblings = assertSameType allSiblingsUnsafe `seq` allSiblingsUnsafe
              where
                allSiblingsUnsafe = lsibs ++ (focus:rsibs)
                assertSameType []  = True
                assertSameType [_] = True
                assertSameType xs
                  | all (\x -> isLeaf (head xs) == isLeaf x) (tail xs) = True
                  | otherwise = error "children are not all same type"
                  where
                    isLeaf :: Node -> Bool
                    isLeaf (Leaf {})    = True
                    isLeaf (Nonleaf {}) = False
            newParentMbr = Rect { rectMinX = min (rectMinX $ mbr focus) (rectMinX $ mbr parentFocus)
                                , rectMinY = min (rectMinY $ mbr focus) (rectMinY $ mbr parentFocus)
                                , rectMaxX = max (rectMaxX $ mbr focus) (rectMaxX $ mbr parentFocus)
                                , rectMaxY = max (rectMaxY $ mbr focus) (rectMaxY $ mbr parentFocus)
                                }
            newParentLhv
              | newParentLhvUnsafe /= (lhv $ last allSiblings) = error "hilbert out of order yo"
              | otherwise                                      = trace "hilbert in order yo" newParentLhv
              where
                newParentLhvUnsafe = maximum $ map lhv allSiblings
                  
-- rect tried to insert in focus, but focus was full
handleOverflow :: Rect -> ZNode -> Root
handleOverflow rect znode = undefined

insert :: Rect -> Root -> Root
insert rect root = insertInLeaf rect (chooseLeaf rect root)

insertInLeaf :: Rect -> ZNode -> Root
insertInLeaf rect znode@(ZNode parent leftSiblings oldleaf rightSiblings)
  | isFull oldleaf = handleOverflow rect znode
  | otherwise      = zipup $ ZNode parent leftSiblings newleaf rightSiblings
  where
    newleaf = Leaf { lhv = maximum $ map (\(HRect h _) -> h) newHRects
                   , mbr = newMbr
                   , hrects = newHRects
                   }

    newHRects = insertBy (\(HRect h0 _) (HRect h1 _) -> compare h0 h1) (fromRect rect) (hrects oldleaf)
    newMbr = Rect { rectMinX = min (rectMinX rect) (rectMinX $ mbr oldleaf)
                  , rectMinY = min (rectMinY rect) (rectMinY $ mbr oldleaf)
                  , rectMaxX = max (rectMaxX rect) (rectMaxX $ mbr oldleaf)
                  , rectMaxY = max (rectMaxY rect) (rectMaxY $ mbr oldleaf)
                  }

-- choose the best sibling from the root, construct the zipper, and call zChooseLeaf
chooseLeaf :: Rect -> Root -> ZNode
chooseLeaf rect (Root topSiblings) = zChooseLeaf rect $ ZNode Nothing lsibs focus rsibs
  where
    (lsibs, focus, rsibs) = chooseLeafSplit $ break (\x -> lhv x > hilbertValue rect) topSiblings
    
zChooseLeaf :: Rect -> ZNode -> ZNode
-- return on finding a leaf
zChooseLeaf _ zNode@(ZNode _ _ (Leaf {}) _) = zNode
-- normal search
zChooseLeaf rect searchMe@(ZNode _ _ (Nonleaf {children = ch}) _) = ZNode (Just searchMe) lsibs focus rsibs
  where
    (lsibs, focus, rsibs) = chooseLeafSplit $ break (\x -> lhv x > hilbertValue rect) ch

chooseLeafSplit :: ([a], [a]) -> ([a], a, [a])
chooseLeafSplit ([], []) = error "can't chooseLeaf on an empty list"
chooseLeafSplit ([focus'], []) = ([], focus', [])
chooseLeafSplit (lsibsPlusFocus, []) = (init lsibsPlusFocus, last lsibsPlusFocus, [])
chooseLeafSplit (lsibs', focus':rsibs') = (lsibs', focus', rsibs')

