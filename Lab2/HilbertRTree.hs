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


data HRect = HRect Int Rect deriving Show
data Children = Leaves [Leaf] | Nonleaves [Nonleaf] deriving Show
data Leaf = Leaf { lLhv      :: Int
                 , lMbr      :: Rect
                 , hrects   :: [HRect]
                 } deriving Show
data Nonleaf = Nonleaf { nlLhv   :: Int
                       , nlMbr     :: Rect
                       , children  :: Children
                       } deriving Show
data Root = Root [Nonleaf] deriving Show

data ZNonleaf = ZNonleaf (Maybe ZNonleaf) ([Nonleaf], [Nonleaf]) Nonleaf
data ZLeaf = ZLeaf ZNonleaf ([Leaf], [Leaf]) Leaf

hilbertValue :: Rect -> Int
hilbertValue rect = xy2d hilbertDim (hx, hy)
  where
    hx = rectMinX rect + ((rectMaxX rect - rectMinX rect) `div` 2)
    hy = rectMinY rect + ((rectMaxY rect - rectMinY rect) `div` 2)

fromRect :: Rect -> HRect
fromRect rect = HRect (hilbertValue rect) rect

leafFull :: Leaf -> Bool
leafFull (Leaf {hrects = hrects'})
  | length hrects'  > cL = error "leaf somehow got too many entries, this is a bug"
  | length hrects' == cL = True
  | otherwise            = False

newHRTree :: [Rect] -> Root
newHRTree [] = error "sorry, you have to insert something in me with non-zero length"
newHRTree (rect0:rects) = foldr insert firstRoot rects
  where
    firstRoot = zipup $ ZNonleaf Nothing ([], []) firstNonleaf
    firstNonleaf = Nonleaf { nlLhv = lLhv firstLeaf
                           , nlMbr = lMbr firstLeaf
                           , children = Leaves [firstLeaf]
                           }
      where
        firstLeaf = Leaf { lLhv = hilbertValue rect0
                         , lMbr = rect0
                         , hrects = [fromRect rect0]
                         }

zipup :: ZNonleaf -> Root
zipup (ZNonleaf Nothing (lsibs, rsibs) focus) = Root $ lsibs ++ (focus:rsibs)
zipup z = zipup $ zipupOnce z
                                           
-- take focus NonleafNode, return parent's ZNode updating new focus's mbr and lhv
zipupOnce :: ZNonleaf -> ZNonleaf
zipupOnce (ZNonleaf Nothing _ _) = error "this should probably return self"
zipupOnce (ZNonleaf (Just oldParentNode) (lsibs, rsibs) focus) = newZNode
  where
    ZNonleaf grandparent (parentLsibs, parentRsibs) parentFocus = oldParentNode
    newZNode = ZNonleaf grandparent (parentLsibs, parentRsibs) newNonleaf
      where
        newNonleaf = Nonleaf { nlLhv      = newParentLhv `seq` newParentLhv
                             , nlMbr      = newParentMbr
                             , children   = Nonleaves allSiblings
                             }
          where
            allSiblings = lsibs ++ (focus:rsibs)
            newParentMbr = getMbr (nlMbr focus) (nlMbr parentFocus)
            newParentLhv
              | newParentLhvUnsafe /= nlLhv (last allSiblings) = error "hilbert out of order yo"
              | otherwise                                      = trace "hilbert in order yo" newParentLhvUnsafe
              where
                newParentLhvUnsafe = maximum $ map nlLhv allSiblings


zipupLeaf :: ZLeaf -> ZNonleaf
zipupLeaf (ZLeaf oldParentNode (lsibs, rsibs) focus) = newZNode
  where
    ZNonleaf grandparent (parentLsibs, parentRsibs) parentFocus = oldParentNode
    newZNode = ZNonleaf grandparent (parentLsibs, parentRsibs) newNonleaf
      where
        newNonleaf = Nonleaf { nlLhv      = newParentLhv `seq` newParentLhv
                             , nlMbr      = newParentMbr
                             , children   = Leaves allSiblings
                             }
          where
            allSiblings = lsibs ++ (focus:rsibs)
            newParentMbr = getMbr (lMbr focus) (nlMbr parentFocus)
            newParentLhv
              | newParentLhvUnsafe /= lLhv (last allSiblings) = error "hilbert out of order yo"
              | otherwise                                     = trace "hilbert in order yo" newParentLhvUnsafe
              where
                newParentLhvUnsafe = maximum $ map lLhv allSiblings

handleLeafOverflow :: Rect -> ZLeaf -> Root
handleLeafOverflow rect znode = error "handle overflow"

insert :: Rect -> Root -> Root
insert rect root = insertInLeaf rect (chooseLeafFromRoot rect root)

insertInLeaf :: Rect -> ZLeaf -> Root
insertInLeaf rect zleaf@(ZLeaf parent (leftSiblings, rightSiblings) oldleaf)
  | leafFull oldleaf = handleLeafOverflow rect zleaf
  | otherwise        = zipup $ zipupLeaf $ ZLeaf parent (leftSiblings, rightSiblings) newleaf
  where
    newleaf = Leaf { lLhv = maximum $ map (\(HRect h _) -> h) newHRects -- this could do a check
                   , lMbr = newMbr
                   , hrects = newHRects
                   }

    newHRects = insertBy (\(HRect h0 _) (HRect h1 _) -> compare h0 h1) (fromRect rect) (hrects oldleaf)
    newMbr = getMbr rect (lMbr oldleaf)

-- choose the best sibling from the root, construct the zipper, and call zChooseLeaf
chooseLeafFromRoot :: Rect -> Root -> ZLeaf
chooseLeafFromRoot rect (Root topSiblings) = chooseLeafFromZNonleaf rect $ ZNonleaf Nothing (lsibs, rsibs) focus
  where
    (lsibs, focus, rsibs) = chooseLeafSplit $ break (\x -> nlLhv x > hilbertValue rect) topSiblings
    
chooseLeafFromZNonleaf :: Rect -> ZNonleaf -> ZLeaf
-- return on finding a leaf node
chooseLeafFromZNonleaf rect searchme@(ZNonleaf _ _ (Nonleaf {children = (Leaves ch)})) = out
  where
    out = ZLeaf searchme (lsibs, rsibs) focus
    (lsibs, focus, rsibs) = chooseLeafSplit $ break (\x -> lLhv x > hilbertValue rect) ch
-- normal search
chooseLeafFromZNonleaf rect searchme@(ZNonleaf _ _ (Nonleaf {children = (Nonleaves ch)})) = out
  where
    out = chooseLeafFromZNonleaf rect $ ZNonleaf (Just searchme) (lsibs, rsibs) focus
    (lsibs, focus, rsibs) = chooseLeafSplit $ break (\x -> nlLhv x > hilbertValue rect) ch

chooseLeafSplit :: ([a], [a]) -> ([a], a, [a])
chooseLeafSplit ([], []) = error "can't chooseLeaf on an empty list"
chooseLeafSplit ([focus'], []) = ([], focus', [])
chooseLeafSplit (lsibsPlusFocus, []) = (init lsibsPlusFocus, last lsibsPlusFocus, [])
chooseLeafSplit (lsibs', focus':rsibs') = (lsibs', focus', rsibs')
