-- HilbertRTree.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.HilbertRTree( newHRTree
                        , insert
                        ) where

import Lab2.Rect
import Lab2.HilbertCurve(xy2d)
import Lab2.Misc(evenlyDistribute)
import Data.List(insertBy)
import Debug.Trace

cL,cN :: Int
cL = 3
cN = 4

hilbertDim :: Int
hilbertDim = 16 -- for max value of 65535

class HasLhv a where
  lhv :: a -> Int
instance HasLhv HRect where
  lhv (HRect h _) = h
instance HasLhv Leaf where
  lhv = lLhv
instance HasLhv Nonleaf where
  lhv = nlLhv
instance HasLhv Child where
  lhv (CLeaf leaf) = lLhv leaf
  lhv (CNonleaf nonleaf) = nlLhv nonleaf

class HasMbr a where
  mbr :: a -> Rect
instance HasMbr HRect where
  mbr (HRect _ r) = r
instance HasMbr Nonleaf where
  mbr = nlMbr
instance HasMbr Leaf where
  mbr = lMbr
instance HasMbr Child where
  mbr (CLeaf leaf) = mbr leaf
  mbr (CNonleaf nonleaf) = mbr nonleaf

insertByLhv :: HasLhv a => a -> [a] -> [a]
insertByLhv = insertBy (\x0 x1 -> compare (lhv x0) (lhv x1))


data HRect = HRect Int Rect deriving Show
data Child = CLeaf Leaf | CNonleaf Nonleaf deriving Show

data Leaf = Leaf { lLhv      :: Int
                 , lMbr      :: Rect
                 , hrects   :: [HRect]
                 } deriving Show
data Nonleaf = Nonleaf { nlLhv   :: Int
                       , nlMbr     :: Rect
                       , nlChildren  :: [Child]
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

-- take in sorted hrects, return a leaf
makeLeaf :: [HRect] -> Leaf
makeLeaf someHRects = Leaf { lLhv = maximum $ map lhv someHRects
                           , lMbr = getMbrs $ map mbr someHRects
                           , hrects = someHRects
                           }

makeNonleaf :: [Child] -> Nonleaf
makeNonleaf children' = Nonleaf { nlLhv = maximum $ map lhv children'
                                , nlMbr = getMbrs $ map mbr children'
                                , nlChildren = children'
                                }

leafFull :: Leaf -> Bool
leafFull (Leaf {hrects = hrects'})
  | length hrects'  > cL = error "leaf somehow got too many entries, this is a bug"
  | length hrects' == cL = True
  | otherwise            = False

nonleafFull :: Nonleaf -> Bool
nonleafFull (Nonleaf {nlChildren = children'})
  | length children'  > cN = error "nonleaf somehow got too many children, this is a bug"
  | length children' == cN = True
  | otherwise              = False

newHRTree :: [Rect] -> Root
newHRTree [] = error "sorry, you have to insert something in me with non-zero length"
newHRTree (rect0:rects) = foldr insert firstRoot rects
  where
    firstRoot = zipup $ ZNonleaf Nothing ([], []) firstNonleaf
    firstNonleaf = makeNonleaf [CLeaf firstLeaf]
      where
        firstLeaf = makeLeaf [fromRect rect0]

zipup :: ZNonleaf -> Root
zipup (ZNonleaf Nothing (lsibs, rsibs) focus) = Root $ lsibs ++ (focus:rsibs)
zipup z = zipup $ zipupOnce z
                                           
-- take focus NonleafNode, return parent's ZNode updating new focus's mbr and lhv
zipupOnce :: ZNonleaf -> ZNonleaf
zipupOnce (ZNonleaf Nothing _ _) = error "this should probably return self"
zipupOnce (ZNonleaf (Just oldParentNode) (lsibs, rsibs) focus) = newZNode
  where
    ZNonleaf grandparent (parentLsibs, parentRsibs) _ = oldParentNode
    newZNode = ZNonleaf grandparent (parentLsibs, parentRsibs) newNonleaf
      where
        newNonleaf = Nonleaf { nlLhv      = newParentLhv `seq` newParentLhv
                             , nlMbr      = getMbrs $ (map nlMbr allSiblings)
                             , nlChildren = map CNonleaf allSiblings
                             }
          where
            allSiblings = lsibs ++ (focus:rsibs)
            newParentLhv
              | newParentLhvUnsafe /= nlLhv (last allSiblings) = error "hilbert out of order yo"
              | otherwise                                      = trace "hilbert in order yo" newParentLhvUnsafe
              where
                newParentLhvUnsafe = maximum $ map nlLhv allSiblings


zipupLeaf :: ZLeaf -> ZNonleaf
zipupLeaf (ZLeaf oldParentNode (lsibs, rsibs) focus) = newZNode
  where
    ZNonleaf grandparent (parentLsibs, parentRsibs) _ = oldParentNode
    newZNode = ZNonleaf grandparent (parentLsibs, parentRsibs) newNonleaf
      where
        newNonleaf = Nonleaf { nlLhv      = newParentLhv `seq` newParentLhv
                             , nlMbr      = getMbrs $ (map lMbr allSiblings)
                             , nlChildren = map CLeaf allSiblings
                             }
          where
            allSiblings = lsibs ++ (focus:rsibs)
            newParentLhv
              | newParentLhvUnsafe /= lLhv (last allSiblings) = error "hilbert out of order yo"
              | otherwise                                     = trace "hilbert in order yo" newParentLhvUnsafe
              where
                newParentLhvUnsafe = maximum $ map lLhv allSiblings


handleNonleafOverflow :: ZNonleaf -> Nonleaf -> Root
handleNonleafOverflow (ZNonleaf parent (lsibs, rsibs) self) nn
  -- if there was space to redistribute, then do it
  | newNumberOfNonleaves <= cN = zipup (ZNonleaf parent ([], bl1:bls) bl0)
  -- if there is no space left, make new node and evenly redistribute all siblings
  -- take the node with new smallest hilbert value and make it the child of a new node
  -- call adjustTree with this new node
  | otherwise = adjustTree (zipupOnce (ZNonleaf parent ([], bls) bl1)) Nonleaf { nlLhv = nlLhv bl0
                                                                               , nlMbr = nlMbr bl0
                                                                               , nlChildren = [CNonleaf bl0]
                                                                               }
  where
    newNumberOfNonleaves
      -- if any nonleaf has space, don't add a node
      | any (not . nonleafFull) allSiblings = length allSiblings
      | otherwise                           = length allSiblings + 1

    allSiblings = lsibs ++ (self:rsibs)
    
    (bl0:bl1:bls) = map makeNonleaf (evenlyDistribute newNumberOfNonleaves newChildren)
    newChildren :: [Child]
    newChildren = foldr insertByLhv oldChildren (nlChildren nn)
    oldChildren :: [Child]
    oldChildren = concat $ map nlChildren allSiblings


adjustTree :: ZNonleaf -> Nonleaf -> Root
adjustTree n@(ZNonleaf parent (lsibs, rsibs) self) nn
  -- if there was an extra space, simply insert the node
  | newNumberOfNonleaves <  cN = zipup (ZNonleaf parent ([], bl1:bls) bl0)
  -- otherwise call handleNonleafOverflow
  | newNumberOfNonleaves == cN = handleNonleafOverflow n nn
  -- if a split caused an overflow, evenly redistribute all siblings
  -- take the nonleaf with new smallest hilbert value and make it the child of a new nonnonleaf node
  -- call adjustTree with this new nonnonleaf node
  | otherwise = adjustTree (zipupOnce (ZNonleaf parent ([], bls) bl1)) Nonleaf { nlLhv = nlLhv bl0
                                                                               , nlMbr = nlMbr bl0
                                                                               , nlChildren = [CNonleaf bl0]
                                                                               }
  where
    allSiblings = lsibs ++ (self:rsibs)
    newNumberOfNonleaves
      -- if any nonleaf has space, don't add a node
      | any (not . nonleafFull) allSiblings = length allSiblings
      | otherwise                           = length allSiblings + 1
    
    (bl0:bl1:bls)
      -- if there was an extra space, simply insert the node
      | newNumberOfNonleaves < cN = insertByLhv nn allSiblings
      -- if there was not extra space, evenly redistribute all siblings
      | otherwise                 = map makeNonleaf (evenlyDistribute newNumberOfNonleaves newChildren)
    newChildren :: [Child]
    newChildren = foldr insertByLhv oldChildren (nlChildren nn)
    oldChildren :: [Child]
    oldChildren = concat $ map nlChildren allSiblings
    
handleLeafOverflow :: ZLeaf -> HRect -> Root
handleLeafOverflow (ZLeaf parent (lsibs, rsibs) self) hrect
  -- if no split caused a overflow, evenly redistribute all children by hilbert value
  | newNumberOfLeaves <= cN = zipup $ zipupLeaf (ZLeaf parent ([], bl1:bls) bl0)
  -- if a node split caused an overflow evenly redistribute all children by hilbet value
  -- take the leaf with new smallest hilbert value and make it the child of a new nonleaf node
  -- call adjustTree with this new nonleaf node
  | otherwise = adjustTree (zipupLeaf (ZLeaf parent ([], bls) bl1)) Nonleaf { nlLhv = lLhv bl0
                                                                            , nlMbr = lMbr bl0
                                                                            , nlChildren = [CLeaf bl0]
                                                                            }
  where
    -- if any leaf has space, redistribute evenly
    newNumberOfLeaves
      | any (not . leafFull) allSiblings = length allSiblings
      | otherwise                        = length allSiblings + 1
    
    allSiblings = lsibs ++ (self:rsibs)
    (bl0:bl1:bls) = map makeLeaf (evenlyDistribute newNumberOfLeaves newHRects)
    newHRects = insertByLhv hrect $ concat $ map hrects allSiblings
    

insert :: Rect -> Root -> Root
insert rect root = insertInLeaf hrect (chooseLeafFromRoot hrect root)
  where
    hrect = fromRect rect

insertInLeaf :: HRect -> ZLeaf -> Root
insertInLeaf hrect zleaf@(ZLeaf parent (leftSiblings, rightSiblings) oldleaf)
  | leafFull oldleaf = handleLeafOverflow zleaf hrect
  | otherwise        = zipup $ zipupLeaf $ ZLeaf parent (leftSiblings, rightSiblings) newleaf
  where
    newleaf = makeLeaf newHRects
    newHRects = insertByLhv hrect (hrects oldleaf)

-- choose the best sibling from the root, construct the zipper, and call zChooseLeaf
chooseLeafFromRoot :: HRect -> Root -> ZLeaf
chooseLeafFromRoot hrect (Root topSiblings) = chooseLeafFromZNonleaf hrect $ ZNonleaf Nothing (lsibs, rsibs) focus
  where
    (lsibs, focus, rsibs) = breaker hrect topSiblings
    
chooseLeafFromZNonleaf :: HRect -> ZNonleaf -> ZLeaf
-- return on finding a leaf node
chooseLeafFromZNonleaf _ (ZNonleaf _ _ (Nonleaf {nlChildren = []})) = error "empty children yo"
chooseLeafFromZNonleaf hrect searchme@(ZNonleaf _ _ (Nonleaf {nlChildren = ch@((CLeaf _):_)})) = out
  where
    out = ZLeaf searchme (lsibs, rsibs) focus
    (lsibs, focus, rsibs) = breaker hrect $ map getChild ch
      where
        getChild (CLeaf x) = x
        getChild (CNonleaf _) = error "shouldn't ever be a nonleaf here"
-- normal search
chooseLeafFromZNonleaf hrect searchme@(ZNonleaf _ _ (Nonleaf {nlChildren = ch@((CNonleaf _):_)})) = out
  where
    out = chooseLeafFromZNonleaf hrect $ ZNonleaf (Just searchme) (lsibs, rsibs) focus
    (lsibs, focus, rsibs) = breaker hrect $ map getChild ch
      where
        getChild (CNonleaf x) = x
        getChild (CLeaf _) = error "shouldn't ever be a leaf here"

breaker :: (HasLhv a, HasLhv b) => a -> [b] -> ([b], b, [b])
breaker hrect y = chooseLeafSplit $ break (\x -> lhv x > lhv hrect) y
  where
    chooseLeafSplit :: ([a], [a]) -> ([a], a, [a])
    chooseLeafSplit ([], []) = error "can't chooseLeaf on an empty list"
    chooseLeafSplit ([focus'], []) = ([], focus', [])
    chooseLeafSplit (lsibsPlusFocus, []) = (init lsibsPlusFocus, last lsibsPlusFocus, [])
    chooseLeafSplit (lsibs', focus':rsibs') = (lsibs', focus', rsibs')
