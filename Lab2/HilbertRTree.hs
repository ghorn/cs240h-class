-- HilbertRTree.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Lab2.HilbertRTree( newHRTree
                        , insert
                        ) where

import Lab2.Rect
import Lab2.HilbertCurve(xy2d)
import Lab2.Misc(evenlyDistribute)
import Data.List(insertBy)

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
  lhv (Leaf node) = nodeLhv node
instance HasLhv Nonleaf where
  lhv (Nonleaf node) = nodeLhv node
instance HasLhv Child where
  lhv (CLeaf leaf) = lhv leaf
  lhv (CNonleaf nonleaf) = lhv nonleaf

class HasMbr a where
  mbr :: a -> Rect

instance HasMbr HRect where
  mbr (HRect _ r) = r
instance HasMbr Nonleaf where
  mbr (Nonleaf node) = nodeMbr node
instance HasMbr Leaf where
  mbr (Leaf node) = nodeMbr node
instance HasMbr Child where
  mbr (CLeaf leaf) = mbr leaf
  mbr (CNonleaf nonleaf) = mbr nonleaf

--class HasChildren a b where
class HasChildren a b | a -> b where
  children :: a -> [b]
  maxNumChildren :: a -> Int
  
instance HasChildren Leaf HRect where
  children (Leaf node) = nodeChildren node
  maxNumChildren _ = cL

instance HasChildren Nonleaf Child where
  children (Nonleaf node) = nodeChildren node
  maxNumChildren _ = cN

insertByLhv :: HasLhv a => a -> [a] -> [a]
insertByLhv = insertBy (\x0 x1 -> compare (lhv x0) (lhv x1))


data Node a = Node { nodeLhv      :: Int
                   , nodeMbr      :: Rect
                   , nodeChildren :: [a]
                   } deriving Show

data HRect = HRect Int Rect deriving Show
data Child = CLeaf Leaf | CNonleaf Nonleaf deriving Show

data Leaf = Leaf (Node HRect) deriving Show
data Nonleaf = Nonleaf (Node Child) deriving Show
data Root = Root [Nonleaf] deriving Show

--data ZNonleaf = ZNonleaf (Maybe ZNonleaf) ([Nonleaf], [Nonleaf]) Nonleaf
--data ZLeaf = ZLeaf ZNonleaf ([Leaf], [Leaf]) Leaf

data ZNode a b = ZNode b ([a], [a]) a
data ZLeaf = ZLeaf (ZNode Leaf ZNonleaf)
data ZNonleaf = ZNonleaf (ZNode Nonleaf (Maybe ZNonleaf))

class IsZNode a b c | a -> b c where
  getParent :: a -> c
  getSiblings :: a -> ([b], [b])
  getFocus :: a -> b
  
instance IsZNode ZLeaf Leaf ZNonleaf where
  getParent (ZLeaf (ZNode parent _ _)) = parent
  getSiblings (ZLeaf (ZNode _ sibs _)) = sibs
  getFocus (ZLeaf (ZNode _ _ focus)) = focus


fromRect :: Rect -> HRect
fromRect rect = HRect (xy2d hilbertDim (hx, hy)) rect
  where
    hx = rectMinX rect + ((rectMaxX rect - rectMinX rect) `div` 2)
    hy = rectMinY rect + ((rectMaxY rect - rectMinY rect) `div` 2)

-- take in sorted hrects, return a leaf
makeLeaf :: [HRect] -> Leaf
makeLeaf someHRects = Leaf $ Node { nodeLhv = maximum $ map lhv someHRects
                                  , nodeMbr = getMbrs $ map mbr someHRects
                                  , nodeChildren = someHRects
                                  }

makeNonleaf :: [Child] -> Nonleaf
makeNonleaf children' = Nonleaf $ Node { nodeLhv = maximum $ map lhv children'
                                       , nodeMbr = getMbrs $ map mbr children'
                                       , nodeChildren = children'
                                       }
                        
isFull :: HasChildren a b => a -> Bool
isFull node
  | length (children node)  > (maxNumChildren node) = error "node somehow got too many entries, this is a bug"
  | length (children node) == (maxNumChildren node) = True
  | otherwise                                       = False

newHRTree :: [Rect] -> Root
newHRTree [] = error "sorry, you have to insert something in me with non-zero length"
newHRTree (rect0:rects) = foldr insert firstRoot rects
  where
    firstRoot = zipup $ ZNonleaf (ZNode Nothing ([], []) firstNonleaf)
    firstNonleaf = makeNonleaf [CLeaf firstLeaf]
      where
        firstLeaf = makeLeaf [fromRect rect0]

zipup :: ZNonleaf -> Root
zipup (ZNonleaf (ZNode Nothing (lsibs, rsibs) focus)) = Root $ lsibs ++ (focus:rsibs)
zipup z = zipup $ zipupOnce z

-- take focus NonleafNode, return parent's ZNode updating new focus's mbr and lhv
zipupOnce :: ZNonleaf -> ZNonleaf
zipupOnce (ZNonleaf (ZNode Nothing _ _)) = error "this should probably return self"
zipupOnce (ZNonleaf (ZNode (Just oldParentNode) (lsibs, rsibs) focus)) = newZNode
  where
    ZNonleaf (ZNode grandparent (parentLsibs, parentRsibs) _) = oldParentNode
    newZNode = ZNonleaf (ZNode grandparent (parentLsibs, parentRsibs) newNonleaf)
      where
        newNonleaf = makeNonleaf $ map CNonleaf allSiblings
          where
            allSiblings = lsibs ++ (focus:rsibs)

zipupLeaf :: ZLeaf -> ZNonleaf
zipupLeaf (ZLeaf (ZNode oldParentNode (lsibs, rsibs) focus)) = newZNode
  where
    ZNonleaf (ZNode grandparent (parentLsibs, parentRsibs) _) = oldParentNode
    newZNode = ZNonleaf (ZNode grandparent (parentLsibs, parentRsibs) newNonleaf)
      where
        newNonleaf = makeNonleaf $ map CLeaf allSiblings
          where
            allSiblings = lsibs ++ (focus:rsibs)


--zipupNodeOnce :: ZLeaf -> ZNonleaf
--zipupNodeOnce (ZLeaf (ZNode oldParentNode (lsibs, rsibs) focus)) = newZNode
zipupNodeOnce znode = newZNode
  where
    (ZNode grandparent (parentLsibs, parentRsibs) _) = getParent znode
    newZNode = ZNonleaf (ZNode grandparent (parentLsibs, parentRsibs) newNonleaf)
      where
        newNonleaf = makeNonleaf $ map CLeaf allSiblings
          where
            allSiblings = lsibs ++ (focus:rsibs)
            (lsibs, rsibs) = getSiblings znode
            focus = getFocus znode


adjustTree :: ZNonleaf -> Nonleaf -> Root
adjustTree n@(ZNonleaf (ZNode parent (lsibs, rsibs) self)) nn
  -- if there was an extra space, simply insert the node
  | newNumberOfNonleaves <  cN = zipup (ZNonleaf (ZNode parent ([], bl1:bls) bl0))
  -- otherwise call handleNonleafOverflow
  | newNumberOfNonleaves == cN = handleNonleafOverflow n nn
  -- if a split caused an overflow, evenly redistribute all siblings
  -- take the nonleaf with new smallest hilbert value and make it the child of a new nonnonleaf node
  -- call adjustTree with this new nonnonleaf node
  | otherwise = adjustTree (zipupOnce (ZNonleaf (ZNode parent ([], bls) bl1))) $ makeNonleaf [CNonleaf bl0]
  where
    allSiblings = lsibs ++ (self:rsibs)
    newNumberOfNonleaves
      -- if any nonleaf has space, don't add a node
      | any (not . isFull) allSiblings = length allSiblings
      | otherwise                      = length allSiblings + 1
    
    (bl0:bl1:bls)
      -- if there was an extra space, simply insert the node
      | newNumberOfNonleaves < cN = insertByLhv nn allSiblings
      -- if there was not extra space, evenly redistribute all siblings
      | otherwise                 = map makeNonleaf (evenlyDistribute newNumberOfNonleaves newChildren)
    newChildren = foldr insertByLhv oldChildren (children nn)
    oldChildren = concat $ map children allSiblings
    
--handleNonleafOverflow :: HasChildren a Child => ZNonleaf -> a -> Root
--handleNonleafOverflow :: ZNonleaf -> Nonleaf -> Root
handleNonleafOverflow (ZNonleaf (ZNode parent (lsibs, rsibs) self)) nn
  -- if there was space to redistribute, then do it
  | newNumberOfNonleaves <= cN = zipup (ZNonleaf (ZNode parent ([], bl1:bls) bl0))
  -- if there is no space left, make new node and evenly redistribute all siblings
  -- take the node with new smallest hilbert value and make it the child of a new node
  -- call adjustTree with this new node
  | otherwise = adjustTree (zipupOnce (ZNonleaf (ZNode parent ([], bls) bl1))) $ makeNonleaf [CNonleaf bl0]
  where
    newNumberOfNonleaves
      -- if any nonleaf has space, don't add a node
      | any (not . isFull) allSiblings = length allSiblings
      | otherwise                      = length allSiblings + 1

    allSiblings = lsibs ++ (self:rsibs)
    
    (bl0:bl1:bls) = map makeNonleaf (evenlyDistribute newNumberOfNonleaves newChildren)
    newChildren = foldr insertByLhv oldChildren (children nn)
    oldChildren = concat $ map children allSiblings


handleLeafOverflow :: ZLeaf -> HRect -> Root
handleLeafOverflow (ZLeaf (ZNode parent (lsibs, rsibs) self)) hrect
  -- if there is space to redistribute, then do it
  | newNumberOfLeaves <= cN = zipup $ zipupLeaf (ZLeaf (ZNode parent ([], bl1:bls) bl0))
  -- if there is no space left, make a new node and redistribute all siblings
  -- take the node with new smallest hilbert value and make it the child of a new node
  -- call adjustTree with this new nonleaf node
  | otherwise = adjustTree (zipupLeaf (ZLeaf (ZNode parent ([], bls) bl1))) $ makeNonleaf [CLeaf bl0]
  where
    -- if any leaf has space, redistribute evenly
    newNumberOfLeaves
      | any (not . isFull) allSiblings = length allSiblings
      | otherwise                      = length allSiblings + 1
    
    allSiblings = lsibs ++ (self:rsibs)
    (bl0:bl1:bls) = map makeLeaf (evenlyDistribute newNumberOfLeaves newHRects)
    newHRects = insertByLhv hrect $ concat $ map children allSiblings
    


insert :: Rect -> Root -> Root
insert rect root = insertInLeaf hrect (chooseLeafFromRoot hrect root)
  where
    hrect = fromRect rect

insertInLeaf :: HRect -> ZLeaf -> Root
insertInLeaf hrect zleaf@(ZLeaf (ZNode parent (leftSiblings, rightSiblings) oldleaf))
  | isFull oldleaf = handleLeafOverflow zleaf hrect
  | otherwise      = zipup $ zipupLeaf $ ZLeaf (ZNode parent (leftSiblings, rightSiblings) newleaf)
  where
    newleaf = makeLeaf newHRects
    newHRects = insertByLhv hrect (children oldleaf)

-- choose the best sibling from the root, construct the zipper, and call zChooseLeaf
chooseLeafFromRoot :: HasLhv a => a -> Root -> ZLeaf
chooseLeafFromRoot hrect (Root topSiblings) = chooseLeafFromZNonleaf hrect $ ZNonleaf (ZNode Nothing (lsibs, rsibs) focus)
  where
    (lsibs, focus, rsibs) = breaker hrect topSiblings
    
chooseLeafFromZNonleaf :: HasLhv a => a -> ZNonleaf -> ZLeaf
-- return on finding a leaf node
chooseLeafFromZNonleaf _ (ZNonleaf (ZNode _ _ (Nonleaf Node {nodeChildren = []}))) = error "empty children yo"
chooseLeafFromZNonleaf hrect searchme@(ZNonleaf (ZNode _ _ (Nonleaf Node {nodeChildren = ch@((CLeaf _):_)}))) = out
  where
    out = ZLeaf (ZNode searchme (lsibs, rsibs) focus)
    (lsibs, focus, rsibs) = breaker hrect $ map getChild ch
      where
        getChild (CLeaf x) = x
        getChild (CNonleaf _) = error "shouldn't ever be a nonleaf here"
-- normal search
chooseLeafFromZNonleaf hrect searchme@(ZNonleaf (ZNode _ _ (Nonleaf Node {nodeChildren = ch@((CNonleaf _):_)}))) = out
  where
    out = chooseLeafFromZNonleaf hrect $ ZNonleaf (ZNode (Just searchme) (lsibs, rsibs) focus)
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
