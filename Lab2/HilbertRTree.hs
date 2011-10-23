-- HilbertRTree.hs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Lab2.HilbertRTree(newTree
--                        , insert
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

--instance HasLhv Leaf where
--  lhv (Leaf node) = nodeLhv node
--instance HasLhv Nonleaf where
--  lhv (Nonleaf node) = nodeLhv node
--instance HasLhv Child where
--  lhv (CLeaf leaf) = lhv leaf
--  lhv (CNonleaf nonleaf) = lhv nonleaf


--instance HasMbr Nonleaf where
--  mbr (Nonleaf node) = nodeMbr node
--instance HasMbr Leaf where
--  mbr (Leaf node) = nodeMbr node
--instance HasMbr Child where
--  mbr (CLeaf leaf) = mbr leaf
--  mbr (CNonleaf nonleaf) = mbr nonleaf

class HasChildren a b where
--class HasChildren a b | a -> b where
  children :: a -> [b]
--  maxNumChildren :: a -> Int
  
--instance HasChildren Leaf HRect where
--  children (Leaf node) = nodeChildren node
--  maxNumChildren _ = cL

--instance HasChildren Nonleaf Child where
--  children (Nonleaf node) = nodeChildren node
--  maxNumChildren _ = cN

class HasLhv a where
  lhv :: a -> Int
instance HasLhv HRect where
  lhv (HRect h _) = h
instance HasLhv Leaf where
  lhv = lLhv
instance HasLhv (Nonleaf a) where
  lhv = nlLhv
  

class HasMbr a where
  mbr :: a -> Rect
instance HasMbr HRect where
  mbr (HRect _ r) = r
instance HasMbr Leaf where
  mbr = lMbr
instance HasMbr (Nonleaf a) where
  mbr = nlMbr


--instance Node Leaf HRect where
--  children = lHRects  
--  makeNode someHRects = Leaf { lLhv = maximum $ map lhv someHRects
--                             , lMbr = getMbrs $ map mbr someHRects
--                             , lHRects = someHRects
--                             }
--  maxNumChildren a b = cL
--instance Node (Nonleaf Leaf) where
--  children = lHRects
--  makeNode children' = Nonleaf { nlLhv = maximum $ map lhv children'
--                               , nlMbr = getMbrs $ map mbr children'
--                               , nlChildren = children'
--                               }
--  maxNumChildren a b = cN
--instance Node (Nonleaf (Nonleaf a)) where
--  children = lHRects
--  makeNode children' = Nonleaf { nlLhv = maximum $ map lhv children'
--                               , nlMbr = getMbrs $ map mbr children'
--                               , nlChildren = children'
--                               }
--  maxNumChildren a b = cN


insertByLhv :: HasLhv a => a -> [a] -> [a]
insertByLhv = insertBy (\x0 x1 -> compare (lhv x0) (lhv x1))

data HRect = HRect Int Rect deriving Show

data Leaf = Leaf { lLhv :: Int
                 , lMbr :: Rect
                 , lHRects :: [HRect]
                 } deriving Show

data Nonleaf a = Nonleaf { nlLhv :: Int
                         , nlMbr :: Rect
                         , nlChildren :: [a]
                         } deriving Show

data Root a = Root a deriving Show

--data (ZNode a) => ZNonleaf a = ZNonleaf { znonleafParent :: Maybe a
--                                        , znonleafSiblings :: ([Nonleaf], [Nonleaf])
--                                        , znonleafFocus :: Nonleaf
--                                        }
--data (ZNode a) => ZLeaf a = ZLeaf { zleafParent   :: a
--                                  , zleafSiblings :: ([Leaf], [Leaf])
--                                  , zleafFocus    :: Leaf
--                                  }

class (HasLhv a, HasMbr a, HasLhv b, HasMbr b) => Node a b where
--  children :: a -> [b]
  makeNode :: [b] -> a
--  maxNumChildren :: a -> b -> Int

--class ZNode a b c where
--  makeZNode       :: c -> ([b], [b]) -> b -> a
--  getParentZNode  :: a -> Maybe c
--  getSiblingNodes :: a -> ([b], [b])
--  getFocusNode    :: a -> b
--  getLhv          :: a -> Int
--  getMbr          :: a -> Rect
--  getChildren     :: a -> b
--  isLeaf          :: a -> Bool
--  isRoot          :: a -> Bool

class CanMakeZNode a b c where
  makeZNode       :: c -> ([b], [b]) -> b -> a
class CanGetParentZNode a c where
  getParentZNode  :: a -> Maybe c
class CanGetSiblingNodes a b where
  getSiblingNodes :: a -> ([b], [b])
class CanGetFocusNode a b where
  getFocusNode    :: a -> b
class CanGetLhv a where
  getLhv          :: a -> Int
class CanGetMbr a where
  getMbr          :: a -> Rect
class CanGetChildren a b where
  getChildren     :: a -> b
class CanIsLeaf a where
  isLeaf          :: a -> Bool
class CanIsRoot a where
  isRoot          :: a -> Bool

  
  
--getAllSiblings :: a -> [b]
getAllSiblings znode = lsibs ++ (self:rsibs)
  where
    (lsibs, rsibs) = getSiblingNodes znode
    self = getFocusNode znode
  
totalNumSiblings znode = length lsibs + length rsibs + 1
  where
    (lsibs, rsibs) = getSiblingNodes znode


modifyZNode znode newFocus = makeZNode (getParentZNode znode) (getSiblingNodes znode) newFocus


insertNodeIntoZNodeByLhv node znode = makeZNode (getParentZNode znode) ([], rsibs) self
  where
    self:rsibs = insertByLhv node (getAllSiblings znode)


--adjustTree :: ZNode -> Node -> ZNode
adjustTree znode newNode
  -- check for errors
  | totalNumSiblings znode > cN = error "error (adjustTree): too many siblings somehow"
  
  -- if there is an extra space, simply insert the node by lhv
  | totalNumSiblings znode < cN = zipupFull $ insertNodeIntoZNodeByLhv newNode znode
  
  -- otherwise call handleNodeOverflow
  | otherwise = handleNodeOverflow znode newNode
  
                
handleNodeOverflow znode newNode
  -- check for errors
  | newNumberOfSiblings > cN + 1 = error "error (handleNodeOverflow): too many siblings somehonw"
  | newNumberOfSiblings < cN     = error "error - handleNodeOverflow called when there is no overflow?"
  
  -- if it's possible to evenly redistribute siblings without splitting, do it
  | newNumberOfSiblings == cN = zipupFull $ makeZNode (getParentZNode znode) ([], bl1:bls) bl0
  
  -- if a split occurs, evenly redistribute all siblings
  -- then take the node with new smallest hilbert value and make it the child of a new parent node
  -- merge this new split parent node with the rebalanced node with adjustTree
  | otherwise = adjustTree (zipupOnce makeZNode (getParentZNode znode) ([], bls) bl1) bl0
  where
    allOldSiblings = getAllSiblings znode
    
    newNumberOfSiblings
      -- if any node has space, don't split
      | any (not . nodeFull) allOldSiblings = length allOldSiblings
      | otherwise                           = length allOldSiblings + 1

    -- nodes after children redistribution:
    rebalancedNodes@(bl0:bl1:bls) = map makeNode (evenlyDistribute newNumberOfSiblings newChildren)
      where
        newChildren = foldr insertByLhv oldChildren (children newNode)
        oldChildren = concat $ map children allOldSiblings


zipupFull = error "zip up full"


--zipupOnce :: (ZNode znode0 focus0 parent0) => znode0 -> parent0
zipupOnce selfZNode = makeZNode grandParentZNode auntiesUnclesNodes updatedParentNode
  where
    grandParentZNode   = getParentZNode $ getParentZNode selfZNode
    auntiesUnclesNodes = getSiblingNodes $ getParentZNode selfZNode
    updatedParentNode = makeZNode allSiblingNodes
      where
        allSiblingNodes = lsibsNodes ++ (selfNode:rsibsNodes)
          where
            selfNode = getFocusNode selfZNode
            (lsibsNodes, rsibsNodes) = getSiblingNodes selfZNode

fromRect :: Rect -> HRect
fromRect rect = HRect (xy2d hilbertDim (hx, hy)) rect
  where
    hx = rectMinX rect + ((rectMaxX rect - rectMinX rect) `div` 2)
    hy = rectMinY rect + ((rectMaxY rect - rectMinY rect) `div` 2)

--isFull :: Node a b => a -> Bool
nodeFull node
  | length (children node)  > cN = error "node somehow got too many entries, this is a bug"
  | length (children node) == cN = True
  | otherwise                    = False



chooseLeaf hrect znode 
  | isLeaf znode = znode
  | otherwise    = chooseLeaf hrect $ makeZNode znode (lsibs, rsibs) self
  where
    (lsibs, self, rsibs) = breakByLhv hrect $ getChildren znode

breakByLhv :: (HasLhv a, HasLhv b) => a -> [b] -> ([b], b, [b])
breakByLhv hrect y = case break (\x -> lhv x > lhv hrect) y
                     of ([], [])              -> error "can't breakByLhv on an empty list"
                        ([focus], [])         -> ([], focus, [])
                        (lsibsPlusFocus, [])  -> (init lsibsPlusFocus, last lsibsPlusFocus, [])
                        (lsibs, focus:rsibs)  -> (lsibs, focus, rsibs)


--insert :: Rect -> Root -> Root
insert rect root = insertInLeaf hrect (chooseLeaf hrect root)
  where
    hrect = fromRect rect

insertInLeaf hrect leaf 
  -- check for errors
  | length (getChildren leaf) > cL = assertIsLeaf `seq` error "error (insertInLeaf) - leaf overfull"
  -- if there is space for another hrect, insert it
  | length (getChildren leaf) < cL = assertIsLeaf `seq` zipupFull $ modifyZNode leaf newChildren
  -- if there is no more space, make a new leaf node and call adjustTree
  | otherwise = adjustTree leaf $ makeZNode (getParentZNode leaf) ([],[]) (makeLeaf [hrect])
  where
    newChildren = insertByLhv hrect (getChildren leaf)
    assertIsLeaf
      | isLeaf leaf = True
      | otherwise   = error "insertInLeaf called on nonleaf"

---- take in sorted hrects, return a leaf
makeLeaf :: [HRect] -> Leaf
makeLeaf someHRects = Leaf { lLhv = maximum $ map lhv someHRects
                           , lMbr = getMbrs $ map mbr someHRects
                           , lHRects = someHRects
                           }
--makeLeaf :: [HRect] -> Leaf
--makeLeaf = makeNode
--
--makeNonleaf :: (HasMbr a, HasLhv a) => [a] -> Nonleaf
--makeNonleaf = makeNode                        

--newTree :: [Rect] -> 
newTree [] = error "sorry, you have to insert something in me with non-zero length"
newTree (rect0:rects) = foldr insert firstRoot rects
  where
    firstRoot = makeZNode Nothing ([], []) (makeLeaf [fromRect rect0])


--zipup :: ZNonleaf -> Root
--zipup :: ZNode a => a -> b
--zipup (ZNonleaf (ZNode Nothing (lsibs, rsibs) focus)) = lsibs ++ (focus:rsibs)
--zipup z = zipup $ zipupOnce z
--zipup (ZNonleaf (ZNode Nothing (lsibs, rsibs) focus)) = lsibs ++ (focus:rsibs)
--zipup = undefined

-- take focus NonleafNode, return parent's ZNode updating new focus's mbr and lhv
--zipupOnce :: ZNonleaf -> ZNonleaf
--zipupOnce (ZNonleaf (ZNode Nothing _ _)) = error "this should probably return self"
--zipupOnce (ZNonleaf (ZNode (Just oldParentNode) (lsibs, rsibs) focus)) = newZNode
--  where
--    ZNonleaf (ZNode grandparent (parentLsibs, parentRsibs) _) = oldParentNode
--    newZNode = ZNonleaf (ZNode grandparent (parentLsibs, parentRsibs) newNonleaf)
--      where
--        newNonleaf = makeNonleaf allSiblings
--          where
--            allSiblings = lsibs ++ (focus:rsibs)

--zipupLeaf :: ZLeaf -> ZNonleaf
--zipupLeaf = zipupOnce
--zipupLeaf (ZLeaf (ZNode oldParentNode (lsibs, rsibs) focus)) = newZNode
--  where
--    ZNonleaf (ZNode grandparent (parentLsibs, parentRsibs) _) = oldParentNode
--    newZNode = ZNonleaf (ZNode grandparent (parentLsibs, parentRsibs) newNonleaf)
--      where
--        newNonleaf = makeNonleaf allSiblings
--          where
--            allSiblings = lsibs ++ (focus:rsibs)


--zipupNodeOnce :: ZLeaf -> ZNonleaf
--zipupNodeOnce (ZLeaf (ZNode oldParentNode (lsibs, rsibs) focus)) = newZNode
--zipupNodeOnce = zipupLeaf
--zipupNodeOnce znode = newZNode
--  where
--    (ZNode grandparent (parentLsibs, parentRsibs) _) = getParent znode
--    newZNode = ZNonleaf (ZNode grandparent (parentLsibs, parentRsibs) newNonleaf)
--      where
--        newNonleaf = makeNonleaf allSiblings
--          where
--            allSiblings = lsibs ++ (focus:rsibs)
--            (lsibs, rsibs) = getSiblings znode
--            focus = getFocus znode



--adjustTree :: ZNonleaf -> Nonleaf -> Root
--adjustTree n@(ZNonleaf (ZNode parent (lsibs, rsibs) self)) nn
--  -- if there was an extra space, simply insert the node
--  | newNumberOfNonleaves <  cN = zipup (ZNonleaf (ZNode parent ([], bl1:bls) bl0))
--  -- otherwise call handleNonleafOverflow
--  | newNumberOfNonleaves == cN = handleNonleafOverflow n nn
--  -- if a split caused an overflow, evenly redistribute all siblings
--  -- take the nonleaf with new smallest hilbert value and make it the child of a new nonnonleaf node
--  -- call adjustTree with this new nonnonleaf node
--  | otherwise = adjustTree (zipupOnce (ZNonleaf (ZNode parent ([], bls) bl1))) $ makeNonleaf [bl0]
--  where
--    allSiblings = lsibs ++ (self:rsibs)
--    newNumberOfNonleaves
--      -- if any nonleaf has space, don't add a node
--      | any (not . isFull) allSiblings = length allSiblings
--      | otherwise                      = length allSiblings + 1
--    
--    (bl0:bl1:bls)
--      -- if there was an extra space, simply insert the node
--      | newNumberOfNonleaves < cN = insertByLhv nn allSiblings
--      -- if there was not extra space, evenly redistribute all siblings
--      | otherwise                 = map makeNonleaf (evenlyDistribute newNumberOfNonleaves newChildren)
--    newChildren = foldr insertByLhv oldChildren (children nn)
--    oldChildren = concat $ map children allSiblings
    
    

--handleNonleafOverflow :: HasChildren a Child => ZNonleaf -> a -> Root
--handleNonleafOverflow :: ZNonleaf -> Nonleaf -> Root
--handleNonleafOverflow (ZNonleaf (ZNode parent (lsibs, rsibs) self)) nn
--  -- if there was space to redistribute, then do it
--  | newNumberOfNonleaves <= cN = zipup (ZNonleaf (ZNode parent ([], bl1:bls) bl0))
--  -- if there is no space left, make new node and evenly redistribute all siblings
--  -- take the node with new smallest hilbert value and make it the child of a new node
--  -- call adjustTree with this new node
--  | otherwise = adjustTree (zipupOnce (ZNonleaf (ZNode parent ([], bls) bl1))) $ makeNonleaf [bl0]
--  where
--    newNumberOfNonleaves
--      -- if any nonleaf has space, don't add a node
--      | any (not . isFull) allSiblings = length allSiblings
--      | otherwise                      = length allSiblings + 1
--
--    allSiblings = lsibs ++ (self:rsibs)
--    
--    (bl0:bl1:bls) = map makeNonleaf (evenlyDistribute newNumberOfNonleaves newChildren)
--    newChildren = foldr insertByLhv oldChildren (children nn)
--    oldChildren = concat $ map children allSiblings
--
--
----handleLeafOverflow :: ZLeaf -> HRect -> Root
--handleLeafOverflow (ZLeaf (ZNode parent (lsibs, rsibs) self)) hrect
--  -- if there is space to redistribute, then do it
--  | newNumberOfLeaves <= cN = zipup $ zipupLeaf (ZLeaf (ZNode parent ([], bl1:bls) bl0))
--  -- if there is no space left, make a new node and redistribute all siblings
--  -- take the node with new smallest hilbert value and make it the child of a new node
--  -- call adjustTree with this new nonleaf node
--  | otherwise = adjustTree (zipupLeaf (ZLeaf (ZNode parent ([], bls) bl1))) $ makeNonleaf [bl0]
--  where
--    -- if any leaf has space, redistribute evenly
--    newNumberOfLeaves
--      | any (not . isFull) allSiblings = length allSiblings
--      | otherwise                      = length allSiblings + 1
--    
--    allSiblings = lsibs ++ (self:rsibs)
--    (bl0:bl1:bls) = map makeLeaf (evenlyDistribute newNumberOfLeaves newHRects)
--    newHRects = insertByLhv hrect $ concat $ map children allSiblings
    


--insert :: Rect -> Root -> Root
--insert rect root = insertInLeaf hrect (chooseLeafFromRoot hrect root)
--  where
--    hrect = fromRect rect

----insertInLeaf :: HRect -> ZLeaf -> Root
--insertInLeaf hrect zleaf@(ZLeaf (ZNode parent (leftSiblings, rightSiblings) oldleaf))
--  | isFull oldleaf = handleLeafOverflow zleaf hrect
--  | otherwise      = zipup $ zipupLeaf $ ZLeaf (ZNode parent (leftSiblings, rightSiblings) newleaf)
--  where
--    newleaf = makeLeaf newHRects
--    newHRects = insertByLhv hrect (children oldleaf)
--
---- choose the best sibling from the root, construct the zipper, and call zChooseLeaf
----chooseLeafFromRoot :: HasLhv a => a -> Root -> ZLeaf
--chooseLeafFromRoot hrect (Root topSiblings) = chooseLeafFromZNonleaf hrect $ ZNonleaf (ZNode Nothing (lsibs, rsibs) focus)
--  where
--    (lsibs, focus, rsibs) = breaker hrect topSiblings
--    
--chooseLeafFromZNonleaf :: HasLhv a => a -> ZNonleaf -> ZLeaf
---- return on finding a leaf node
--chooseLeafFromZNonleaf = undefined
----chooseLeafFromZNonleaf _ (ZNonleaf (ZNode _ _ (Nonleaf Node {nodeChildren = []}))) = error "empty children yo"
----chooseLeafFromZNonleaf hrect searchme@(ZNonleaf (ZNode _ _ (Nonleaf Node {nodeChildren = ch@((CLeaf _):_)}))) = out
----  where
----    out = ZLeaf (ZNode searchme (lsibs, rsibs) focus)
----    (lsibs, focus, rsibs) = breaker hrect $ map getChild ch
----      where
----        getChild (CLeaf x) = x
----        getChild (CNonleaf _) = error "shouldn't ever be a nonleaf here"
------ normal search
----chooseLeafFromZNonleaf hrect searchme@(ZNonleaf (ZNode _ _ (Nonleaf Node {nodeChildren = ch@((CNonleaf _):_)}))) = out
----  where
----    out = chooseLeafFromZNonleaf hrect $ ZNonleaf (ZNode (Just searchme) (lsibs, rsibs) focus)
----    (lsibs, focus, rsibs) = breaker hrect $ map getChild ch
----      where
----        getChild (CNonleaf x) = x
----        getChild (CLeaf _) = error "shouldn't ever be a leaf here"

--breaker :: (HasLhv a, HasLhv b) => a -> [b] -> ([b], b, [b])
--breaker hrect y = chooseLeafSplit $ break (\x -> lhv x > lhv hrect) y
--  where
--    chooseLeafSplit :: ([a], [a]) -> ([a], a, [a])
--    chooseLeafSplit ([], []) = error "can't chooseLeaf on an empty list"
--    chooseLeafSplit ([focus'], []) = ([], focus', [])
--    chooseLeafSplit (lsibsPlusFocus, []) = (init lsibsPlusFocus, last lsibsPlusFocus, [])
--    chooseLeafSplit (lsibs', focus':rsibs') = (lsibs', focus', rsibs')
