-- HilbertRTree.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.HilbertRTree(newTree
--                        , insert
                        ) where

import Lab2.Rect
import Lab2.HilbertCurve(xy2d)
import Lab2.Misc(evenlyDistribute)
import Data.List(insertBy)
import Data.Maybe
import Debug.Trace

cL,cN :: Int
cL = 3
cN = 4

hilbertDim :: Int
hilbertDim = 16 -- for max value of 65535

insertNodeByLhv :: Node -> [Node] -> [Node]
insertNodeByLhv = insertBy (\x0 x1 -> compare (nodeLhv x0) (nodeLhv x1))

data HRect = HRect Int Rect deriving Show

data Node = Node { nodeLhv :: Int
                 , nodeMbr :: Rect
                 , nodeChildren :: [Node]
                 }
          | HRectChild HRect deriving Show

data ZNode = ZNode { znodeParent    :: Maybe ZNode
                   , znodeSiblings :: ([Node],[Node])
                   , znodeFocus    :: Node
                   } deriving Show
--getLhv :: ZNode -> Int
--getLhv znode = nodeLhv $ znodeFocus znode
--getMbr :: ZNode -> Rect
--getMbr znode = nodeMbr $ znodeFocus znode
getSiblingNodes :: ZNode -> ([Node], [Node])
getSiblingNodes = znodeSiblings
getParentZNode :: ZNode -> Maybe ZNode
getParentZNode = znodeParent
getFocusNode :: ZNode -> Node
getFocusNode = znodeFocus
makeZNode :: Maybe ZNode -> ([Node], [Node]) -> Node -> ZNode
makeZNode parent sibs focus = ZNode {znodeParent = parent, znodeSiblings = sibs, znodeFocus = focus}
getChildren :: ZNode -> [Node]
getChildren znode = nodeChildren $ znodeFocus znode

isLeaf :: ZNode -> Bool
isLeaf znode = isNodeLeaf $ znodeFocus znode
  where
    isNodeLeaf (HRectChild _) = error "uuugh this shouldn't happen"
    isNodeLeaf (Node {nodeChildren = [_]}) = error "fuuuuuuuu empty node children in isLeaf call"
    isNodeLeaf (Node {nodeChildren = ((HRectChild _):_)}) = True 
    isNodeLeaf _ = False

isRoot :: ZNode -> Bool
isRoot znode = isNothing (znodeParent znode)
  
getAllSiblings :: ZNode -> [Node]
getAllSiblings znode = lsibs ++ (self:rsibs)
  where
    (lsibs, rsibs) = getSiblingNodes znode
    self = getFocusNode znode
  
totalNumSiblings :: ZNode -> Int
totalNumSiblings znode = length lsibs + length rsibs + 1
  where
    (lsibs, rsibs) = getSiblingNodes znode

modifyZNode :: ZNode -> Node -> ZNode
modifyZNode znode newFocus = makeZNode (getParentZNode znode) (getSiblingNodes znode) newFocus


insertNodeIntoZNodeByLhv :: Node -> ZNode -> ZNode
insertNodeIntoZNodeByLhv node znode = makeZNode (getParentZNode znode) ([], rsibs) self
  where
    self:rsibs = insertNodeByLhv node (getAllSiblings znode)


adjustTree :: ZNode -> Node -> ZNode
adjustTree znode newNode
  -- check for errors
  | totalNumSiblings znode > cN = error "error (adjustTree): too many siblings somehow"
  
  -- if there is an extra space, simply insert the node by lhv
  | totalNumSiblings znode < cN = zipupFull $ insertNodeIntoZNodeByLhv newNode znode
  
  -- otherwise call handleNodeOverflow
  | otherwise = handleNodeOverflow znode newNode
  
                
handleNodeOverflow :: ZNode -> Node -> ZNode
handleNodeOverflow znode newNode
  -- check for errors
  | newNumberOfSiblings > cN + 1 = error "error (handleNodeOverflow): too many siblings somehonw"
  | newNumberOfSiblings < cN     = error "error - handleNodeOverflow called when there is no overflow?"
  
  -- if it's possible to evenly redistribute siblings without splitting, do it
  | newNumberOfSiblings == cN = zipupFull $ makeZNode (getParentZNode znode) ([], bl1:bls) bl0
  
  -- if a split occurs, evenly redistribute all siblings
  -- then take the node with new smallest hilbert value and make it the child of a new parent node
  -- merge this new split parent node with the rebalanced node with adjustTree
  | otherwise = adjustTree (zipupOnce $ makeZNode (getParentZNode znode) ([], bls) bl1) bl0
  where
    allOldSiblings = getAllSiblings znode
    
    newNumberOfSiblings
      -- if any node has space, don't split
      | any (not . nodeFull) allOldSiblings = length allOldSiblings
      | otherwise                           = length allOldSiblings + 1

    -- nodes after children redistribution:
    bl0:bl1:bls = map makeNode (evenlyDistribute newNumberOfSiblings newChildren)
      where
        newChildren = foldr insertNodeByLhv oldChildren (nodeChildren newNode)
        oldChildren = concat $ map nodeChildren allOldSiblings

zipupFull :: ZNode -> ZNode
zipupFull = error "zip up full"

zipupOnce :: ZNode -> ZNode
zipupOnce selfZNode 
  | isNothing $ getParentZNode selfZNode = trace "zipupOnce called on root, returning root" selfZNode
  | otherwise = makeZNode grandParentZNode auntiesUnclesNodes updatedParentNode
  where
    grandParentZNode   = getParentZNode  $ fromJust $ getParentZNode selfZNode
    auntiesUnclesNodes = getSiblingNodes $ fromJust $ getParentZNode selfZNode
    updatedParentNode = makeNode allSiblingNodes
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

nodeFull :: Node -> Bool
nodeFull node
  | length (nodeChildren node)  > cN = error "node somehow got too many entries, this is a bug"
  | length (nodeChildren node) == cN = True
  | otherwise                        = False


chooseLeaf :: HRect -> ZNode -> ZNode
chooseLeaf hrect znode 
  | isLeaf znode = znode
  | otherwise    = chooseLeaf hrect $ makeZNode (Just znode) (lsibs, rsibs) self
  where
    (lsibs, self, rsibs) = breakByLhv hrect $ getChildren znode

breakByLhv :: HRect -> [Node] -> ([Node], Node, [Node])
breakByLhv (HRect hrectLhv _) y = case break (\x -> nodeLhv x > hrectLhv) y
                     of ([], [])              -> error "can't breakByLhv on an empty list"
                        ([focus], [])         -> ([], focus, [])
                        (lsibsPlusFocus, [])  -> (init lsibsPlusFocus, last lsibsPlusFocus, [])
                        (lsibs, focus:rsibs)  -> (lsibs, focus, rsibs)


insert :: Rect -> ZNode -> ZNode
insert rect root = insertInLeaf (chooseLeaf hrect root) hrect
  where
    hrect = fromRect rect

insertInLeaf :: ZNode -> HRect -> ZNode
insertInLeaf leaf hrect
  -- check for errors
  | length (getChildren leaf) > cL = assertIsLeaf `seq` error "error (insertInLeaf) - leaf overfull"
  -- if there is space for another hrect, insert it
  | length (getChildren leaf) < cL = assertIsLeaf `seq` zipupFull $ modifyZNode leaf (makeNode newChildren)
  -- if there is no more space, make a new leaf node and call adjustTree
  | otherwise = assertIsLeaf `seq` adjustTree leaf $ makeNode [HRectChild hrect]
  where
    newChildren = insertNodeByLhv (HRectChild hrect) (getChildren leaf)
    assertIsLeaf
      | isLeaf leaf = True
      | otherwise   = error "insertInLeaf called on nonleaf"

---- take in sorted children nodes, return a node
makeNode :: [Node] -> Node
makeNode someChildren = Node { nodeLhv = maximum $ map nodeLhv someChildren
                             , nodeMbr = getMbrs $ map nodeMbr someChildren
                             , nodeChildren = someChildren
                             }


newTree :: [Rect] -> ZNode
newTree [] = error "sorry, you have to insert something in me with non-zero length"
newTree (rect0:rects) = foldr insert firstRoot rects
  where
    firstRoot = makeZNode Nothing ([], []) (makeNode [HRectChild $ fromRect rect0])


