-- HilbertRTree.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.HilbertRTree( newTree
                        , insert
                        ) where

import Lab2.Rect
import Lab2.HilbertCurve(xy2d)
import Lab2.Misc(evenlyDistribute)
import Data.List(insertBy)
import Data.Function(on)
import Data.Maybe
import Debug.Trace

cL,cN :: Int
cL = 3
cN = 4

hilbertDim :: Int
hilbertDim = 16 -- for max value of 65535

insertNodeByLhv :: Node -> [Node] -> [Node]
insertNodeByLhv = insertBy (compare `on` getNodeLhv)

data HRect = HRect Int Rect deriving Show

data Node = Node { nodeLhv :: Int
                 , nodeMbr :: Rect
                 , nodeChildren :: [Node]
                 }
          | HRectChild HRect -- deriving Show
instance Show Node where
  show (HRectChild _) = " HRect"
  show (Node nlhv nmbr nch) = "{lhv:" ++ show nlhv ++ ", mbr:" ++ show nmbr ++ ", children:" ++ show nch ++ "}"

getNodeLhv :: Node -> Int
getNodeLhv (Node {nodeLhv = ret}) = ret
getNodeLhv (HRectChild (HRect ret _)) = ret

getNodeMbr :: Node -> Rect
getNodeMbr (Node {nodeMbr = ret}) = ret
getNodeMbr (HRectChild (HRect _ ret)) = ret

data ZNode = ZNode { znodeParent    :: Maybe ZNode
                   , znodeSiblings :: ([Node],[Node])
                   , znodeFocus    :: Node
                   } deriving Show

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
    isNodeLeaf (Node {nodeChildren = []}) = error "fuuuuuuuu empty node children in isLeaf call"
    isNodeLeaf (Node {nodeChildren = (HRectChild _:_)}) = True 
    isNodeLeaf _ = False

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
modifyZNode znode = makeZNode (getParentZNode znode) (getSiblingNodes znode)


insertNodeIntoZNodeByLhv :: Node -> ZNode -> ZNode
insertNodeIntoZNodeByLhv node znode = makeZNode (getParentZNode znode) ([], rsibs) self
  where
    self:rsibs = insertNodeByLhv node (getAllSiblings znode)


adjustTree :: ZNode -> Node -> ZNode
adjustTree znode newNode
  -- check for errors
  | totalNumSiblings znode > cN = error "error (adjustTree): too many siblings somehow"
  
  -- if there is an extra space, simply insert the node by lhv
  | totalNumSiblings znode < cN = zipupFull $ insertNodeIntoZNodeByLhv newNode safeZNode
  
  -- otherwise call handleNodeOverflow
  | otherwise = handleNodeOverflow safeZNode newNode
  where
    safeZNode
      | isNothing $ getParentZNode znode = showAdjustTreeSplit znode
      | otherwise = znode
                
handleNodeOverflow :: ZNode -> Node -> ZNode
handleNodeOverflow znode newNode
  -- check for errors
  | newNumberOfSiblings > cN + 1 = error "error (handleNodeOverflow): too many siblings somehonw"
  | newNumberOfSiblings < cN     = error "error - handleNodeOverflow called when there is no overflow?"
  
  -- if it's possible to evenly redistribute siblings without splitting, do it
  | newNumberOfSiblings == cN = showHandleNodeOverflowNotSplitting $ zipupFull $ makeZNode (getParentZNode znode) ([], bl1:bls) bl0
  
  -- if a split occurs, evenly redistribute all siblings
  -- then take the node with new smallest hilbert value and make it the child of a new parent node
  -- merge this new split parent node with the rebalanced node with adjustTree
  | otherwise = showHandleNodeOverflowSplitting adjustTree (zipupOnce $ makeZNode (getParentZNode znode) ([], bls) bl1) bl0
  where
    allOldSiblings = getAllSiblings znode

    newNumberOfSiblings
      -- if any node has space, don't split
      | any (not . nodeFull) allOldSiblings = length allOldSiblings
      | otherwise                           = length allOldSiblings + 1
      where
        nodeFull :: Node -> Bool
        nodeFull node
          | length (nodeChildren node)  > cN = error "node somehow got too many entries, this is a bug"
          | length (nodeChildren node) == cN = True
          | otherwise                        = False
    
    -- nodes after children redistribution:
    bl0:bl1:bls = map makeNode (evenlyDistribute newNumberOfSiblings newChildren)
      where
        newChildren = foldr insertNodeByLhv oldChildren (nodeChildren newNode)
        oldChildren = concatMap nodeChildren allOldSiblings


zipupFull :: ZNode -> ZNode
zipupFull znode
  -- keep calling zipupOnce until the top is reached
  | isJust (znodeParent znode) = zipupFull $ zipupOnce znode
  -- if root hasn't split, return root
  -- if root has split, make old root and siblings the children of a new root
  | otherwise = case (length (getAllSiblings znode)) of 1 -> znode
                                                        _ -> newRoot
  where
    newRoot = showRootSplit $ makeZNode Nothing ([], []) $ makeNode (getAllSiblings znode)

zipupOnce :: ZNode -> ZNode
zipupOnce selfZNode 
  | isNothing $ getParentZNode selfZNode = showZipupOnceCalledOnRoot selfZNode
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


chooseLeaf :: HRect -> ZNode -> ZNode
chooseLeaf hrect znode 
  | isLeaf znode = znode
  | otherwise    = chooseLeaf hrect $ makeZNode (Just znode) (lsibs, rsibs) self
  where
    (lsibs, self, rsibs) = breakByLhv hrect $ getChildren znode

breakByLhv :: HRect -> [Node] -> ([Node], Node, [Node])
breakByLhv (HRect hrectLhv _) y = case break (\x -> getNodeLhv x > hrectLhv) y
                     of ([], [])              -> error "can't breakByLhv on an empty list"
                        ([focus], [])         -> ([], focus, [])
                        (lsibsPlusFocus, [])  -> (init lsibsPlusFocus, last lsibsPlusFocus, [])
                        (lsibs, focus:rsibs)  -> (lsibs, focus, rsibs)


insertInLeaf :: ZNode -> HRect -> ZNode
insertInLeaf leaf hrect
  -- check for errors
  | length (getChildren leaf) > cL = assertIsLeaf `seq` error $ "error (insertInLeaf) - leaf overfull" ++ show (getChildren leaf) ++ "\nlength: "++show (length $ getChildren leaf)
  -- if there is space for another hrect, insert it
  | length (getChildren leaf) < cL = assertIsLeaf `seq` showInsertInLeafNotSplitting $ zipupFull $ modifyZNode leaf (makeNode newChildren)
  -- if there is no more space, make a new leaf node and call adjustTree
  | otherwise = assertIsLeaf `seq` showInsertInLeafSplitting adjustTree leaf $ makeNode [HRectChild hrect]
  where
    newChildren = insertNodeByLhv (HRectChild hrect) (getChildren leaf)
    assertIsLeaf
      | isLeaf leaf = True
      | otherwise   = error "insertInLeaf called on nonleaf"

---- take in sorted children nodes, return a node
makeNode :: [Node] -> Node
makeNode someChildren = Node { nodeLhv = maximum $ map getNodeLhv someChildren
                             , nodeMbr = getMbrs $ map getNodeMbr someChildren
                             , nodeChildren = someChildren
                             }

newTree :: [Rect] -> ZNode
newTree [] = error "sorry, you have to insert something in me with non-zero length"
newTree (rect0:rects) = foldr insert firstRoot rects
  where
    firstRoot = zipupFull $ makeZNode Nothing ([], []) $ makeNode [HRectChild $ fromRect rect0]

insert :: Rect -> ZNode -> ZNode
insert rect tree 
  | length (getAllSiblings tree) /= 1 = error $ "uh oh, root has siblings in insert\n" ++ show tree
  | otherwise = zipupFull $ insertInLeaf (chooseLeaf hrect tree) hrect
  where
    hrect = fromRect rect


showAdjustTreeSplit x = trace "adjustTree: splitting" x
showHandleNodeOverflowNotSplitting = trace "handleNodeOverflow not splitting yo"
showHandleNodeOverflowSplitting = trace "handleNodeOverflow splitting yo"
showRootSplit = trace "root split"
showZipupOnceCalledOnRoot = trace "zipupOnce called on root, returning root"
showInsertInLeafNotSplitting = trace "insertInLeaf not splitting"
showInsertInLeafSplitting = trace "insertInLeaf splitting"
