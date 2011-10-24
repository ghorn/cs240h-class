-- HilbertRTree.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.HilbertRTree( newTree
                        , makeNewTree
                        , insert
                        ) where

import Lab2.Rect
import Lab2.Utils
import Lab2.Node
import Lab2.ZNode
import Lab2.Config
import Lab2.Debug

import Data.List(foldl')
import Data.Maybe
import Data.Time.Clock
import System.IO.Unsafe -- for debug messages only


-- The children have overflowed and passed in a newly split node.
-- If there is space for another sibling, insert this node in proper hilbert order.
-- If there is no space, call handleOverflow
adjustTree :: ZNode -> Node -> ZNode
adjustTree znode newNode
  -- check for errors
  | totalNumSiblings znode > cN = error "error (adjustTree): too many siblings somehow"
  
  -- if there is an extra space, simply insert the node by lhv
  | totalNumSiblings znode < cN = trace "adjustTree has space, inserting" $ zipupFull $ insertNodeIntoZNodeByLhv newNode safeZNode
  
  -- otherwise call handleOverflow
  | otherwise = trace "adjustTree has no space, calling handleOverflow" $ handleOverflow safeZNode newNode
  where
    safeZNode
      | isNothing $ getParentZNode znode = znode
      | otherwise = znode


-- A child could not be inserted in the desired node.
-- If any sibling has an empty slot, redistribute all children among all siblings in proper hilbert order
-- If all siblings are full, create new sibling and redistribute all children among all siblings,
-- then take the smallest sibling and make it a new node and pass to adjustTree
handleOverflow :: ZNode -> Node -> ZNode
handleOverflow znode newNode
  -- check for uncaught overflow
  | newNumberOfSiblings > cN + 1 = error "error (handleOverflow): too many siblings somehonw"

  -- check for bad call
  | newNumberOfSiblings < cN     = error "error - handleOverflow called when there is no overflow?"
  
  -- if it's possible to evenly redistribute siblings without splitting, do it
  | newNumberOfSiblings == cN = showHandleOverflowNotSplitting $ zipupFull $ makeZNode (getParentZNode znode) ([], bn1:bns) bn0
  
  -- if a split occurs, evenly redistribute all siblings
  -- then take the node with new smallest hilbert value and make it the child of a new parent node
  -- merge this new split parent node with the rebalanced node with adjustTree
  | otherwise = showHandleOverflowSplitting $ adjustTree (zipupOnce $ makeZNode (getParentZNode znode) ([], bns) bn1) $ makeNode [bn0]
  where
    allOldSiblings = getAllSiblings znode

    newNumberOfSiblings
      -- if any node has space, don't split
      | any (not . nodeFull) allOldSiblings = length allOldSiblings
      | otherwise                           = length allOldSiblings + 1
    
    -- nodes after children redistribution:
    bn0:bn1:bns = redistributeChildren allOldSiblings newNode newNumberOfSiblings


-- take in old sibling nodes and new sibling node
-- insert new node's child into old sibling's children by LHV
-- redistribute children among siblings evenly and return all new siblings
redistributeChildren :: [Node] -> Node -> Int -> [Node]
redistributeChildren allOldSiblings@((Node {}):_) newSibling@(Node {}) newNumberOfSiblings = newSiblings
  where
    newSiblings = map makeNode (evenlyDistribute newNumberOfSiblings newChildren)
      where
        newChildren = foldr insertNodeByLhv oldChildren (nodeChildren newSibling)
        oldChildren = concatMap nodeChildren allOldSiblings
redistributeChildren allOldSiblings@((Leaf {}):_) newSibling@(Leaf {}) newNumberOfSiblings = newSiblings
  where
    newSiblings = map makeLeaf (evenlyDistribute newNumberOfSiblings newChildren)
      where
        newChildren = foldr insertHRectByLhv oldChildren (leafHRects newSibling)
        oldChildren = concatMap leafHRects allOldSiblings
redistributeChildren _ _ _ = error "can't mix and match Nodes/Leaves in redistributeChildren"


-- take the zipper node at whatever level and zip it up until it reaches the top, return the result
-- if the root has split, create a new parent and zip up once more then return
zipupFull :: ZNode -> ZNode
zipupFull znode
  -- keep calling zipupOnce until the top is reached
  | isJust (getParentZNode znode) = zipupFull $ zipupOnce znode
  -- if root hasn't split, return root
  -- if root has split, make old root and siblings the children of a new root
  | otherwise = case (length (getAllSiblings znode)) of 1 -> znode
                                                        _ -> newRoot
  where
    newRoot = showRootSplit $ trace ("old root: " ++ show znode ++ "\nnew root: " ++ show newRoot') newRoot'
    newRoot' = makeZNode Nothing ([], []) $ makeNode (getAllSiblings znode)


-- take the zipper node and zip up one level, return parent zipper node
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


-- Take in hrect and zipper node. Descend recursively through tree until a leaf is reached.
-- At each node, descend to the child node which has smallest LHV still bigger than hrect
chooseLeaf :: HRect -> ZNode -> ZNode
chooseLeaf hrect znode
  | isLeaf znode = trace ("chooseLeaf returning: " ++show znode) znode
  | otherwise    = chooseLeaf hrect $ makeZNode (Just znode) (lsibs, rsibs) self
  where
    (lsibs, self, rsibs) = breakByLhv hrect $ getChildren znode
      where
        breakByLhv :: HRect -> [Node] -> ([Node], Node, [Node])
        breakByLhv (HRect hrectLhv _) y = case break (\x -> nodeLhv x > hrectLhv) y
                             of ([], [])               -> error "can't breakByLhv on an empty list"
                                ([focus], [])          -> ([], focus, [])
                                (lsibsPlusFocus, [])   -> (init lsibsPlusFocus, last lsibsPlusFocus, [])
                                (lsibs', focus:rsibs') -> (lsibs', focus, rsibs')


-- take a znode that is a leaf (chosen with chooseLeaf) and an hrect
-- insert the hrect into the zleaf if there is space
-- if there is not space, resort the children by LHV, then turn the smallest child into a new node
-- call adjustTree to insert the new node into the parent
insertInLeaf :: ZNode -> HRect -> ZNode
insertInLeaf leaf hrect
  | not (isLeaf leaf) = error "insertInLeaf called on nonleaf znode"
  | otherwise = safeInsertInLeaf
  where
    safeInsertInLeaf
      -- check for uncaught overflow
      | length oldChildren > cL = error $ "error (insertInLeaf) - leaf overfull:\n" ++ show (getChildren leaf) ++ "\nlength: "++show (length $ getChildren leaf)
      
      -- if there is space for another hrect, insert it
      | length oldChildren < cL = showInsertInLeafNotSplitting $ zipupFull $ modifyZNode leaf (makeLeaf newChildren)
      
      -- if there is no more space, resort children by hibert value
      -- child with smallest hilbert value is new node
      -- rest of children are old znode
      -- call adjust tree
      | otherwise = showInsertInLeafSplitting $ adjustTree (modifyZNode leaf (makeLeaf chs)) $ makeLeaf [ch0]
      where
        oldChildren = leafHRects $ getFocusNode leaf
        newChildren@(ch0:chs) = insertHRectByLhv hrect oldChildren


-- take a bunch of rectangles
-- create a tree out of the first rectangle
-- fold insert over the remaining rectangles using tree as the accumulator
newTree :: [Rect] -> ZNode
newTree [] = error "sorry, you have to insert something in me with non-zero length"
newTree (rect0:rects) = foldl' (flip insert) firstRoot rects
  where
    firstRoot = zipupFull $ makeZNode Nothing ([], []) $ makeLeaf [fromRect rect0]

-- time the construction of a new tree
-- print the time, return the tree
makeNewTree :: [Rect] -> IO ZNode
makeNewTree rects = do
  startTime <- getCurrentTime
  let tree = newTree rects
  endTime   <- tree `seq` getCurrentTime
  
  let diffTime = (realToFrac $ diffUTCTime endTime startTime)::Double
  putStrLn $ "constructed tree in " ++ show diffTime ++ " seconds"

  return tree

--insert :: Rect -> ZNode -> ZNode
--insert rect tree
--  | length (getAllSiblings tree) /= 1 = error $ "uh oh, root has siblings in insert\n" ++ show tree
--  | otherwise = trace ("-----------------------------inserting: "++ (printf "%.1g " (fromIntegral (hilbertValue rect) :: Double)) ++"----------------------\nbefore: " ++show tree++"\nafter: "++show newTree'++"\n\n") newTree'
--  where
--    newTree' = zipupFull $ insertInLeaf (chooseLeaf hrect tree) hrect
--    hrect = fromRect rect


-- take a rectangle and a tree, convert the rectangle into an hrect (rectangle with hilbert value)
-- call chooseleaf to find the proper znode to insert in
-- call insertInLeaf on this leaf
insert :: Rect -> ZNode -> ZNode
insert rect tree
  | length (getAllSiblings tree) /= 1 = error $ "uh oh, root has siblings in insert\n" ++ show tree
  | otherwise = unsafePerformIO $ do
    dbgPutStrLn $ "\n-----------------inserting: "++ show (hilbertValue rect) ++"----------------------"
    dbgPutStrLn $ "before: " ++show tree
    
    let newTree' = zipupFull $ insertInLeaf (chooseLeaf hrect tree) hrect
        hrect = fromRect rect
    
    newTree' `seq` dbgPutStrLn $ "after: "++show newTree'
    return newTree'


-- suppressible debugging messages
showHandleOverflowNotSplitting :: a -> a
showHandleOverflowSplitting :: a -> a
showRootSplit :: a -> a
showZipupOnceCalledOnRoot :: a -> a
showInsertInLeafNotSplitting :: a -> a
showInsertInLeafSplitting :: a -> a

showHandleOverflowNotSplitting = trace "handleOverflow can redistribute"
showHandleOverflowSplitting = trace "handleOverflow must split"
showRootSplit = trace "root split"
showZipupOnceCalledOnRoot = trace "zipupOnce called on root, returning root"
showInsertInLeafNotSplitting = trace "insertInLeaf not splitting"
showInsertInLeafSplitting = trace "insertInLeaf splitting"

hilbertValue :: Rect -> Int
hilbertValue r = blah $ fromRect r
  where
    blah (HRect i _) = i
