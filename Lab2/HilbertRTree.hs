-- HilbertRTree.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.HilbertRTree( newTree
                        , insert
                        ) where

import Lab2.Rect
import Lab2.HilbertCurve(xy2d)
import Lab2.Misc(evenlyDistribute)
import Data.List(insertBy, intersperse, foldl')
import Data.Function(on)
import Data.Maybe
import Debug.Trace
import Text.Printf
import System.IO.Unsafe


cL,cN :: Int
cL = 2
cN = 3

hilbertDim :: Int
hilbertDim = 16 -- for max value of 65535

insertNodeByLhv :: Node -> [Node] -> [Node]
insertNodeByLhv = insertBy (compare `on` getNodeLhv)

insertHRectByLhv :: HRect -> [HRect] -> [HRect]
insertHRectByLhv = insertBy (compare `on` (\(HRect h _) -> h))

data HRect = HRect Int Rect deriving Show

data Node = Node { nodeLhv :: Int
                 , nodeMbr :: Rect
                 , nodeChildren :: [Node]
                 }
          | Leaf { nodeLhv :: Int
                 , nodeMbr :: Rect
                 , leafHRects :: [HRect]
                 } -- deriving Show
instance Show Node where
  --show (HRectChild _) = "H"
--  show (HRectChild (HRect i _)) = (printf "r%.2g" ((fromIntegral i)::Double))
  show (Leaf {leafHRects = r}) = "L{"++boo++"}" 
    where
      boo = concat $ intersperse "," $ map poo r
        where
          poo (HRect i _) = show i
--  show (Node nlhv nmbr nch) = "{lhv:" ++ show nlhv ++ ", mbr:" ++ show nmbr ++ ", children:" ++ show nch ++ "}"
--  show (Node _ _ nch) = "{"++ show nch ++ "}"
  show (Node _ _ nch) = "N{"++ boo ++ "}"
    where
      boo = concat $ intersperse "," $ map show nch

getNodeLhv :: Node -> Int
getNodeLhv = nodeLhv

getNodeMbr :: Node -> Rect
getNodeMbr = nodeMbr

nodeFull :: Node -> Bool
nodeFull (Node {nodeChildren = ch})
  | length ch  > cN = error "nonleaf somehow got too many entries, this is a bug"
  | length ch == cN = True
  | otherwise       = False
nodeFull (Leaf {leafHRects = ch})
  | length ch  > cL = error "leaf somehow got too many entries, this is a bug"
  | length ch == cL = True
  | otherwise       = False


data ZNode = ZNode { znodeParent    :: Maybe ZNode
                   , znodeSiblings :: ([Node],[Node])
                   , znodeFocus    :: Node
                   } --deriving Show
instance Show ZNode where
  show = show . znodeFocus

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
  | totalNumSiblings znode < cN = trace "adjustTree has space, inserting" $ zipupFull $ insertNodeIntoZNodeByLhv newNode safeZNode
  
  -- otherwise call handleOverflow
  | otherwise = trace "adjustTree has no space, calling handleOverflow" $ handleOverflow safeZNode newNode
  where
    safeZNode
      | isNothing $ getParentZNode znode = znode
      | otherwise = znode
                
handleOverflow :: ZNode -> Node -> ZNode
handleOverflow znode newNode
  -- check for errors
  | newNumberOfSiblings > cN + 1 = error "error (handleOverflow): too many siblings somehonw"
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


zipupFull :: ZNode -> ZNode
zipupFull znode
  -- keep calling zipupOnce until the top is reached
  | isJust (znodeParent znode) = zipupFull $ zipupOnce znode
  -- if root hasn't split, return root
  -- if root has split, make old root and siblings the children of a new root
  | otherwise = case (length (getAllSiblings znode)) of 1 -> znode
                                                        _ -> newRoot
  where
    newRoot = showRootSplit $ trace ("old root: " ++ show znode ++ "\nnew root: " ++ show newRoot') newRoot'
    newRoot' = makeZNode Nothing ([], []) $ makeNode (getAllSiblings znode)

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
chooseLeaf _ znode@(ZNode {znodeFocus = (Leaf {})}) = trace ("chooseLeaf returning: " ++show znode) znode
chooseLeaf hrect znode = chooseLeaf hrect $ makeZNode (Just znode) (lsibs, rsibs) self
  where
    (lsibs, self, rsibs) = breakByLhv hrect $ getChildren znode

breakByLhv :: HRect -> [Node] -> ([Node], Node, [Node])
breakByLhv (HRect hrectLhv _) y = case break (\x -> getNodeLhv x > hrectLhv) y
                     of ([], [])              -> error "can't breakByLhv on an empty list"
                        ([focus], [])         -> ([], focus, [])
                        (lsibsPlusFocus, [])  -> (init lsibsPlusFocus, last lsibsPlusFocus, [])
                        (lsibs, focus:rsibs)  -> (lsibs, focus, rsibs)


insertInLeaf :: ZNode -> HRect -> ZNode
insertInLeaf (ZNode {znodeFocus = Node {}}) _ = error "insertInLeaf called on nonleaf znode"
insertInLeaf leaf hrect
  -- check for errors
  | length oldChildren > cL = error $ "error (insertInLeaf) - leaf overfull:\n" ++ show (getChildren leaf) ++ "\nlength: "++show (length $ getChildren leaf)
  
  -- if there is space for another hrect, insert it
  | length oldChildren < cL = showInsertInLeafNotSplitting $ zipupFull $ modifyZNode leaf (makeLeaf newChildren)
  
  -- if there is no more space, resort children by hibert value
  -- child with smallest hilbert value is new node
  -- rest of children are old znode
  -- call adjust tree
  | otherwise = showInsertInLeafSplitting $ adjustTree (modifyZNode leaf (makeLeaf chs)) $ makeLeaf [ch0]
  where
    oldChildren = leafHRects $ znodeFocus leaf
    newChildren@(ch0:chs) = insertHRectByLhv hrect oldChildren


---- take in sorted children hrects, return a leaf
makeLeaf :: [HRect] -> Node
makeLeaf someChildren = Leaf { nodeLhv = maximum $ map (\(HRect h _) -> h) someChildren
                             , nodeMbr = getMbrs $ map (\(HRect _ r) -> r) someChildren
                             , leafHRects = someChildren
                             }

---- take in sorted children nodes, return a node
makeNode :: [Node] -> Node
makeNode someChildren = Node { nodeLhv = maximum $ map getNodeLhv someChildren
                             , nodeMbr = getMbrs $ map getNodeMbr someChildren
                             , nodeChildren = someChildren
                             }

newTree :: [Rect] -> ZNode
newTree [] = error "sorry, you have to insert something in me with non-zero length"
newTree (rect0:rects) = foldl' (flip insert) firstRoot rects
  where
    firstRoot = zipupFull $ makeZNode Nothing ([], []) $ makeLeaf [fromRect rect0]

--insert :: Rect -> ZNode -> ZNode
--insert rect tree
--  | length (getAllSiblings tree) /= 1 = error $ "uh oh, root has siblings in insert\n" ++ show tree
--  | otherwise = trace ("-----------------------------inserting: "++ (printf "%.1g " (fromIntegral (hilbertValue rect) :: Double)) ++"----------------------\nbefore: " ++show tree++"\nafter: "++show newTree'++"\n\n") newTree'
--  where
--    newTree' = zipupFull $ insertInLeaf (chooseLeaf hrect tree) hrect
--    hrect = fromRect rect

insert :: Rect -> ZNode -> ZNode
insert rect tree
  | length (getAllSiblings tree) /= 1 = error $ "uh oh, root has siblings in insert\n" ++ show tree
  | otherwise = unsafePerformIO $ do
    putStrLn $ "\n-----------------inserting: "++ (printf "%.2g " (fromIntegral (hilbertValue rect) :: Double)) ++"----------------------"
    putStrLn $ "before: " ++show tree
    
    let newTree' = zipupFull $ insertInLeaf (chooseLeaf hrect tree) hrect
        hrect = fromRect rect
    
    newTree' `seq` putStrLn $ "after: "++show newTree'
    return newTree'



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
