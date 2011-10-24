-- Node.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.Node( Node(..)
                , nodeFull
                , makeNode
                , makeLeaf
                ) where

import Lab2.Config
import Lab2.Rect

import Data.List(intercalate)

data Node = Node { nodeLhv :: Int
                 , nodeMbr :: Rect
                 , nodeChildren :: [Node]
                 }
          | Leaf { nodeLhv :: Int
                 , nodeMbr :: Rect
                 , leafHRects :: [HRect]
                 } -- deriving Show
instance Show Node where
  show (Leaf {leafHRects = r}) = "L{"++intercalate "," (map (\(HRect i _) -> show i) r)++"}" 
  show (Node _ _ nch) = "N{"++ intercalate "," (map show nch) ++ "}"

instance CanIntersect Node where
  mbr = nodeMbr

nodeFull :: Node -> Bool
nodeFull (Node {nodeChildren = ch})
  | length ch  > cN = error "nonleaf somehow got too many entries, this is a bug"
  | length ch == cN = True
  | otherwise       = False
nodeFull (Leaf {leafHRects = ch})
  | length ch  > cL = error "leaf somehow got too many entries, this is a bug"
  | length ch == cL = True
  | otherwise       = False


---- take in sorted children hrects, return a leaf node
makeLeaf :: [HRect] -> Node
makeLeaf someChildren = Leaf { nodeLhv = maximum $ map (\(HRect h _) -> h) someChildren
                             , nodeMbr = getMbrs $ map (\(HRect _ r) -> r) someChildren
                             , leafHRects = someChildren
                             }

---- take in sorted children nodes, return a nonleaf node
makeNode :: [Node] -> Node
makeNode someChildren = Node { nodeLhv = maximum $ map nodeLhv someChildren
                             , nodeMbr = getMbrs $ map nodeMbr someChildren
                             , nodeChildren = someChildren
                             }

