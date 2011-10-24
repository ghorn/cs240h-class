-- ZNode.hs

{-# OPTIONS_GHC -Wall #-}

module Lab2.ZNode( ZNode
                 , isLeaf
                 , getSiblingNodes
                 , getParentZNode
                 , getFocusNode
                 , makeZNode
                 , getChildren
                 , getAllSiblings
                 , totalNumSiblings
                 , modifyZNode
                 ) where

import Lab2.Node

data ZNode = ZNode { znodeParent    :: Maybe ZNode
                   , znodeSiblings :: ([Node],[Node])
                   , znodeFocus    :: Node
                   }

instance Show ZNode where
  show = show . znodeFocus


-- field accessors
getParentZNode :: ZNode -> Maybe ZNode
getParentZNode = znodeParent

getSiblingNodes :: ZNode -> ([Node], [Node])
getSiblingNodes = znodeSiblings

getFocusNode :: ZNode -> Node
getFocusNode = znodeFocus


-- constructors
makeZNode :: Maybe ZNode -> ([Node], [Node]) -> Node -> ZNode
makeZNode parent sibs focus = ZNode {znodeParent = parent, znodeSiblings = sibs, znodeFocus = focus}

modifyZNode :: ZNode -> Node -> ZNode
modifyZNode znode = makeZNode (getParentZNode znode) (getSiblingNodes znode)


-- utilities
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

isLeaf :: ZNode -> Bool 
isLeaf (ZNode {znodeFocus = (Leaf {})}) = True
isLeaf _ = False
