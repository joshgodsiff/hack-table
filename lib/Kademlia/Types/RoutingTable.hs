module Kademlia.Types.RoutingTable where

import Data.Bits
import qualified Data.List as L

import Kademlia.Types.Key
import qualified Kademlia.Types.KBucket as KB
import Kademlia.Types.KBucket (KBucket, kBucketSize)
import Kademlia.Types.Node
import Kademlia.Metric

bParam :: Int
bParam = 5

-- | A routing table is a binary tree of k-buckets
data RoutingTable = RoutingTable
  { own :: Node    -- ^ The node that owns this routing table
  , root :: RoutingTree    -- ^ The actual routing tree
  } deriving (Show)

-- | The routing tree structure
data RoutingTree
  = Leaf (KBucket Int Key Node)
  | Branch
    { prefix :: Int        -- ^ Common prefix for this subtree
    , left :: RoutingTree  -- ^ Left subtree (0)
    , right :: RoutingTree -- ^ Right subtree (1)
    } deriving (Show)

-- | Create an empty routing table
singleton :: Node -> RoutingTable
singleton n = RoutingTable
  { own = n
  , root = Leaf KB.empty
  }

-- insert :: Int -> Node -> RoutingTable -> RoutingTable
-- insert timestamp n rt = RoutingTable
--   { own = own rt
--   , root = insert' (root rt)
--   }
--   where
--     insert' :: RoutingTree -> RoutingTree
--     insert' (Leaf kb)
--       | KB.full kb = undefined -- Check if bucket contains own node, if it does, split it. Otherwise, drop the new node.
--       | otherwise = Leaf $ KB.insert timestamp (nodeId n) n kb
--     insert' (Branch p l r)
--       | prefix n p = Branch p (insert' l) r
--       | otherwise = Branch p l (insert' r)