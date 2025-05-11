module Kademlia.Types.RoutingTable where

import Data.Bits
import qualified Data.List as L

import Kademlia.Types.Key
import qualified Kademlia.Types.KBucket as KB
import Kademlia.Types.KBucket (KBucket, kBucketSize)
import Kademlia.Types.Node
import Kademlia.Metric
import Kademlia.Types.Block

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
    { prefix :: Block        -- ^ Common prefix for this subtree
    , left   :: RoutingTree  -- ^ Left subtree (0)
    , right  :: RoutingTree  -- ^ Right subtree (1)
    } deriving (Show)

-- | Create an empty routing table
singleton :: Node -> RoutingTable
singleton n = RoutingTable
  { own = n
  , root = Leaf KB.empty
  }

shouldSplit :: Int -> Block -> Key -> KBucket Int Key Node -> Bool
shouldSplit depth block ownKey kb
  | depth == 0 && KB.full kb = True -- Always split root if full
  | contains block ownKey = True -- Split if bucket contains own node
  | depth `mod` bParam == 0 = True -- Split based on b-parameter
  | otherwise = False

split :: Int -> KBucket Int Key Node -> (RoutingTree, RoutingTree)
split depth kb = (leftTree, rightTree)
  where
    (leftBucket, rightBucket) = KB.partition (\k -> not $ testBit k (159 - depth)) kb
    leftTree = Leaf leftBucket
    rightTree = Leaf rightBucket
