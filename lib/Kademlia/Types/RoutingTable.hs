{-# LANGUAGE TypeApplications #-}

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

-- | Determine if a bucket should be split based on several criteria
-- 1. Always split the root bucket if it's full
-- 2. Split if the bucket contains the own node
-- 3. Split if the depth is a multiple of bParam (configurable splitting frequency)
shouldSplit :: Int -> Block -> Key -> KBucket Int Key Node -> Bool
shouldSplit depth block ownKey kb
  | depth == 0 && KB.full kb = True -- Always split root if full
  | contains block ownKey = True -- Split if bucket contains own node
  | depth `mod` bParam == 0 = True -- Split based on b-parameter
  | otherwise = False

-- | Split a k-bucket into two leaves based on a bit at the given depth
-- Returns a pair of trees, where the left tree contains nodes with 0 at the bit position
-- and the right tree contains nodes with 1 at the bit position
split :: Int -> KBucket Int Key Node -> (RoutingTree, RoutingTree)
split depth kb = (leftTree, rightTree)
  where
    (leftBucket, rightBucket) = KB.partition (\k -> not $ testBit k (159 - depth)) kb
    leftTree = Leaf leftBucket
    rightTree = Leaf rightBucket

-- | Insert a node into a routing tree
-- This function handles both leaf and branch nodes and takes care of splitting when needed
insertTree :: Int -> Key -> Node -> RoutingTree -> RoutingTree
insertTree priority ownKey node (Leaf kb)
    | KB.member nodeKey kb || KB.size kb < kBucketSize = inserted -- Node is already in the bucket or there's space for it
    | shouldSplit 0 initialBlock ownKey kb = -- Split the bucket and retry insertion
      let (leftTree, rightTree) = split 0 kb
      in insertTree priority ownKey node (Branch initialBlock leftTree rightTree)
    | otherwise = inserted  -- Don't split, just try to insert (which might evict or be a no-op)
  where
    nodeKey = nodeId node
    inserted = Leaf (KB.insert priority nodeKey node kb)
    initialBlock = Block 0 0  -- Empty block for root level

insertTree priority ownKey node (Branch blockPrefix leftSubtree rightSubtree)
  | not (testBit nodeKey bitIndex) = Branch blockPrefix (insertTree priority ownKey node leftSubtree) rightSubtree  -- Bit is 0, go left
  | otherwise = Branch blockPrefix leftSubtree (insertTree priority ownKey node rightSubtree)  -- Bit is 1, go right
  where
      nodeKey = nodeId node
      -- 160 bits is standard for Kademlia keys
      keyNumBits = (finiteBitSize @Key undefined )
      -- Get the depth from the mask in the block prefix
      depth = fromIntegral $ mask blockPrefix
      bitIndex = keyNumBits - 1 - depth

-- | Insert a node into the routing table with the given priority
insert :: Int -> Node -> RoutingTable -> RoutingTable
insert priority node rt =
  let ownKey = nodeId (own rt)
      newRoot = insertTree priority ownKey node (root rt)
  in rt { root = newRoot }

-- | Insert a node into the routing table with default priority 0
insertDefault :: Node -> RoutingTable -> RoutingTable
insertDefault = insert 0
