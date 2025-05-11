module Spec.RoutingTable (tests) where

import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog
import qualified Data.List as L
import Data.Bits
import Control.Monad (forM_)

import Kademlia.Types.Node (Node, nodeId)
import qualified Kademlia.Types.RoutingTable as RT
import qualified Kademlia.Types.KBucket as KB
import Kademlia.Types.Block (Block(..), canonicalBlock, contains)
import Kademlia.Types.Word160
import Kademlia.Types.Key (Key(..))

import Gen.Kademlia

tests :: TestTree
tests = testGroup "RoutingTable" 
  [ testProperty "singleton creates an empty routing table" prop_singleton
  , testProperty "shouldSplit returns true for root when full" prop_shouldSplit_root_full
  , testProperty "shouldSplit returns true when bucket contains own node" prop_shouldSplit_contains_own
  , testProperty "shouldSplit returns true for depth % bParam == 0" prop_shouldSplit_bParam
  , testProperty "shouldSplit returns false otherwise" prop_shouldSplit_otherwise
  , testProperty "split partitions nodes correctly" prop_split_partition
  , testProperty "split preserves all nodes" prop_split_preserves_nodes
  ]

-- Extract the Word160 from a Key
extractWord160 :: Key -> Word160
extractWord160 (Key w) = w

-- | Test that singleton creates an empty routing table with the correct owner node
prop_singleton :: Property
prop_singleton = property $ do
  node <- forAll genNode
  let rt = RT.singleton node
  RT.own rt === node
  case RT.root rt of
    RT.Leaf kb -> KB.null kb === True
    _          -> failure

-- | Test that shouldSplit returns true for a full k-bucket at the root (depth 0)
prop_shouldSplit_root_full :: Property
prop_shouldSplit_root_full = property $ do
  ownNode <- forAll genNode
  let ownKey = nodeId ownNode
  fullKB <- forAll $ genKBucket (Range.singleton KB.kBucketSize)
  
  -- Depth 0 and full k-bucket should trigger a split
  RT.shouldSplit 0 (Block 0 0) ownKey fullKB === True

-- | Test that shouldSplit returns true when the bucket contains the node's own key
prop_shouldSplit_contains_own :: Property
prop_shouldSplit_contains_own = property $ do
  ownNode <- forAll genNode
  let ownKey = nodeId ownNode
  depth <- forAll $ Gen.int (Range.linear 1 159)
  
  -- Create a block that contains the own key
  -- This is a bit tricky since we need to create a block that contains our key
  let block = canonicalBlock (extractWord160 ownKey) (fromIntegral depth)
  
  -- Create a non-full k-bucket
  kb <- forAll $ genKBucket (Range.linear 1 (KB.kBucketSize - 1))
  
  -- Verify the block actually contains our key (sanity check)
  assert $ contains block ownKey
  
  -- Should return true because block contains own key
  RT.shouldSplit depth block ownKey kb === True

-- | Test that shouldSplit returns true when depth is a multiple of bParam
prop_shouldSplit_bParam :: Property
prop_shouldSplit_bParam = property $ do
  ownNode <- forAll genNode
  let ownKey = nodeId ownNode
  
  -- Choose a depth that is a multiple of bParam but not 0
  bParamMultiple <- forAll $ Gen.int (Range.linear 1 30)
  let depth = bParamMultiple * RT.bParam

  -- Create a block that doesn't contain the own key
  -- This requires some careful construction
  let keyWord = extractWord160 ownKey
      -- Flip all bits to get something far from our key
      invertedKey = keyWord `xor` (maxBound :: Word160)
      -- Create a block with this key that won't contain our node
      block = canonicalBlock invertedKey (fromIntegral depth)
  
  -- Create a non-full k-bucket
  kb <- forAll $ genKBucket (Range.linear 1 (KB.kBucketSize - 1))
  
  -- Verify the block doesn't contain our key (sanity check)
  assert $ not $ contains block ownKey
  
  -- Should return true because depth is a multiple of bParam
  RT.shouldSplit depth block ownKey kb === True

-- | Test that shouldSplit returns false in other cases
prop_shouldSplit_otherwise :: Property
prop_shouldSplit_otherwise = property $ do
  ownNode <- forAll genNode
  let ownKey = nodeId ownNode
  
  -- Choose a depth that is NOT a multiple of bParam and not 0
  bParamMultiple <- forAll $ Gen.int (Range.linear 1 30)
  let nonBParamDepth = bParamMultiple * RT.bParam + 1  -- Add 1 to make it non-multiple
  
  -- Create a block that doesn't contain the own key
  let keyWord = extractWord160 ownKey
      -- Flip all bits to get something far from our key
      invertedKey = keyWord `xor` (maxBound :: Word160)
      -- Create a block with this key that won't contain our node
      block = canonicalBlock invertedKey (fromIntegral nonBParamDepth)
  
  -- Create a non-full k-bucket
  kb <- forAll $ genKBucket (Range.linear 1 (KB.kBucketSize - 1))
  
  -- Verify the block doesn't contain our key (sanity check)
  assert $ not $ contains block ownKey
  
  -- Should return false because:
  -- 1. It's not the root (depth != 0)
  -- 2. The bucket isn't full
  -- 3. The block doesn't contain own key
  -- 4. The depth is not a multiple of bParam
  RT.shouldSplit nonBParamDepth block ownKey kb === False

-- | Test that split correctly partitions nodes based on the bit at the given depth
prop_split_partition :: Property
prop_split_partition = property $ do
  depth <- forAll $ Gen.int (Range.linear 0 159)
  kb <- forAll $ genKBucket (Range.linear 5 15)
  
  let (leftTree, rightTree) = RT.split depth kb
  
  -- Check the results are leaves
  case (leftTree, rightTree) of
    (RT.Leaf leftKB, RT.Leaf rightKB) -> do
      -- Check each node went to the correct bucket based on the bit at depth
      let values = KB.toAscList kb
      forM_ values $ \(_, k, _) -> do
        let bitValue = testBit k (159 - depth)
        if bitValue
          then assert $ KB.member k rightKB 
          else assert $ KB.member k leftKB
    _ -> failure -- split should always return Leaf nodes

-- | Test that split preserves all nodes (no nodes are lost or duplicated)
prop_split_preserves_nodes :: Property
prop_split_preserves_nodes = property $ do
  depth <- forAll $ Gen.int (Range.linear 0 159)
  kb <- forAll $ genKBucket (Range.linear 5 15)
  
  let (leftTree, rightTree) = RT.split depth kb
  
  -- Check the results are leaves
  case (leftTree, rightTree) of
    (RT.Leaf leftKB, RT.Leaf rightKB) -> do
      -- Check that the total node count is preserved
      KB.size leftKB + KB.size rightKB === KB.size kb
      
      -- Additionally check that sets of keys are disjoint
      let leftKeys = map (\(_, k, _) -> k) $ KB.toAscList leftKB
          rightKeys = map (\(_, k, _) -> k) $ KB.toAscList rightKB
      assert $ null (L.intersect leftKeys rightKeys)
    _ -> failure -- split should always return Leaf nodes
