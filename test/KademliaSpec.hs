{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module KademliaSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.List (sortBy)

import Kademlia

-- Generator for NodeID
genNodeID :: Gen NodeID
genNodeID = nodeIDFromBS <$> Gen.bytes (Range.singleton 20)

-- Generator for Node
genNode :: Gen Node
genNode = Node
  <$> genNodeID
  <*> (T.pack <$> Gen.string (Range.linear 7 15) Gen.alphaNum)
  <*> Gen.word16 Range.constantBounded

-- Property: XOR distance is symmetric
prop_xorDistanceSymmetric :: Property
prop_xorDistanceSymmetric = property $ do
  a <- forAll genNodeID
  b <- forAll genNodeID
  xorDistance a b === xorDistance b a

-- Property: XOR distance with self is zero
prop_xorDistanceSelfZero :: Property
prop_xorDistanceSelfZero = property $ do
  a <- forAll genNodeID
  xorDistance a a === nodeIDFromBS (BS.replicate 20 0)

-- Property: findKBucket always returns a value between 0 and 159
prop_findKBucketRange :: Property
prop_findKBucketRange = property $ do
  selfId <- forAll genNodeID
  targetId <- forAll genNodeID
  let bucketIndex = findKBucket selfId targetId
  assert (bucketIndex >= 0 && bucketIndex <= 159)

-- Property: nodeLookup always returns at most k nodes
prop_nodeLookupMaxK :: Property
prop_nodeLookupMaxK = property $ do
  self <- forAll genNode
  target <- forAll genNodeID
  let routingTable = initRoutingTable
  result <- evalIO $ nodeLookup self target routingTable
  assert (length result <= kBucketSize)

-- Test that compareByDistance correctly orders nodes
prop_compareByDistanceOrdering :: Property
prop_compareByDistanceOrdering = property $ do
  target <- forAll genNodeID
  a <- forAll genNode
  b <- forAll genNode
  c <- forAll genNode
  let nodes = [a, b, c]
      sortedNodes = sortBy (compareByDistance target) nodes
  assert (all (\(x, y) -> xorDistance (nodeId x) target <= xorDistance (nodeId y) target) (zip sortedNodes (tail sortedNodes)))

tests :: TestTree
tests = testGroup "Kademlia"
  [ testGroup "NodeID"
    [ testCase "can be created from ByteString" $
        nodeIDFromBS (BS.pack [1..20]) @?= NodeID (V.fromList [16909060, 84281096, 151653132, 219025168, 286397204])
    ]
  , testGroup "XOR Distance"
    [ testProperty "is symmetric" prop_xorDistanceSymmetric
    , testProperty "with self is zero" prop_xorDistanceSelfZero
    ]
  , testGroup "K-Bucket"
    [ testProperty "findKBucket returns value between 0 and 159" prop_findKBucketRange
    ]
  , testGroup "Node Lookup"
    [ testProperty "returns at most k nodes" prop_nodeLookupMaxK
    ]
  , testGroup "Node Ordering"
    [ testProperty "compareByDistance correctly orders nodes" prop_compareByDistanceOrdering
    ]
  ]
