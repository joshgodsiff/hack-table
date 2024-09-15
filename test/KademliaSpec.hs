{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module KademliaSpec (spec) where

import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import HaskellWorks.Hspec.Hedgehog (requireProperty)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Word
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.Text as T

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

-- Hspec test suite
spec :: Spec
spec = do
  describe "Kademlia" $ do
    describe "NodeID" $ do
      it "can be created from ByteString" $ requireProperty $ do
        let bs = BS.pack [1..20]
        nodeIDFromBS bs `shouldBe` NodeID (V.fromList [16909060, 84281096, 151653132, 219025168, 286397204])

    describe "XOR Distance" $ do
      it "Should be symmetric" $ requireProperty $ do
        prop_xorDistanceSymmetric

      it "To itself should be zero" $ requireProperty $ do
        prop_xorDistanceSelfZero

    describe "K-Bucket" $ requireProperty $ do
      prop_findKBucketRange

    describe "Node Lookup" $ requireProperty $ do
      prop_nodeLookupMaxK

    describe "Node Ordering" $ requireProperty $ do
      prop_compareByDistanceOrdering