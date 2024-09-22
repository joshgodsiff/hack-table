{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module KademliaSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.List (sortBy)

import Kademlia
import Kademlia.Metric
import Kademlia.SerDe
import Kademlia.Types.Word160 (Word160(..))
import qualified Kademlia.Types.RoutingTable as RT

-- Generator for Key
genKey :: Gen Key
genKey = fromBS <$> Gen.bytes (Range.singleton 20)

-- Generator for Node
genNode :: Gen Node
genNode = Node
  <$> genKey
  <*> (T.pack <$> Gen.string (Range.linear 7 15) Gen.alphaNum)
  <*> Gen.word16 Range.constantBounded

-- Property: XOR distance is symmetric
prop_distanceSymmetric :: Property
prop_distanceSymmetric = property $ do
  a <- forAll genKey
  b <- forAll genKey
  distance a b === distance b a

-- Property: XOR distance with self is zero
prop_distanceSelfZero :: Property
prop_distanceSelfZero = property $ do
  a <- forAll genKey
  distance a a === 0

-- Property: findKBucket always returns a value between 0 and 159
prop_findKBucketRange :: Property
prop_findKBucketRange = property $ do
  selfId <- forAll genKey
  targetId <- forAll genKey
  let bucketIndex = findKBucket selfId targetId
  assert (bucketIndex >= 0 && bucketIndex <= 159)

-- Property: nodeLookup always returns at most k nodes
prop_nodeLookupMaxK :: Property
prop_nodeLookupMaxK = property $ do
  self <- forAll genNode
  target <- forAll genKey
  let routingTable = RT.empty
  result <- evalIO $ nodeLookup self target routingTable
  assert (length result <= kBucketSize)

-- Test that compareByDistance correctly orders nodes
prop_compareByDistanceOrdering :: Property
prop_compareByDistanceOrdering = property $ do
  target <- forAll genKey
  a <- forAll genNode
  b <- forAll genNode
  c <- forAll genNode
  let nodes = [a, b, c]
      sortedNodes = sortBy (compareByDistance target) nodes
  assert (all (\(x, y) -> distance (nodeId x) target <= distance (nodeId y) target) (zip sortedNodes (tail sortedNodes)))

tests :: TestTree
tests = testGroup "Kademlia"
  [ testGroup "NodeID"
    [ testCase "can be created from ByteString" $
        fromBS (BS.pack [1..20]) @?= Key (Word160 (V.fromList [1..20]))
    , testCase "string encoding is the same as ByteString encoding" $
        Key ("0x1234567890abcdef1234567890abcdef12345678" :: Word160) @?= fromBS "0x1234567890abcdef1234567890abcdef12345678"
    ]
  , testGroup "XOR Distance"
    [ testProperty "is symmetric" prop_distanceSymmetric
    , testProperty "with self is zero" prop_distanceSelfZero
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
