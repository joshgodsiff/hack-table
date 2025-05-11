{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Kademlia (tests) where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog
import qualified Hedgehog.Range as Range
import Hedgehog
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString as BS

import Kademlia.Types.Key
import Kademlia.Types.Node
import Kademlia.Metric
import Kademlia.SerDe
import Kademlia.Types.Word160 (Word160(..))
import qualified Kademlia.Types.RoutingTable as RT
import qualified Kademlia.Types.KBucket as KB

import Gen.Kademlia

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

-- Property: KBucket size is always <= kBucketSize
prop_kBucketSize :: Property
prop_kBucketSize = property $ do
  bucket <- forAll $ genKBucket (Range.linear 0 30)
  assert $ KB.size bucket <= KB.kBucketSize

-- Property: KBucket maintains order (most recently seen at the end)
prop_kBucketOrder :: Property
prop_kBucketOrder = property $ do
  bucket <- forAll $ genKBucket (Range.linear 2 10)
  let (Just (lastNode, _))  = KB.maxView bucket
  let (Just (firstNode, _)) = KB.minView bucket
  let nodes = KB.toAscListValues bucket
  nodeId lastNode === nodeId (last nodes)
  nodeId firstNode === nodeId (head nodes)

-- Unit test for RoutingTable.findBucketIndex
-- unit_findBucketIndex :: Assertion
-- unit_findBucketIndex = do
--   let selfId = Key (Word160 (V.replicate 20 0))
--       targetId = Key (Word160 (V.replicate 19 0 <> V.singleton 1))
--   RT.findBucketIndex selfId targetId @?= 159

tests :: TestTree
tests = testGroup "Kademlia"
  [ testGroup "NodeID"
    [ testCase "can be created from ByteString" $
        fromBS (BS.pack [1..20]) @?= Key (Word160 (V.fromList [1..20]))
    ]
  , testGroup "XOR Distance"
    [ testProperty "is symmetric" prop_distanceSymmetric
    , testProperty "with self is zero" prop_distanceSelfZero
    ]
  , testGroup "KBucket"
    [ testProperty "size is always <= kBucketSize" prop_kBucketSize
    , testProperty "maintains order (most recently seen at the end)" prop_kBucketOrder
    ]
  -- , testGroup "RoutingTable"
  --   [ testCase "findBucketIndex works correctly" unit_findBucketIndex
  --   ]
  ]
