{-# LANGUAGE TypeApplications #-}
module Spec.KBucket where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog
import Kademlia.Types.KBucket as KB

-- Generator for test data
genPriority :: Gen Int
genPriority = Gen.integral (Range.linear 0 100)

genKey :: Gen Int
genKey = Gen.integral (Range.linear 0 100)

genValue :: Gen String
genValue = Gen.string (Range.linear 1 10) Gen.alpha

-- Properties
prop_emptyBucketSize :: Property
prop_emptyBucketSize = property $ do
  let bucket = KB.empty
  KB.size bucket === 0

prop_singletonSize :: Property
prop_singletonSize = property $ do
  p <- forAll genPriority
  k <- forAll genKey
  v <- forAll genValue
  KB.size (KB.singleton p k v) === 1

prop_insertRespectsSizeLimit :: Property
prop_insertRespectsSizeLimit = property $ do
  -- Generate more items than kBucketSize
  items <- forAll $ Gen.list (Range.linear (kBucketSize + 1) (kBucketSize * 2)) $
    (,,) <$> genPriority <*> genKey <*> genValue
  
  let bucket = foldr (\(p,k,v) b -> KB.insert p k v b) KB.empty items
  assert $ KB.size bucket <= kBucketSize

prop_memberAfterInsert :: Property
prop_memberAfterInsert = property $ do
  p <- forAll genPriority
  k <- forAll genKey
  v <- forAll genValue
  let bucket = KB.insert p k v KB.empty
  assert $ KB.member k bucket

tests :: TestTree
tests = testGroup "KBucket Tests"
  [ testGroup "Basic Operations"
    [ testProperty "empty bucket has size 0" prop_emptyBucketSize
    , testProperty "singleton bucket has size 1" prop_singletonSize
    , testProperty "insert respects k-bucket size limit" prop_insertRespectsSizeLimit
    , testProperty "member returns true after insert" prop_memberAfterInsert
    , testCase "manual insertion test" $ do
        let bucket = KB.insert 1 1 "one"
                  $ KB.insert 2 2 "two" 
                  $ KB.empty @Int @Int
        KB.size bucket @?= 2
    ]
  ]
