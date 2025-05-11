{-# LANGUAGE TypeApplications #-}

module Spec.Block (tests) where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog

import Data.Bits
import Data.Word

import Kademlia.Types.Block
import Kademlia.Types.Word160
import Gen.Kademlia (genWord160)

-- Property: A block always contains its base
prop_containsBase :: Property
prop_containsBase = property $ do
  base <- forAll genWord160
  mask <- forAll $ Gen.word8 (Range.linear 0 160)
  let block = canonicalBlock base mask
  let maskedBase = toMask mask .&. base
  assert $ contains block maskedBase

-- Property: Common prefix of identical values is the full value
prop_commonPrefixIdentical :: Property
prop_commonPrefixIdentical = property $ do
  w <- forAll genWord160
  let Block b m = commonPrefix w w
  m === 160
  b === w

-- Property: Common prefix mask is always valid
prop_commonPrefixMask :: Property
prop_commonPrefixMask = property $ do
  a <- forAll genWord160
  b <- forAll genWord160
  let Block _ m = commonPrefix a b
  assert $ m >= 0 && m <= 160

-- Property: Contains is consistent with common prefix
prop_containsCommonPrefix :: Property
prop_containsCommonPrefix = property $ do
  a <- forAll genWord160
  b <- forAll genWord160
  let block = commonPrefix a b
  assert $ contains block a
  assert $ contains block b

-- Unit test for specific common prefix cases
unit_commonPrefix :: Assertion
unit_commonPrefix = do
  let a = fromInteger 0xff00000000000000000000000000000000000000 :: Word160
  let b = fromInteger 0xffff000000000000000000000000000000000000 :: Word160
  let Block _ m = commonPrefix a b
  m @?= 8  -- Only the first byte differs

-- Unit test for block containment
unit_contains :: Assertion
unit_contains = do
  let base = fromInteger 0x8000000000000000000000000000000000000000 :: Word160
  let block = Block base 1
  -- Should contain values with same first bit
  assertBool "Should contain base" $ contains block base
  assertBool "Should contain similar value" $ 
    contains block (fromInteger 0x8010000000000000000000000000000000000000 :: Word160)
  assertBool "Should not contain different prefix" $ 
    not $ contains block (fromInteger 0x4000000000000000000000000000000000000000 :: Word160)


tests :: TestTree
tests = testGroup "Block"
  [ testProperty "Block contains its base" prop_containsBase
  , testProperty "Common prefix of identical values" prop_commonPrefixIdentical
  , testProperty "Common prefix mask is valid" prop_commonPrefixMask
  , testProperty "Contains is consistent with common prefix" prop_containsCommonPrefix
  , testCase "Common prefix specific cases" unit_commonPrefix
  , testCase "Block containment" unit_contains
  ]