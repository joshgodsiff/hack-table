{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Word160Spec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog

import Data.Bits
import qualified Data.ByteString as BS
import Data.Binary
import Data.Word (Word8)

import Kademlia.Types.Word160

-- Generator for Word160
genWord160 :: Gen Word160
genWord160 = toWord160 <$> Gen.list (Range.singleton 20) (Gen.word8 Range.constantBounded)

-- Property: fromWord160 . toWord160 == id for [Word8]
prop_toFromWord160 :: Property
prop_toFromWord160 = property $ do
  ws <- forAll $ Gen.list (Range.singleton 20) (Gen.word8 Range.constantBounded)
  fromWord160 (toWord160 ws) === ws

-- Property: Binary encoding roundtrip
prop_binaryRoundtrip :: Property
prop_binaryRoundtrip = property $ do
  w <- forAll genWord160
  decode (encode w) === w

-- Property: Num instance properties
prop_numInstance :: Property
prop_numInstance = property $ do
  a <- forAll genWord160
  b <- forAll genWord160
  
  -- Commutativity of addition
  a + b === b + a
  
  -- Associativity of addition
  (a + b) + a === a + (b + a)
  
  -- Identity element of addition
  a + 0 === a

prop_numAdditiveInverse :: Property
prop_numAdditiveInverse = property $ do
  a <- forAll genWord160
  a + (-a) === 0

-- Property: Bits instance properties
prop_bitsInstance :: Property
prop_bitsInstance = property $ do
  a <- forAll genWord160
  b <- forAll genWord160
  i <- forAll $ Gen.int (Range.linear 0 159)
  
  -- Commutativity of AND and OR
  (a .&. b) === (b .&. a)
  (a .|. b) === (b .|. a)
  
  -- Associativity of AND and OR
  ((a .&. b) .&. a) === (a .&. (b .&. a))
  ((a .|. b) .|. a) === (a .|. (b .|. a))
  
  -- Identity and annihilator for AND and OR
  (a .&. complement 0) === a
  (a .|. 0) === a
  
  -- De Morgan's laws
  complement (a .&. b) === (complement a .|. complement b)
  complement (a .|. b) === (complement a .&. complement b)
  
  -- Properties of shift and rotate
  (shift a i `shift` (-i)) === a
  (rotate a 160) === a

-- Unit test for Show instance
unit_showInstance :: Assertion
unit_showInstance = 
  show (toWord160 [1,2,3] :: Word160) @?= "0x010203000000000000000000000000000000"

-- Unit test for Enum instance
unit_enumInstance :: Assertion
unit_enumInstance = do
  toEnum 257 @?= toWord160 [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1]
  fromEnum (toWord160 [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1]) @?= 257

-- Unit test for Bounded instance
unit_boundedInstance :: Assertion
unit_boundedInstance = do
  minBound @Word160 @?= toWord160 (replicate 20 0)
  maxBound @Word160 @?= toWord160 (replicate 20 255)

-- Unit test for IsString instance
unit_isStringInstance :: Assertion
unit_isStringInstance = 
  ("0x0102030405" :: Word160) @?= toWord160 [1,2,3,4,5]

tests :: TestTree
tests = testGroup "Word160"
  [ testProperty "toWord160 and fromWord160 are inverses" prop_toFromWord160
  , testProperty "Binary encoding roundtrip" prop_binaryRoundtrip
  , testProperty "Num instance properties" prop_numInstance
  , testProperty "Num additive inverse" prop_numAdditiveInverse
  , testProperty "Bits instance properties" prop_bitsInstance
  , testCase "Show instance" unit_showInstance
  , testCase "Enum instance" unit_enumInstance
  , testCase "Bounded instance" unit_boundedInstance
  , testCase "IsString instance" unit_isStringInstance
  ]
