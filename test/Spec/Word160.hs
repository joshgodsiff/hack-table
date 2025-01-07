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
import Data.Binary

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

-- Property: fromInteger instance properties
prop_fromIntegerInstance :: Property
prop_fromIntegerInstance = property $ do
  n <- forAll $ Gen.integral (Range.linear 0 (2^(160::Int) - 1))
  toInteger (fromInteger n :: Word160) === n

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
  
-- Properties of rotate
prop_bitsInstance_rotate :: Property
prop_bitsInstance_rotate = property $ do
    a <- forAll genWord160
    i <- forAll $ Gen.int (Range.linear 0 159)

    -- The final value should be equal to the original value after rotating left and then right by the same amount.
    rotateR (rotateL a i) i === a
    rotateL (rotateR a i) i === a

prop_bitsInstance_shift :: Property
prop_bitsInstance_shift = property $ do
    a <- forAll genWord160
    i <- forAll $ Gen.int (Range.linear 0 159)

    let shiftedLeft = a `shiftL` i
    let shiftedBack = shiftedLeft `shiftR` i

    -- The final value should be equal to the original value with the lower `160 - i` bits preserved.
    let expected = a .&. (complement zeroBits `shiftR` i)
    
    shiftedBack === expected

prop_bitsInstance_shiftL :: Property
prop_bitsInstance_shiftL = property $ do
    a <- forAll genWord160
    i <- forAll $ Gen.int (Range.linear 0 159)
    
    toInteger (a `shiftL` i) === correctWord128 (toInteger a `shiftL` i)
  where
    correctWord128 :: Integer -> Integer
    correctWord128 x = x .&. toInteger (maxBound @Word160)
    

prop_bitsInstance_shiftR :: Property
prop_bitsInstance_shiftR = property $ do
    w160 <- forAll genWord160
    i <- forAll $ Gen.int (Range.linear 0 159)

    toInteger (shiftR w160 i) === shiftR (toInteger w160) i

-- Unit test for fromInteger instance
unit_fromIntegerInstance :: Assertion
unit_fromIntegerInstance = do
  fromInteger 257 @?= toWord160 [1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  fromInteger (2^(160::Int) - 1) @?= toWord160 (replicate 20 0xFF)

-- Unit test for Show instance
unit_showInstance :: Assertion
unit_showInstance = 
  show (toWord160 [1,2,3] :: Word160) @?= "0x0000000000000000000000000000000000030201"

unit_showInstance_roundtrip :: Assertion
unit_showInstance_roundtrip = do
  show (0x0000000000000000000000000000000000030201 :: Word160) @?= "0x0000000000000000000000000000000000030201"

-- Unit test for Enum instance
unit_enumInstance_toEnum :: Assertion
unit_enumInstance_toEnum = do
  toEnum 257 @?= toWord160 [1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

unit_enumInstance_fromEnum :: Assertion
unit_enumInstance_fromEnum = do
  fromEnum (toWord160 [1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]) @?= 257


-- Unit test for Bounded instance
unit_boundedInstance :: Assertion
unit_boundedInstance = do
  minBound @Word160 @?= toWord160 (replicate 20 0)
  maxBound @Word160 @?= toWord160 (replicate 20 255)



tests :: TestTree
tests = testGroup "Word160"
  [ testProperty "toWord160 and fromWord160 are inverses" prop_toFromWord160
  , testProperty "Num instance - fromInteger properties" prop_fromIntegerInstance
  , testProperty "Binary encoding roundtrip" prop_binaryRoundtrip
  , testProperty "Num instance properties" prop_numInstance
  , testProperty "Num additive inverse" prop_numAdditiveInverse
  , testProperty "Bits instance properties" prop_bitsInstance
  , testProperty "Bits instance - shift" prop_bitsInstance_shift
  , testProperty "Bits instance - shiftL" prop_bitsInstance_shiftL
  , testProperty "Bits instance - shiftR" prop_bitsInstance_shiftR
  , testProperty "Bits instance - rotate" prop_bitsInstance_rotate
  , testCase "Num instance - fromInteger" unit_fromIntegerInstance
  , testCase "Show instance" unit_showInstance
  , testCase "Show instance - roundtrip" unit_showInstance_roundtrip
  , testCase "Enum instance - toEnum" unit_enumInstance_toEnum
  , testCase "Enum instance - fromEnum" unit_enumInstance_fromEnum
  , testCase "Bounded instance" unit_boundedInstance
  ]
