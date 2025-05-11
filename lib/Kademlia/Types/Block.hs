{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}

module Kademlia.Types.Block where

-- This is roughly analogous to a CIDR block in IP addressing.
-- We store a Word160 and a number of bits, which defines a range of Word160s.
-- The bits represent a mask over the top end of the Word160, which tells us how many bits out of 160 are significant.
-- By convention, non-significant bits are zeroed out.
-- This is useful when we're splitting K-Buckets and building a routing table.

import Data.Word
import Data.Bits

import Kademlia.Types.Word160

data Block = Block
  { base :: Word160
  , mask :: Word8
  }
  deriving (Eq, Ord, Show)

canonicalBlock :: Word160 -> Word8 -> Block
canonicalBlock w n = Block (toMask n .&. w) n

class Contains a where
  contains :: Block -> a -> Bool

instance Contains Word160 where
  contains :: Block -> Word160 -> Bool
  contains (Block p n) w = p == toMask n .&. w

class Prefix a where
  commonPrefix :: a -> a -> Block

instance Prefix Word160 where
  commonPrefix :: Word160 -> Word160 -> Block
  commonPrefix a b = canonicalBlock a (fromIntegral prefix)
    where
      prefix = countLeadingZeros (a .^. b)

class Mask a where
  toMask :: a -> Word160

instance Mask Int where
  toMask n = shiftL oneBits (160 - n)

instance Mask Word8 where
  toMask n = toMask (fromIntegral @Word8 @Int n)

instance Mask Word160 where
  toMask = id

instance Mask Block where
  toMask = toMask . mask

