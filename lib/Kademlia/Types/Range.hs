module Kademlia.Types.Range where

import Data.Word
import Data.Bits

import Kademlia.Types.Word160

data Prefix = Prefix Word160 Word8
  deriving (Eq, Ord, Show)

-- Returns the common prefix of two Word160s and the number of bits in the prefix
commonPrefix :: Word160 -> Word160 -> Prefix
commonPrefix a b = Prefix (mask .&. a) (fromIntegral prefix)
  where
    overlap = a .^. b
    prefix = finiteBitSize a - countLeadingZeros overlap
    mask = shiftL oneBits (160 - prefix)

-- Determines if a Word160 falls within the range defined by a Prefix
inPrefixRange :: Prefix -> Word160 -> Bool
inPrefixRange (Prefix p n) w = p == mask .&. w
  where
    mask = shiftL oneBits (160 - fromIntegral n)