{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Kademlia.Types.Key where

import Data.Binary (Binary (..))
import Data.Bits (xor, Bits, FiniteBits)

import Kademlia.Types.Word160
import Kademlia.Metric
import Kademlia.SerDe (ToByteString, FromByteString)
import Kademlia.Types.Block (Prefix (..), Contains(..))

newtype Key = Key Word160
  deriving (Eq, Ord, Show, Enum, Binary, Bits, FiniteBits, ToByteString, FromByteString)

instance Metric Key where
  distance :: Num b => Key -> Key -> b
  distance (Key a) (Key b) = fromIntegral $ xor a b

instance Prefix Key where
  commonPrefix (Key a) (Key b) = commonPrefix a b

instance Contains Key where
  contains b (Key w) = contains b w
