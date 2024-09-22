{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kademlia.Types.Key where

import Data.Binary (Binary (..))
import Data.Bits (xor)

import Kademlia.Types.Word160
import Kademlia.Metric
import Kademlia.SerDe (ToByteString, FromByteString)


newtype Key = Key Word160
  deriving (Eq, Ord, Show, Enum, Binary, ToByteString, FromByteString)

instance Metric Key where
  distance (Key a) (Key b) = fromIntegral $ xor a b