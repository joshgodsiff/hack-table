{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kademlia.Types 
  ( Key(..)
  , Node(..)
  , KBucket(..)
  , RoutingTable(..)
  , kBucketSize
  , initRoutingTable
  ) where

import Kademlia.Types.Word160 (Word160 (..))
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Bits (xor)
import Data.Binary
import Kademlia.Metric (Metric (..))
import Kademlia.SerDe

import Kademlia.Types.KBucket

---------------------------------------------------------------

newtype RoutingTable = RoutingTable (Vector KBucket)
  deriving (Show)

instance Binary RoutingTable where
  put (RoutingTable buckets) = put (V.toList buckets)
  get = RoutingTable . V.fromList <$> get

initRoutingTable :: RoutingTable
initRoutingTable = RoutingTable $ V.replicate 160 (KBucket V.empty)