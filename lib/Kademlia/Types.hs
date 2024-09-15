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

newtype Key = Key Word160
  deriving (Eq, Ord, Show, Enum, Binary)

instance Metric Key where
  distance (Key a) (Key b) = fromIntegral $ xor a b

instance ToByteString Key where
  toBS (Key w160) = toBS w160

instance FromByteString Key where
  fromBS bs = Key $ fromBS bs

data Node = Node
  { nodeId :: Key
  , nodeIP :: T.Text
  , nodePort :: Word16
  } deriving (Eq, Show)

instance Binary Node where
  put (Node nid ip port) = put nid >> put (T.unpack ip) >> put port
  get = Node <$> get <*> (T.pack <$> get) <*> get

newtype KBucket = KBucket (Vector Node)
  deriving (Show)

instance Binary KBucket where
  put (KBucket nodes) = put (V.toList nodes)
  get = KBucket . V.fromList <$> get

newtype RoutingTable = RoutingTable (Vector KBucket)
  deriving (Show)

instance Binary RoutingTable where
  put (RoutingTable buckets) = put (V.toList buckets)
  get = RoutingTable . V.fromList <$> get

kBucketSize :: Int
kBucketSize = 20  -- k parameter from the paper

initRoutingTable :: RoutingTable
initRoutingTable = RoutingTable $ V.replicate 160 (KBucket V.empty)