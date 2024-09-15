{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kademlia.Types 
  ( NodeID(..)
  , Node(..)
  , KBucket(..)
  , RoutingTable(..)
  , nodeIDFromBS
  , nodeIDToBS
  , xorDistance
  , kBucketSize
  , initRoutingTable
  ) where

import Kademlia.Types.Word160 (Word160, fromBS, toBS)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Bits (xor)
import Data.Binary

newtype NodeID = NodeID Word160
  deriving (Eq, Ord, Show, Enum, Binary)

data Node = Node
  { nodeId :: NodeID
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

nodeIDFromBS :: BS.ByteString -> NodeID
nodeIDFromBS = NodeID . fromBS

nodeIDToBS :: NodeID -> BS.ByteString
nodeIDToBS (NodeID w) = toBS w

xorDistance :: NodeID -> NodeID -> Integer
xorDistance (NodeID a) (NodeID b) = fromIntegral $ xor a b

initRoutingTable :: RoutingTable
initRoutingTable = RoutingTable $ V.replicate 160 (KBucket V.empty)