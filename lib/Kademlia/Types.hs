{-# LANGUAGE OverloadedStrings #-}

module Kademlia.Types 
  ( NodeID(..)
  , Node(..)
  , KBucket(..)
  , RoutingTable(..)
  , nodeIDFromBS
  , xorDistance
  , xorDistanceToInt
  , kBucketSize
  , initRoutingTable
  ) where

import Data.Bits
import Data.Word
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Vector (Vector)
import qualified Data.Vector as V

newtype NodeID = NodeID (Vector Word32)
  deriving (Eq, Ord, Show)

data Node = Node
  { nodeId :: NodeID
  , nodeIP :: T.Text
  , nodePort :: Word16
  } deriving (Eq, Show)

newtype KBucket = KBucket (Vector Node)
  deriving (Show)

newtype RoutingTable = RoutingTable (Vector KBucket)
  deriving (Show)

kBucketSize :: Int
kBucketSize = 20  -- k parameter from the paper

-- Helper function to create a NodeID from a ByteString
nodeIDFromBS :: BS.ByteString -> NodeID
nodeIDFromBS bs = NodeID $ V.fromList $ map bytesToWord32 $ take 5 $ padAndChunk bs

-- Pad the ByteString to 20 bytes and chunk it into 4-byte segments
padAndChunk :: BS.ByteString -> [BS.ByteString]
padAndChunk bs = chunksOf 4 $ BS.append bs (BS.replicate (20 - BS.length bs) 0)

-- Convert a 4-byte ByteString to a Word32
bytesToWord32 :: BS.ByteString -> Word32
bytesToWord32 = BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) 0

-- Custom chunksOf function for ByteString
chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf n bs
  | BS.null bs = []
  | otherwise = BS.take n bs : chunksOf n (BS.drop n bs)

xorDistance :: NodeID -> NodeID -> NodeID
xorDistance (NodeID a) (NodeID b) = NodeID $ V.zipWith xor a b

xorDistanceToInt :: NodeID -> Integer
xorDistanceToInt (NodeID v) = foldr (\w acc -> acc * (2^(32 :: Int)) + fromIntegral w) 0 $ V.toList v

initRoutingTable :: RoutingTable
initRoutingTable = RoutingTable $ V.replicate 160 (KBucket V.empty)