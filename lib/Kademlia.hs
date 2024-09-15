{-# LANGUAGE OverloadedStrings #-}

module Kademlia where

import Data.Bits
import Data.Word
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List (sortBy)

newtype NodeID = NodeID (Vector Word32)
  deriving (Eq, Ord, Show)

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

data Node = Node
  { nodeId :: NodeID
  , nodeIP :: T.Text
  , nodePort :: Word16
  } deriving (Eq, Show)

xorDistance :: NodeID -> NodeID -> NodeID
xorDistance (NodeID a) (NodeID b) = NodeID $ V.zipWith xor a b

newtype KBucket = KBucket (Vector Node)
  deriving (Show)

kBucketSize :: Int
kBucketSize = 20  -- k parameter from the paper

newtype RoutingTable = RoutingTable (Vector KBucket)
  deriving (Show)

initRoutingTable :: RoutingTable
initRoutingTable = RoutingTable $ V.replicate 160 (KBucket V.empty)

findKBucket :: NodeID -> NodeID -> Int
findKBucket selfId targetId = 159 - (floor (logBase (2 :: Double) (fromIntegral distance)) :: Int)
  where
    distance = xorDistanceToInt $ xorDistance selfId targetId

xorDistanceToInt :: NodeID -> Integer
xorDistanceToInt (NodeID v) = foldr (\w acc -> acc * (2^(32 :: Int)) + fromIntegral w) 0 $ V.toList v


data LookupState = LookupState
  { lookupTarget :: NodeID
  , lookupClosestNodes :: Vector Node
  , lookupQueriedNodes :: Vector Node
  }

nodeLookup :: Node -> NodeID -> RoutingTable -> IO [Node]
nodeLookup self target routingTable = do
  let initialState = LookupState
        { lookupTarget = target
        , lookupClosestNodes = V.fromList $ findClosestNodes routingTable target kBucketSize
        , lookupQueriedNodes = V.empty
        }
  finalState <- nodeLookupStep self initialState
  return $ V.toList $ lookupClosestNodes finalState

nodeLookupStep :: Node -> LookupState -> IO LookupState
nodeLookupStep self state = do
  -- In a real implementation, this would involve network communication
  -- For now, we'll just simulate it
  let newQueriedNodes = V.take 3 $ V.filter (\n -> not $ n `V.elem` lookupQueriedNodes state) $ lookupClosestNodes state
      newState = state { lookupQueriedNodes = lookupQueriedNodes state V.++ newQueriedNodes }
  if V.null newQueriedNodes
    then return state
    else do
      -- Simulate querying nodes and getting results
      let simulatedResults = simulateNodeResponses self (lookupTarget state) newQueriedNodes
          updatedClosestNodes = V.take kBucketSize $ V.fromList $ sortBy (compareByDistance (lookupTarget state)) $
                                V.toList $ lookupClosestNodes state V.++ simulatedResults
      nodeLookupStep self $ newState { lookupClosestNodes = updatedClosestNodes }


simulateNodeResponses :: Node -> NodeID -> Vector Node -> Vector Node
simulateNodeResponses self target queriedNodes =
  V.concatMap (\n -> V.fromList $ take 3 $ filter (/= self) $ generateRandomNodes (nodeId n) target) queriedNodes

generateRandomNodes :: NodeID -> NodeID -> [Node]
generateRandomNodes _ _ = 
  -- In a real implementation, this would generate nodes based on the network state
  -- For now, we'll just create some dummy nodes
  [Node (nodeIDFromBS $ BS.pack [1,2,3,4,5]) "192.168.0.1" 8080
  ,Node (nodeIDFromBS $ BS.pack [6,7,8,9,10]) "192.168.0.2" 8080
  ,Node (nodeIDFromBS $ BS.pack [11,12,13,14,15]) "192.168.0.3" 8080]

compareByDistance :: NodeID -> Node -> Node -> Ordering
compareByDistance target a b = compare (xorDistance (nodeId a) target) (xorDistance (nodeId b) target)

findClosestNodes :: RoutingTable -> NodeID -> Int -> [Node]
findClosestNodes (RoutingTable buckets) target count =
  take count $ sortBy (compareByDistance target) $ concatMap (\(KBucket nodes) -> V.toList nodes) $ V.toList buckets