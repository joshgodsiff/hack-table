{-# LANGUAGE OverloadedStrings #-}

module Kademlia 
  ( module Kademlia.Types
  , findKBucket
  , nodeLookup
  , compareByDistance
  , findClosestNodes
  ) where

import Kademlia.Types

import Data.List (sortBy)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.ByteString as BS

findKBucket :: Key -> Key -> Int
findKBucket selfId targetId = 159 - (floor (logBase (2 :: Double) (fromIntegral distance)) :: Int)
  where
    distance = xorDistance selfId targetId

data LookupState = LookupState
  { lookupTarget :: Key
  , lookupClosestNodes :: Vector Node
  , lookupQueriedNodes :: Vector Node
  }

nodeLookup :: Node -> Key -> RoutingTable -> IO [Node]
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
  let newQueriedNodes = V.take 3 $ V.filter (\n -> not $ n `V.elem` lookupQueriedNodes state) $ lookupClosestNodes state
      newState = state { lookupQueriedNodes = lookupQueriedNodes state V.++ newQueriedNodes }
  if V.null newQueriedNodes
    then return state
    else do
      let simulatedResults = simulateNodeResponses self (lookupTarget state) newQueriedNodes
          updatedClosestNodes = V.take kBucketSize $ V.fromList $ sortBy (compareByDistance (lookupTarget state)) $
                                V.toList $ lookupClosestNodes state V.++ simulatedResults
      nodeLookupStep self $ newState { lookupClosestNodes = updatedClosestNodes }

simulateNodeResponses :: Node -> Key -> Vector Node -> Vector Node
simulateNodeResponses self target queriedNodes =
  V.concatMap (\n -> V.fromList $ take 3 $ filter (/= self) $ generateRandomNodes (nodeId n) target) queriedNodes

generateRandomNodes :: Key -> Key -> [Node]
generateRandomNodes _ _ = 
  [Node (keyFromBS $ BS.pack [1,2,3,4,5]) "192.168.0.1" 8080
  ,Node (keyFromBS $ BS.pack [6,7,8,9,10]) "192.168.0.2" 8080
  ,Node (keyFromBS $ BS.pack [11,12,13,14,15]) "192.168.0.3" 8080]

compareByDistance :: NodeID -> Node -> Node -> Ordering
compareByDistance target a b = compare (xorDistance (nodeId a) target) (xorDistance (nodeId b) target)

findClosestNodes :: RoutingTable -> NodeID -> Int -> [Node]
findClosestNodes (RoutingTable buckets) target count =
  take count $ sortBy (compareByDistance target) $ concatMap (\(KBucket nodes) -> V.toList nodes) $ V.toList buckets