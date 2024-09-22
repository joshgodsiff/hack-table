module Kademlia.Types.RoutingTable where

import Data.Vector (Vector)
import qualified Data.Vector as V

import Kademlia.Types.Key
import Kademlia.Types.KBucket as KB
import Kademlia.Types.Node
import Kademlia.Metric

newtype RoutingTable = RoutingTable (Vector (KB.KBucket Int Key Node))

empty :: RoutingTable
empty = RoutingTable $ V.replicate 160 (KB.empty)

findBucketIndex :: Key -> Key -> Int
findBucketIndex selfId targetId = 159 - (floor (logBase (2 :: Double) (fromIntegral d)) :: Int)
  where d = distance selfId targetId

