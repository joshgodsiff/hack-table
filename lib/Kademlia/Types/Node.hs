module Kademlia.Types.Node where

import qualified Data.Text as T
import Data.Word (Word16)

import Kademlia.Types.Key

data Node = Node
  { nodeId :: Key
  , nodeIP :: T.Text
  , nodePort :: Word16
  } deriving (Eq, Show)

