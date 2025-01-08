module Kademlia.Metric where

class Metric a where
  distance :: Num b => a -> a -> b