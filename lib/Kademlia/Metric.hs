module Kademlia.Distance where

class Metric a where
  distance :: a -> a -> Integer