module Kademlia.Metric where

class Metric a where
  distance :: a -> a -> Integer