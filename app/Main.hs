module Main where

import qualified Kademlia as K (kBucketSize)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print $ K.kBucketSize
