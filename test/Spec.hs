module Main (main) where

import Test.Tasty
import qualified KademliaSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [KademliaSpec.tests]