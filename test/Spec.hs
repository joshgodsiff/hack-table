module Main (main) where

import Test.Tasty
import qualified KademliaSpec
import qualified Word160Spec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" $
  [ KademliaSpec.tests
  , Word160Spec.tests
  ]