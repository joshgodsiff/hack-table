module Main (main) where

import Test.Tasty
import qualified Spec.Kademlia
import qualified Spec.Word160
import qualified Spec.KBucket

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" $
  [ Spec.Kademlia.tests
  , Spec.Word160.tests
  , Spec.KBucket.tests
  ]