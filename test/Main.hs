module Main (main) where

import Test.Tasty
import qualified Spec.Kademlia
import qualified Spec.Word160
import qualified Spec.KBucket
import qualified Spec.RoutingTable
import qualified Spec.Block

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" $
  [ Spec.Kademlia.tests
  , Spec.Word160.tests
  , Spec.KBucket.tests
  , Spec.RoutingTable.tests
  , Spec.Block.tests
  ]