module Spec.KBucket where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog
import qualified Data.Map as M
import Kademlia.Types.KBucket as KB
import Control.Monad (forM_)
import qualified Data.Set as S


tests :: TestTree
tests = testGroup "KBucket Tests"
  [ 
  ]