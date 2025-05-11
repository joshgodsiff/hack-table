module Gen.Kademlia where

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog
import qualified Data.Text as T

import Kademlia.Types.Key
import Kademlia.Types.Node
import qualified Kademlia.Types.KBucket as KB
import Kademlia.SerDe
import Kademlia.Types.Word160

genWord160 :: Gen Word160
genWord160 = fromBS <$> Gen.bytes (Range.singleton 20)

-- Generator for Key
genKey :: Gen Key
genKey = fromBS <$> Gen.bytes (Range.singleton 20)

-- Generator for Node
genNode :: Gen Node
genNode = Node
  <$> genKey
  <*> (T.pack <$> Gen.string (Range.linear 7 15) Gen.alphaNum)
  <*> Gen.word16 Range.constantBounded

genPriority :: Gen Int
genPriority = Gen.integral (Range.linear 0 1000)

genKBucketTriplet :: Gen (Int, Key, Node)
genKBucketTriplet = do
  p <- genPriority
  node <- genNode
  let k = nodeId node
  pure $ (p, k, node)

genKBucket :: Range Int -> Gen (KB.KBucket Int Key Node)
genKBucket r = do
  triplets <- Gen.list r genKBucketTriplet
  pure $ KB.fromList triplets