module Kademlia.Types.KBucket
  ( KBucket (..)
  , kBucketSize
  , empty
  , singleton
  , insert
  , minView
  , minViewWithKey
  , maxView
  , maxViewWithKey
  , member
  , Kademlia.Types.KBucket.null
  , partition
  , size
  , fromList
  , toAscList
  , toAscListValues
  , full
  )
where

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import qualified Data.Foldable as F
import Data.List (unfoldr)

kBucketSize :: Int
kBucketSize = 20  -- ^ k parameter for k-bucket size (hard coded to 20 for now)

-- | The KBucket data type represents a Kademlia k-bucket, which stores nodes in the DHT
-- The type parameters are:
--   p: priority type (usually a timestamp or sequence number)
--   k: key type (usually a NodeId)
--   v: value type (usually node contact information)
data KBucket p k v = KBucket
  { mems :: M.Map (p, k) v  -- ^ Maps (priority, key) pairs to values, allowing multiple entries with same key but different priorities
  , prio :: M.Map k p       -- ^ Maps keys to their current priority, used to track the most recent priority for each key
  } deriving (Show)

empty :: KBucket p k v
empty = KBucket
  { mems = M.empty
  , prio = M.empty
  }

singleton :: p -> k -> v -> KBucket p k v
singleton p k v = KBucket
  { mems = M.singleton (p, k) v
  , prio = M.singleton k p
  }

insert :: (Ord p, Ord k) => p -> k -> v -> KBucket p k v -> KBucket p k v
insert p k v kb
  | member k kb                                = adjustP p k kb
  | not (member k kb) && size kb < kBucketSize = unsafeInsert p k v kb
  | otherwise = kb -- Bucket full, intentionally ignore input

minView :: (Ord p, Ord k) => KBucket p k v -> Maybe (v, KBucket p k v)
minView = fmap justValue . minViewWithKey

minViewWithKey :: (Ord p, Ord k) => KBucket p k v -> Maybe ((p, k, v), KBucket p k v)
minViewWithKey (KBucket m ps)
  | M.null m = Nothing
  | otherwise = Just ((p, k, v), KBucket { mems = m', prio = M.delete k ps})
    where (((p, k), v), m') = M.deleteFindMin m

maxView :: (Ord p, Ord k) => KBucket p k v -> Maybe (v, KBucket p k v)
maxView = fmap justValue . maxViewWithKey

maxViewWithKey :: (Ord p, Ord k) => KBucket p k v -> Maybe ((p, k, v), KBucket p k v)
maxViewWithKey (KBucket m ps)
  | M.null m = Nothing
  | otherwise = Just ((p, k, v), KBucket { mems = m', prio = M.delete k ps})
    where (((p, k), v), m') = M.deleteFindMax m

member :: Ord k => k -> KBucket p k v -> Bool
member k = M.member k . prio

size :: KBucket p k v -> Int
size = M.size . prio

null :: KBucket p k v -> Bool
null = M.null . prio

fromList :: (Ord p, Ord k) =>  [(p, k, v)] -> KBucket p k v
fromList xs = F.foldl' ins empty xs
  where ins kb (p, k, v) = insert p k v kb

toAscListValues :: (Ord p, Ord k) => KBucket p k v -> [v]
toAscListValues = unfoldr minView

toAscList :: (Ord p, Ord k) => KBucket p k v -> [(p, k, v)]
toAscList = unfoldr minViewWithKey

-- | Partition a k-bucket into two based on a predicate
partition :: (Ord k) => (k -> Bool) -> KBucket p k v -> (KBucket p k v, KBucket p k v)
partition p (KBucket m ps) = (KBucket lmems lprio, KBucket rmems rprio)
  where
    (lmems, rmems) = M.partitionWithKey (\(_, k) _ -> p k) m
    (lprio, rprio) = M.partitionWithKey (\k _ -> p k) ps

full :: KBucket p k v -> Bool
full = (>= kBucketSize) . size

----------------------------------------------------------------
-- Private functions --
----------------------------------------------------------------

unsafeInsert :: (Ord p, Ord k) => p -> k -> v -> KBucket p k v -> KBucket p k v
unsafeInsert p k v (KBucket m ps)
  = KBucket
  { mems = M.insert (p, k) v m
  , prio = M.insert k p ps
  }

adjustP :: (Ord p, Ord k) => p -> k -> KBucket p k v -> KBucket p k v
adjustP p k kb@(KBucket m ps)
  | member k kb && (ps ! k == p) = kb
  | member k kb = let
      currentP = ps ! k
      v = m ! (currentP, k)
      deleted = M.delete (currentP, k) m
    in unsafeInsert p k v (KBucket { mems = deleted, prio = ps })
  | otherwise = kb -- No-op. Caller is a dumb-dumb.

justValue :: ((a,b,c), x) -> (c, x)
justValue ((_,_,c), x) = (c, x)