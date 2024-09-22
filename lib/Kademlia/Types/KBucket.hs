module Kademlia.Types.KBucket
  ( KBucket (..)
  , kBucketSize
  , empty
  , singleton
  , insert
  , pop
  , member
  , size
  )
where

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

kBucketSize :: Int
kBucketSize = 20  -- k parameter from the paper

data KBucket p k v = KBucket
  { mems :: M.Map (p, k) v
  , prio :: M.Map k p
  }

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

pop :: (Ord p, Ord k) => KBucket p k v -> Maybe (v, KBucket p k v)
pop (KBucket m ps)
  | M.null m = Nothing
  | otherwise = Just (v, KBucket { mems = m', prio = ps'})
    where
      (((_, k), v), m') = M.deleteFindMin m
      ps' = M.delete k ps

member :: Ord k => k -> KBucket p k v -> Bool
member k = M.member k . prio

size :: KBucket p k v -> Int
size = M.size . prio

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