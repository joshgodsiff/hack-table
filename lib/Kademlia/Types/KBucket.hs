module Kademlia.Types.KBucket where

import qualified Data.FingerTree as FT
import Data.FingerTree (FingerTree, (<|), (|>), (><))

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List (unfoldr)
import Data.Arrow ((***))

kBucketSize :: Int
kBucketSize = 20  -- k parameter from the paper

data Entry p k v = Entry p k v

instance Functor (Entry p k) where
    fmap f (Entry p k v) = Entry p k (f v)

instance Foldable (Entry p k) where
  foldMap f (Entry _ _ v) = f v

---------------------------------------------------------------

data Prio p k v = NoPrio | Prio p k v

instance Ord p => Semigroup (Prio p k v) where
  (<>) = unionPrio

instance Ord k => Monoid (Prio k v) where
  mempty  = NoPrio
  mappend = unionPrio

unionPrio :: Ord p => Prio p k v -> Prio p k v -> Prio p k v
x `unionPrio` NoPrio      = x
NoPrio `unionPrio` y      = y
x@(Prio px _ _) `unionPrio` y@(Prio py _ _)
  | px <= py            = x
  | otherwise           = y

instance Measured (Prio p k v) (Entry p k v) where
  measure (Entry p k v) = Prio p k v

---------------------------------------------------------------

newtype KBucket p k v = KBucket (FT.FingerTree (Prio p k v) (Entry p k v))
  deriving (Show)

instance Ord p => Functor (KBucket p k) where
    fmap f (KBucket xs) = KBucket (FT.fmap' (fmap f) xs)

-- | In ascending order of keys.
instance Ord p => Foldable (KBucket p k) where
    foldMap f q = case minView q of
        Nothing -> mempty
        Just (v, q') -> f v `mappend` foldMap f q'
    null (PQueue q) = FT.null q

instance Ord p => Semigroup (KBucket p k v) where
    (<>) = union

-- | 'empty' and 'union'
instance Ord p => Monoid (KBucket p k v) where
    mempty = empty
    mappend = union

instance (Ord p, Eq k, Eq v) => Eq (KBucket p k v) where
    xs == ys = assocs xs == assocs ys

minView :: Ord p => KBucket p k v -> Maybe (v, KBucket p k v)
minView q = fmap (snd *** id) (minViewWithKey q)

minViewWithKey :: Ord p => KBucket p k v -> Maybe ((p, k, v), KBucket p k v)
minViewWithKey (KBucket q)
  | FT.null q = Nothing
  | otherwise = Just ((p, k, v), case FT.viewl r of
    _ :< r' -> KBucket (l >< r')
    _ -> error "can't happen")
  where
    Prio p k v = measure q
    (l, r) = FT.split (below p) q

union :: Ord p => KBucket p k v -> KBucket p k v -> KBucket p k v
union (KBucket xs) (KBucket ys) = KBucket (xs >< ys)

assocs :: Ord p => KBucket p k v -> [(p, k, v)]
assocs = unfoldr minViewWithKey