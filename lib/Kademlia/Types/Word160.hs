{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Kademlia.Types.Word160
  ( Word160(..)
  , toWord160
  , fromWord160
  , toBS
  , fromBS
  ) where

import Data.Bits
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Word
import Data.String (IsString(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Text.Printf (printf)
import Data.List (unfoldr)
import Data.Binary
import Control.Monad (replicateM)
import Kademlia.SerDe

newtype Word160 = Word160 (Vector Word8)
  deriving (Eq, Ord)

class ToWord160 a where
  toWord160 :: a -> Word160

class FromWord160 a where
  fromWord160 :: Word160 -> a

instance ToWord160 Word160 where
  toWord160 = id

instance FromWord160 Word160 where
  fromWord160 = id

instance ToByteString Word160 where
  toBS (Word160 v) = BS.pack $ V.toList v

instance FromByteString Word160 where
  fromBS bs = Word160 . V.fromList . BS.unpack . BS.take 20 $ padded
    where padded = BS.append bs (BS.replicate (20 - BS.length bs) 0)

instance Show Word160 where
  show (Word160 v) = "0x" ++ concatMap (printf "%02x") (V.toList v)

instance (a ~ Word8) => ToWord160 [a] where
  toWord160 = Word160 . V.fromList . take 20 . (++ repeat 0)

instance (a ~ Word8) => FromWord160 [a] where
  fromWord160 (Word160 v) = V.toList v

instance ToWord160 BS.ByteString where
  toWord160 = fromBS

instance FromWord160 BS.ByteString where
  fromWord160 = toBS

instance Binary Word160 where
  put (Word160 v) = mapM_ put (V.toList v)
  get = Word160 . V.fromList <$> replicateM 20 get

instance Enum Word160 where
  toEnum i
    | i >= 0 && i <= fromIntegral (maxBound :: Word160) = fromInteger . toInteger $ i
    | otherwise = error $ "Kademlia.Types.Word160.toEnum{" ++ show i ++ "is outside bounds}"
  fromEnum i
    | i <= fromIntegral (maxBound :: Int) = fromIntegral . toInteger $ i
    | otherwise = error $ "Kademlia.Types.Word160.fromEnum{" ++ show i ++ "is outside bounds}"

instance Bounded Word160 where
  minBound = Word160 $ V.replicate 20 (0 :: Word8)
  maxBound = Word160 $ V.replicate 20 (0xFF :: Word8)

instance IsString Word160 where
  fromString = fromBS . C8.pack

instance Num Word160 where
  (Word160 a) + (Word160 b) = Word160 $ V.zipWith (\x y -> fromIntegral $ (fromIntegral x :: Word16) + (fromIntegral y :: Word16)) a b
  (Word160 a) - (Word160 b) = Word160 $ V.zipWith (\x y -> fromIntegral $ (fromIntegral x :: Word16) - (fromIntegral y :: Word16)) a b
  (Word160 a) * (Word160 b) = toWord160 $ take 20 $ multiply (V.toList a) (V.toList b)
    where
      multiply xs ys = [sum [fromIntegral (xs !! i) * fromIntegral (ys !! j) | (i, j) <- zip [0..k] [k,k-1..0]] | k <- [0..38]]
  negate (Word160 a) = Word160 $ V.map negate a
  abs = id
  signum (Word160 a) = if V.all (== 0) a then 0 else 1
  fromInteger n = toWord160 $ take 20 $ map fromIntegral word8s
    where
      word8s :: [Word8]
      word8s = unfoldr (\x -> if x == 0 then Nothing else Just (fromIntegral (x `mod` 256), x `div` 256)) n

instance Bits Word160 where
  (Word160 a) .&. (Word160 b) = Word160 $ V.zipWith (.&.) a b
  (Word160 a) .|. (Word160 b) = Word160 $ V.zipWith (.|.) a b
  xor (Word160 a) (Word160 b) = Word160 $ V.zipWith xor a b
  complement (Word160 a) = Word160 $ V.map complement a
  shift (Word160 a) n
    | n >= 0    = Word160 $ V.drop n a <> V.replicate n 0
    | otherwise = Word160 $ V.replicate (-n) 0 <> V.take (20 + n) a
  rotate (Word160 a) n = Word160 $ V.drop n' a <> V.take n' a
    where n' = n `mod` 160
  bitSize _ = 160
  bitSizeMaybe _ = Just 160
  isSigned _ = False
  testBit (Word160 a) n = testBit (a V.! (n `div` 8)) (n `mod` 8)
  bit n = Word160 $ V.generate 20 (\i -> if i == n `div` 8 then bit (n `mod` 8) else 0)
  popCount (Word160 a) = sum $ map popCount $ V.toList a

instance Integral Word160 where
  quotRem a b = (fromInteger q, fromInteger r)
    where (q, r) = quotRem (toInteger a) (toInteger b)
  toInteger (Word160 a) = foldr (\x acc -> acc * 256 + toInteger x) 0 (V.toList a)

instance Real Word160 where
  toRational = toRational . toInteger