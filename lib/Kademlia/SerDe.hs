module Kademlia.SerDe where

import Kademlia.Types.Word160 (Word160 (..))
import qualified Data.ByteString as BS


class ToByteString a where
  toBS :: a -> BS.ByteString

class FromByteString a where
  fromBS :: BS.ByteString -> a

instance ToByteString BS.ByteString where
  toBS = id

instance FromByteString BS.ByteString where
  fromBS = id




