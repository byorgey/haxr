module Network.XmlRpc.Base64 (
    Base64(..)
  , encode
  , decode
) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as B64

newtype Base64 = Base64 { unBase64 :: ByteString }
  deriving (Show, Eq)

encode :: ByteString -> Base64
encode = Base64 . B64.encode

decode :: Base64 -> ByteString
decode (Base64 a) = B64.decodeLenient a
