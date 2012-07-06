module Network.XmlRpc.Base64 (
    encode,
    decode
) where

import qualified Data.ByteString.Base64 as B64

encode = B64.encode
decode = B64.decodeLenient
