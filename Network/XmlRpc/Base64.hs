module Network.XmlRpc.Base64 (
    encode,
    decode
) where

import qualified Codec.Binary.Base64 as B64
import Data.Maybe

encode = B64.encode
decode = fromJust . B64.decode
