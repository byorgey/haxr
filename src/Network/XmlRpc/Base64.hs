-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XmlRpc.Base64
-- Copyright   :  (c) Bjorn Bringert 2003
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (requires extensions and non-portable libraries)
--
-- Base64 encoder/decoder and wrapper type.
--
-----------------------------------------------------------------------------
module Network.XmlRpc.Base64 (
    Base64(..)
  , encode
  , decode
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64

newtype Base64 = Base64 { unBase64 :: ByteString }
  deriving (Show, Eq)

encode :: ByteString -> Base64
encode = Base64 . B64.encode

decode :: Base64 -> ByteString
decode (Base64 a) = B64.decodeLenient a
