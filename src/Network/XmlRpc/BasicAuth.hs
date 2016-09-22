-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XmlRpc.BasicAuth
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (requires extensions and non-portable libraries)
--
-- HTTP Basic Auth parser for URI and header generator.
--
-----------------------------------------------------------------------------

module Network.XmlRpc.BasicAuth (
    RequestHeaders
  , auth
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as B64
import           Network.HTTP.Types.Header (RequestHeaders)
import           Data.Monoid
import           Network.URI

auth :: String -> RequestHeaders
auth url = case parseURI url >>= uriAuthority of
    Nothing -> []
    Just (URIAuth [] _ _) -> [] 
    Just (URIAuth u _ _) -> [("Authorization", "Basic " <> B64.encode (BS.pack (init u)))] 
