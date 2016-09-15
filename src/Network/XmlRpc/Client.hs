-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XmlRpc.Client
-- Copyright   :  (c) Bjorn Bringert 2003
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (requires extensions and non-portable libraries)
--
-- This module contains the client functionality of XML-RPC.
-- The XML-RPC specifcation is available at <http://www.xmlrpc.com/spec>.
--
-- A simple client application:
--
-- > import Network.XmlRpc.Client
-- >
-- > server = "http://localhost/~bjorn/cgi-bin/simple_server"
-- >
-- > add :: String -> Int -> Int -> IO Int
-- > add url = remote url "examples.add"
-- >
-- > main = do
-- >        let x = 4
-- >            y = 7
-- >        z <- add server x y
-- >        putStrLn (show x ++ " + " ++ show y ++ " = " ++ show z)
--
-----------------------------------------------------------------------------

module Network.XmlRpc.Client
    (
     remote, remoteWithHeaders,
     call, callWithHeaders,
     Remote, URL
    ) where

import Network.XmlRpc.BasicAuth
import Network.XmlRpc.Internals
import Network.HTTP.Client.TLS
import Network.HTTP.Client

-- | URL type is a String synonym
type URL = String

-- | Gets the return value from a method response.
--   Throws an exception if the response was a fault.
handleResponse :: Monad m => MethodResponse -> m Param
handleResponse (Fault code str) = fail ("Error " ++ show code ++ ": " ++ show str)
handleResponse (Return v)       = return v

-- | Sends a method call to a server and returns the response.
--   Throws an exception if the response was an error.
doCall :: URL -> RequestHeaders -> MethodCall -> IO MethodResponse
doCall url headers mc = do
    -- Prepare request
    req <- parseRequest url
    let req' = req { method = "POST"
                   , requestBody = RequestBodyBS (renderXml mc)
                   , requestHeaders = headers ++ headers' } 
        headers' = ("Content-Type", "text/xml") : auth url

    -- Request with TLS manager
    manager <- newManager tlsManagerSettings
    resp <- httpLbs req' manager
    parseXml "methodResponse" (responseBody resp)

-- | Low-level method calling function. Use this function if
--   you need to do custom conversions between XML-RPC types and
--   Haskell types.
--   Throws an exception if the response was a fault.
call :: URL         -- ^ URL for the XML-RPC server.
     -> MethodName  -- ^ Method name.
     -> [Value]     -- ^ The arguments.
     -> IO Value    -- ^ The result
call url name args = doCall url [] (MethodCall name args) >>= handleResponse

-- | Low-level method calling function. Use this function if
--   you need to do custom conversions between XML-RPC types and
--   Haskell types. Takes a list of extra headers to add to the
--   HTTP request.
--   Throws an exception if the response was a fault.
callWithHeaders :: URL            -- ^ URL for the XML-RPC server.
                -> MethodName     -- ^ Method name.
                -> RequestHeaders -- ^ Extra headers to add to HTTP request.
                -> [Value]        -- ^ The arguments.
                -> IO Value       -- ^ The result
callWithHeaders url name headers args =
    doCall url headers (MethodCall name args) >>= handleResponse

-- | Call a remote method.
remote :: Remote a =>
          URL        -- ^ Server URL. May contain username and password on
                     --   the format username:password\@ before the hostname.
       -> MethodName -- ^ Remote method name.
       -> a          -- ^ Any function
                     -- @(XmlRpcType t1, ..., XmlRpcType tn, XmlRpcType r) =>
                     -- t1 -> ... -> tn -> IO r@
remote u m = remote_ (call u m)

-- | Call a remote method. Takes a list of extra headers to add to the HTTP
--   request.
remoteWithHeaders :: Remote a =>
                     URL            -- ^ Server URL. May contain username and password on
                                    --   the format username:password\@ before the hostname.
                  -> MethodName     -- ^ Remote method name.
                  -> RequestHeaders -- ^ Extra headers to add to HTTP request.
                  -> a              -- ^ Any function
                                    -- @(XmlRpcType t1, ..., XmlRpcType tn, XmlRpcType r) =>
                                    -- t1 -> ... -> tn -> IO r@
remoteWithHeaders u m headers = remote_ (callWithHeaders u m headers)

class Remote a where
    remote_ :: ([Value] -> IO Value) -> a

instance XmlRpcType a => Remote (IO a) where
    remote_ f = do
        v <- f []
        case fromValue v of
            Just v' -> return v'
            Nothing -> fail ("Unable to convert from " ++ show v)

instance (XmlRpcType a, Remote b) => Remote (a -> b) where
    remote_ f x = remote_ (\xs -> f (toValue x : xs))
