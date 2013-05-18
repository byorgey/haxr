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
     Remote
    ) where

import qualified Network.XmlRpc.Base64      as Base64
import           Network.XmlRpc.Internals

import           Control.Exception          (handleJust)
import           Data.Char
import           Data.Maybe
import           Data.Word                  (Word8)
import           Network.Socket             (withSocketsDo)
import           Network.URI

import           Network.HTTP
import           Network.Stream

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL (ByteString, toChunks)
import qualified Data.ByteString.UTF8       as U

-- | Gets the return value from a method response.
--   Throws an exception if the response was a fault.
handleResponse :: Monad m => MethodResponse -> m Value
handleResponse (Return v) = return v
handleResponse (Fault code str) = fail ("Error " ++ show code ++ ": " ++ str)

-- | Sends a method call to a server and returns the response.
--   Throws an exception if the response was an error.
doCall :: String -> [Header] -> MethodCall -> Err IO MethodResponse
doCall url headers mc =
    do
    let req = renderCall mc
    --FIXME: remove
    --putStrLn req
    resp <- ioErrorToErr $ post url headers req
    --FIXME: remove
    --putStrLn resp
    parseResponse resp

-- | Low-level method calling function. Use this function if
--   you need to do custom conversions between XML-RPC types and
--   Haskell types.
--   Throws an exception if the response was a fault.
call :: String -- ^ URL for the XML-RPC server.
     -> String -- ^ Method name.
     -> [Value] -- ^ The arguments.
     -> Err IO Value -- ^ The result
call url method args = doCall url [] (MethodCall method args) >>= handleResponse

-- | Low-level method calling function. Use this function if
--   you need to do custom conversions between XML-RPC types and
--   Haskell types. Takes a list of extra headers to add to the
--   HTTP request.
--   Throws an exception if the response was a fault.
callWithHeaders :: String -- ^ URL for the XML-RPC server.
                -> String -- ^ Method name.
                -> [Header] -- ^ Extra headers to add to HTTP request.
                -> [Value] -- ^ The arguments.
                -> Err IO Value -- ^ The result
callWithHeaders url method headers args =
    doCall url headers (MethodCall method args) >>= handleResponse


-- | Call a remote method.
remote :: Remote a =>
          String -- ^ Server URL. May contain username and password on
                 --   the format username:password\@ before the hostname.
       -> String -- ^ Remote method name.
       -> a      -- ^ Any function
                 -- @(XmlRpcType t1, ..., XmlRpcType tn, XmlRpcType r) =>
                 -- t1 -> ... -> tn -> IO r@
remote u m = remote_ (\e -> "Error calling " ++ m ++ ": " ++ e) (call u m)

-- | Call a remote method. Takes a list of extra headers to add to the HTTP
--   request.
remoteWithHeaders :: Remote a =>
                     String   -- ^ Server URL. May contain username and password on
                              --   the format username:password\@ before the hostname.
                  -> String   -- ^ Remote method name.
                  -> [Header] -- ^ Extra headers to add to HTTP request.
                  -> a        -- ^ Any function
                              -- @(XmlRpcType t1, ..., XmlRpcType tn, XmlRpcType r) =>
                              -- t1 -> ... -> tn -> IO r@
remoteWithHeaders u m headers =
    remote_ (\e -> "Error calling " ++ m ++ ": " ++ e)
            (callWithHeaders u m headers)

class Remote a where
    remote_ :: (String -> String)        -- ^ Will be applied to all error
                                         --   messages.
            -> ([Value] -> Err IO Value)
            -> a

instance XmlRpcType a => Remote (IO a) where
    remote_ h f = handleError (fail . h) $ f [] >>= fromValue

instance (XmlRpcType a, Remote b) => Remote (a -> b) where
    remote_ h f x = remote_ h (\xs -> f (toValue x:xs))



--
-- HTTP functions
--

userAgent :: String
userAgent = "Haskell XmlRpcClient/0.1"

-- | Handle connection errors.
handleE :: Monad m => (ConnError -> m a) -> Either ConnError a -> m a
handleE h (Left e) = h e
handleE _ (Right v) = return v

-- | Post some content to a uri, return the content of the response
--   or an error.
-- FIXME: should we really use fail?
post :: String -> [Header] -> BSL.ByteString -> IO String
post url headers content = do
    uri <- maybeFail ("Bad URI: '" ++ url ++ "'") (parseURI url)
    let a = uriAuthority uri
    auth <- maybeFail ("Bad URI authority: '" ++ show (fmap showAuth a) ++ "'") a
    post_ uri auth headers content
  where showAuth (URIAuth u r p) = "URIAuth "++u++" "++r++" "++p

-- | Post some content to a uri, return the content of the response
--   or an error.
-- FIXME: should we really use fail?
post_ :: URI -> URIAuth -> [Header] -> BSL.ByteString -> IO String
post_ uri auth headers content =
    do
    eresp <- simpleHTTP (request uri auth headers (BS.concat . BSL.toChunks $ content))
    resp <- handleE (fail . show) eresp
    case rspCode resp of
                      (2,0,0) -> return (U.toString (rspBody resp))
                      _ -> fail (httpError resp)
    where
    showRspCode (a,b,c) = map intToDigit [a,b,c]
    httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp

-- | Create an XML-RPC compliant HTTP request.
request :: URI -> URIAuth -> [Header] -> BS.ByteString -> Request BS.ByteString
request uri auth usrHeaders content = Request{ rqURI = uri,
                                               rqMethod = POST,
                                               rqHeaders = headers,
                                               rqBody = content }
    where
    -- the HTTP module adds a Host header based on the URI
    headers = [Header HdrUserAgent userAgent,
               Header HdrContentType "text/xml",
               Header HdrContentLength (show (BS.length content))
              ] ++ maybeToList (uncurry authHdr . parseUserInfo $ auth)
                ++ usrHeaders
    parseUserInfo info = let (u,pw) = break (==':') $ uriUserInfo info
                         in ( if null u then Nothing else Just u
                            , if null pw then Nothing else Just (tail pw))

-- | Creates an Authorization header using the Basic scheme,
--   see RFC 2617 section 2.
authHdr :: Maybe String -- ^ User name, if any
        -> Maybe String -- ^ Password, if any
        -> Maybe Header -- ^ If user name or password was given, returns
                        --   an Authorization header, otherwise 'Nothing'
authHdr Nothing Nothing = Nothing
authHdr u p = Just (Header HdrAuthorization ("Basic " ++ base64encode user_pass))
        where user_pass    = fromMaybe "" u ++ ":" ++ fromMaybe "" p
              base64encode = BS.unpack . Base64.encode . BS.pack

--
-- Utility functions
--

maybeFail :: Monad m => String -> Maybe a -> m a
maybeFail msg = maybe (fail msg) return
