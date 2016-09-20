{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XmlRpc.Server
-- Copyright   :  (c) Bjorn Bringert 2003
--                (c) Alexaner Krupenkin 2016
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (requires extensions and non-portable libraries)
--
-- This module contains the server functionality of XML-RPC.
-- The XML-RPC specifcation is available at <http://www.xmlrpc.com/spec>.
--
-- A simple CGI-based XML-RPC server application:
--
-- > import Network.XmlRpc.Server
-- >
-- > add :: Int -> Int -> IO Int
-- > add x y = return (x + y)
-- >
-- > main = cgiXmlRpcServer [("examples.add", fun add)]
-----------------------------------------------------------------------------

module Network.XmlRpc.Server (
  -- * Basic method types
    XmlRpcMethod(..)
  , Signature(..)
  , Methods
  -- * Snap HTTP server
  , serve
  , xmlRpcServer
  -- * XML-RPC method creator
  , fun
  ) where

import           Network.XmlRpc.Internals

import           Control.Monad.IO.Class (liftIO)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Monoid (mempty)
import           Control.Exception
import           Snap.Http.Server
import qualified Data.Text as T
import           Data.Map (Map)
import qualified Data.Map as M
import           Snap.Core

-- Properties
maxBodySize :: Num a => a
maxBodySize = 1048576 * 10 -- 10 MiB is max request size

--
-- API
--

-- | Method signature (args types, return type)
data Signature = Signature
  { arguments :: [Type]
  , result    :: Type
  } deriving Show

-- | Method function, take 'MethodCall' structure and return 'MehtodResponse'
type MethodFun = MethodCall -> Err IO MethodResponse

-- | The type of XML-RPC methods on the server.
data XmlRpcMethod = XmlRpcMethod
  { runMethod :: MethodFun
  , signature :: Signature
  }

-- | The type of XML-RPC exported methods.
type Methods = Map MethodName XmlRpcMethod

--
-- Converting Haskell functions to XML-RPC methods
--

-- | Turns any function
--   @(XmlRpcType t1, ..., XmlRpcType tn, XmlRpcType r) =>
--   t1 -> ... -> tn -> IO r@
--   into an 'XmlRpcMethod'
fun :: XmlRpcFun a => a -> XmlRpcMethod
fun f = XmlRpcMethod (toFun f) (sig f)

class XmlRpcFun a where
    toFun :: a -> MethodFun
    sig   :: a -> Signature

instance XmlRpcType a => XmlRpcFun (IO a) where
    toFun x (MethodCall _ []) =
        liftIO (try x) >>=
            either showException (return . Return . Param . toValue)
      where showException :: (Monad m, MonadError Text m) => SomeException -> m a
#if MIN_VERSION_base(4,8,0)
            showException = throwError . T.pack . displayException
#else
            showException = throwError . T.pack . show
#endif

    toFun _ _ = fail "Too many arguments"

    sig x = Signature [] (getType (mType x))

instance (XmlRpcType a, XmlRpcFun b) => XmlRpcFun (a -> b) where
    toFun f (MethodCall n (x : xs)) = do v <- fromValue (unParam x)
                                         toFun (f v) (MethodCall n xs)
    toFun _ _ = fail "Too few arguments"

    sig f = let (a, b)         = funType f
                Signature as r = sig b
             in Signature (getType a : as) r

mType :: m a -> a
mType _ = undefined

funType :: (a -> b) -> (a, b)
funType _ = (undefined, undefined)

--
-- Server
--

-- | Create a 'Snap' by given 'Methods'
serve :: Methods -> Snap ()
serve ms = do
    -- Read request
    req <- readRequestBody maxBodySize

    -- Lift call execution
    x <- liftIO $ runExceptT $ do
            -- Parse method request
            c@(MethodCall name _) <- parseXml "methodCall" req

            -- Lookup and run method
            case M.lookup name ms of
                Nothing -> return (Fault 404 $ "Method '" <> name <> "' not found!")
                Just m  -> runMethod m c

    -- Log error or write response
    either (logError . encodeUtf8) (writeBS . renderXml) x

-- | Run XML-RPC server
xmlRpcServer :: Int     -- ^ Server port
             -> Methods -- ^ Exported methods
             -> IO ()
xmlRpcServer port = simpleHttpServe config . serve
  where config :: Config Snap a
        config = setPort port mempty
