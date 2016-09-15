module Network.XmlRpc.Introspect where

import Network.XmlRpc.Client
import Network.XmlRpc.Internals
import Data.Text (Text)

type Signature = ([Type],Type)
type Help = Text
type MethodInfo = (Text,[Signature],Help)

-- Primitive introspection functions

listMethods :: URL -> IO [Text]
listMethods url = remote url "system.listMethods"

methodSignature :: URL -> Text -> IO [[String]]
methodSignature url = remote url "system.methodSignature"

methodHelp :: URL -> Text -> IO Text
methodHelp url = remote url "system.methodHelp"


signatures :: URL -> Text -> IO [Signature]
signatures url name = do
                      sigs <- methodSignature url name
                      return [ (map read as, read r) | (r:as) <- sigs ]

methodInfo :: URL -> Text -> IO MethodInfo
methodInfo url name = do
                      sigs <- signatures url name
                      help <- methodHelp url name
                      return (name, sigs, help)
