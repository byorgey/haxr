{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XmlRpc.Internals
-- Copyright   :  (c) Bjorn Bringert 2003
--                (c) Alexander Krupenkin 2016
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (requires extensions and non-portable libraries)
--
-- This module contains the core functionality of the XML-RPC library.
-- Most applications should not need to use this module. Client
-- applications should use "Network.XmlRpc.Client" and server applications should
-- use "Network.XmlRpc.Server".
--
-- The XML-RPC specification is available at <http://www.xmlrpc.com/spec>.
--
-----------------------------------------------------------------------------

module Network.XmlRpc.Internals (
-- * Err staff
Err, handleErr, lift,
MonadError(..), runExceptT,
-- * Text utility
Text, showText, readText, (<>),
-- * XML-RPC types
Value(..), Type(..), XmlRpcType(..),
getField,
-- * Method calls and repsonses
MethodCall(..), MethodResponse(..),
MethodName, Param,
-- * Converting
parseXml, renderXml,
) where

import           Control.Monad ((>=>), liftM, liftM2, liftM3, liftM4, liftM5)
import           Data.Typeable (Typeable, typeOf, typeRepTyCon, tyConName)

#if MIN_VERSION_mtl(2,2,1)
import           Control.Monad.Except (MonadError(throwError, catchError),
                                       ExceptT, runExceptT, lift)
#else
import           Control.Monad.Error (MonadError(throwError, catchError),
                                      ExceptT, runExceptT, lift)
#endif

import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Network.XmlRpc.Base64 (Base64(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import           Text.Read (readMaybe)
import           Data.Char (isSpace)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Time

import           Text.HTML.TagSoup.Tree
import           Text.HTML.TagSoup

-- | The format for \"dateTime.iso8601\"
xmlRpcDateFormat :: String
xmlRpcDateFormat = "%Y%m%dT%H:%M:%S"


--
-- Error monad stuff
--

type Err m a = ExceptT Text m a

-- | Handle errors from the error monad.
handleErr :: Monad m => (Text -> m a) -> Err m a -> m a
handleErr h m = do
    Right x <- runExceptT (catchError m (lift . h))
    return x

--
-- Text staff
--

-- | Safe read for 'Text'
readText :: (Monad m, MonadError Text m, Read a) => Text -> m a
readText t =
    case readMaybe (T.unpack t) of
        Nothing -> throwError ("Unable to parse: " <> t)
        Just t' -> return t'

-- | Show value to 'Text'
showText :: Show a => a -> Text
showText = T.pack . show

--
-- Types for methods calls and responses
--

-- | An XML-RPC value.
data Value
  = ValueInt !Int
  -- ^ int, i4, or i8
  | ValueBool !Bool
  -- ^ boolean
  | ValueString !Text
  -- ^ string
  | ValueDouble !Double
  -- ^ double
  | ValueDateTime !LocalTime
  -- ^ dateTime.iso8601
  | ValueBase64 !Base64
  -- ^ base64
  -- NOTE that you should provide the raw data;
  -- the haxr library takes care of doing the base-64 encoding.
  | ValueStruct !(Map Text Value)
  -- ^ struct
  | ValueArray ![Value]
  -- ^ array
  | ValueAny !Value
  -- ^ any value type
  deriving Show

-- | An XML-RPC value. Use for error messages and introspection.
data Type
  = TInt
  | TBool
  | TString
  | TDouble
  | TDateTime
  | TBase64
  | TStruct
  | TArray
  | TUnknown
  deriving (Eq, Enum)

instance Show Type where
    show TInt      = "int"
    show TBool     = "bool"
    show TString   = "string"
    show TDouble   = "double"
    show TDateTime = "dateTime.iso8601"
    show TBase64   = "base64"
    show TStruct   = "struct"
    show TArray    = "array"
    show TUnknown  = "unknown"

instance Read Type where
    readsPrec _ s =
        case break isSpace (dropWhile isSpace s) of
            ("i4",r)     -> [(TInt,r)]
            ("i8",r)     -> [(TInt,r)]
            ("int",r)    -> [(TInt,r)]
            ("bool",r)   -> [(TBool,r)]
            ("string",r) -> [(TString,r)]
            ("double",r) -> [(TDouble,r)]
            ("dateTime.iso8601",r) -> [(TDateTime,r)]
            ("base64",r) -> [(TBase64,r)]
            ("struct",r) -> [(TStruct,r)]
            ("array",r)  -> [(TArray,r)]
            (_, r)       -> [(TUnknown,r)]

-- | Get 'Type' of given 'Value'
getValueType :: Value -> Type
getValueType v = case v of
    ValueInt _          -> TInt
    ValueBool _         -> TBool
    ValueString _       -> TString
    ValueDouble _       -> TDouble
    ValueDateTime _     -> TDateTime
    ValueBase64 _       -> TBase64
    ValueStruct _       -> TStruct
    ValueArray _        -> TArray
    ValueAny x          -> getValueType x

-- | An method parameter type.
type Param = Value

-- | An method name.
type MethodName = Text

-- | An XML-RPC method call. Consists of a method name and a list of
--   parameters.
data MethodCall = MethodCall !MethodName ![Param]
  deriving Show -- for debugging

-- | An XML-RPC response.
data MethodResponse
  = Return !Param
  -- ^ A method response returning a value
  | Fault !Int !Text
  -- ^ A fault response
  deriving Show -- for debugging

--
-- Converting to and from XML-RPC types
--

-- | A class for mapping Haskell types to XML-RPC types.
class Typeable a => XmlRpcType a where
    -- | Convert from this type to a 'Value', not at all
    -- types can be a 'Value'
    toValue   :: a -> Value

    -- | Convert from a 'Value' to this type. May fail if
    --   if there is a type error.
    fromValue :: (Monad m, MonadError Text m) => Value -> m a

    -- | Get type of 'XmlRpcType'
    getType   :: a -> Type

typeError :: (Monad m, MonadError Text m, XmlRpcType a) => Value -> m a
typeError v = withType $ \t ->
    throwError $ "Type disparity: XML-RPC '"
               <> showText (getValueType v) <> "', Haskell '"
               <> T.pack (tyConName $ typeRepTyCon $ typeOf t) <> "'"
  where withType :: (a -> m a) -> m a
        withType f = f undefined

instance XmlRpcType Int where
    toValue                = ValueInt
    fromValue (ValueInt x) = return x
    fromValue x            = typeError x
    getType _              = TInt

instance XmlRpcType Integer where
    toValue                = ValueInt . fromIntegral
    fromValue (ValueInt x) = return (fromIntegral x)
    fromValue x            = typeError x
    getType _              = TInt

instance XmlRpcType Double where
    toValue                   = ValueDouble
    fromValue (ValueDouble x) = return x
    fromValue x               = typeError x
    getType _                 = TDouble

instance XmlRpcType Float where
    toValue                   = ValueDouble . realToFrac
    fromValue (ValueDouble x) = return (realToFrac x)
    fromValue x               = typeError x
    getType _                 = TDouble

instance XmlRpcType Bool where
    toValue                 = ValueBool
    fromValue (ValueBool x) = return x
    fromValue x             = typeError x
    getType _               = TBool

instance XmlRpcType Text where
    toValue                      = ValueString
    fromValue (ValueString x)    = return x
    fromValue x                  = typeError x
    getType _                    = TString

instance XmlRpcType String where
    toValue   = ValueString . T.pack
    fromValue = liftM T.unpack . fromValue
    getType _ = TString

instance XmlRpcType Base64 where
    toValue                   = ValueBase64
    fromValue (ValueBase64 x) = return x
    fromValue x               = typeError x
    getType _                 = TBase64

instance XmlRpcType LocalTime where
    toValue                     = ValueDateTime
    fromValue (ValueDateTime x) = return x
    fromValue x                 = typeError x
    getType _                   = TDateTime

-- Monotype array
instance {-# OVERLAPPABLE #-} XmlRpcType a => XmlRpcType [a] where
    toValue                   = ValueArray . fmap toValue
    fromValue (ValueArray xs) = mapM fromValue xs
    fromValue x               = typeError x
    getType _                 = TArray

-- Polytype array
instance XmlRpcType [Value] where
    toValue                   = ValueArray
    fromValue (ValueArray xs) = return xs
    fromValue x               = typeError x
    getType _                 = TArray

-- Monotype struct
instance {-# OVERLAPPABLE #-} XmlRpcType a => XmlRpcType (Map Text a) where
    toValue   = toValue . fmap toValue
    fromValue = fromValue >=> mapM fromValue
    getType _ = TStruct

-- Monotype struct as list
instance {-# OVERLAPPABLE #-} XmlRpcType a => XmlRpcType [(Text, a)] where
    toValue   = toValue . fmap (fmap toValue)
    fromValue = fromValue >=> mapM (mapM fromValue)
    getType _ = TStruct

-- Polytype struct
instance XmlRpcType (Map Text Value) where
    toValue                    = ValueStruct
    fromValue (ValueStruct xs) = return xs
    fromValue x                = typeError x
    getType _                  = TStruct

-- Polytype struct as list
instance XmlRpcType [(Text, Value)] where
    toValue   = toValue . M.fromList
    fromValue = liftM M.toList . fromValue
    getType _ = TStruct

-- | Lookup an item from struct data
getField :: (Monad m, MonadError Text m, XmlRpcType a) => Text -> Map Text Value -> m a
getField k s = do
    v <- case M.lookup k s of
        Nothing -> throwError ("Struct field '" <> k <> "' not found")
        Just x  -> return x
    fromValue v

-- Tuple instances may be used for heterogenous array types.
instance (XmlRpcType a, XmlRpcType b, XmlRpcType c, XmlRpcType d,
          XmlRpcType e) =>
         XmlRpcType (a,b,c,d,e) where
    toValue (v,w,x,y,z) = ValueArray
        [toValue v, toValue w, toValue x, toValue y, toValue z]
    fromValue (ValueArray [v,w,x,y,z]) =
        liftM5 (,,,,) (fromValue v) (fromValue w) (fromValue x)
                      (fromValue y) (fromValue z)
    fromValue x = typeError x
    getType _   = TArray

instance (XmlRpcType a, XmlRpcType b, XmlRpcType c, XmlRpcType d) =>
         XmlRpcType (a,b,c,d) where
    toValue (w,x,y,z) = ValueArray
        [toValue w, toValue x, toValue y, toValue z]
    fromValue (ValueArray [w,x,y,z]) =
        liftM4 (,,,) (fromValue w) (fromValue x) (fromValue y) (fromValue z)
    fromValue x = typeError x
    getType _   = TArray

instance (XmlRpcType a, XmlRpcType b, XmlRpcType c) => XmlRpcType (a,b,c) where
    toValue (x,y,z) = ValueArray [toValue x, toValue y, toValue z]
    fromValue (ValueArray [x,y,z]) =
        liftM3 (,,) (fromValue x) (fromValue y) (fromValue z)
    fromValue x = typeError x
    getType _   = TArray

instance (XmlRpcType a, XmlRpcType b) => XmlRpcType (a,b) where
    toValue (x,y) = ValueArray [toValue x, toValue y]
    fromValue (ValueArray [x,y]) = liftM2 (,) (fromValue x) (fromValue y)
    fromValue x = typeError x
    getType _   = TArray

leafText :: (Monad m, MonadError Text m) => [TagTree Text] -> m Text
leafText [TagLeaf (TagText t)] = return t
leafText x = throwError ("Broken text leaf: " <> renderTree x)

-- | 'Value' parser from XML tags
treeToValue :: (Monad m, MonadError Text m) => TagTree Text -> m Value
treeToValue x@(TagBranch typ _ inner) = do
    t <- readText typ
    case t of
        TInt        -> do numText <- leafText inner
                          liftM ValueInt (readText numText)

        TBool       -> do numText <- leafText inner
                          return (ValueBool $
                              if (numText == "1") then True else False)

        TString     -> do liftM ValueString (leafText inner)

        TDouble     -> do numText <- leafText inner
                          liftM ValueDouble (readText numText)

        TDateTime   -> do dateText <- leafText inner
                          dt <- parseDateTime dateText
                          return (ValueDateTime dt)

        TBase64     -> do b64Text <- leafText inner
                          return (ValueBase64 $ Base64 $ BS.pack $ T.unpack b64Text)

        TStruct     -> do mems <- parseMembers inner
                          return (ValueStruct (M.fromList mems))

        TArray      -> do vals <- parseArray inner
                          return (ValueArray vals)

        TUnknown    -> throwError ("Unknown value: " <> renderTree [x])
treeToValue x = throwError ("Broken XML: " <> renderTree [x])

parseDateTime :: (Monad m, MonadError Text m) => Text -> m LocalTime
parseDateTime t =
    case parse (T.unpack t) of
        Just t' -> return t'
        Nothing -> throwError ("Unable to parse LocalTime: " <> t)
 where parse = parseTimeM True defaultTimeLocale xmlRpcDateFormat

parseMembers :: (Monad m, MonadError Text m) => [TagTree Text] -> m [(Text, Value)]
parseMembers = go []
  where go acc [] = return (reverse acc)
        go acc (TagBranch "member" [] m : xs) =
            case m of
                [TagBranch "name" [] name, TagBranch "value" [] [value]]
                    -> do n <- leafText name
                          v <- treeToValue value
                          go ((n, v) : acc) xs
                b -> throwError ("Broken struct <name> and <value>: " <> renderTree b)
        go _ m = throwError ("Broken <member>: " <> renderTree m)

parseArray :: (Monad m, MonadError Text m) => [TagTree Text] -> m [Value]
parseArray [TagBranch "data" [] values] = mapM go values
  where go (TagBranch "value" [] [v]) = treeToValue v
        go v = throwError ("Broken <value>: " <> renderTree [v])
parseArray d = throwError ("Broken <data>: " <> renderTree d)

-- | Make a 'TagTree' leaf from 'Text'
textLeaf :: Text -> TagTree Text
textLeaf = TagLeaf . TagText

-- | 'Value' to XML tags render
valueToTree :: Value -> TagTree Text
valueToTree v = case v of
    ValueInt x      -> branch [textLeaf $ T.pack $ show x]
    ValueBool x     -> branch [textLeaf $ if x then "1" else "0"]
    ValueString x   -> branch [textLeaf x]
    ValueDouble x   -> branch [textLeaf $ T.pack $ show x]
    ValueDateTime x -> branch [textLeaf $ showDateTime x]
    ValueBase64 x   -> branch [textLeaf $ T.pack $ BS.unpack $ unBase64 x]
    ValueStruct x   -> branch (fmap structMember (M.toList x))
    ValueArray x    -> branch [TagBranch "data" [] $
        fmap (\a -> TagBranch "value" [] [valueToTree a]) x]
    ValueAny x      -> valueToTree x
  where branch   = TagBranch (typeText v) []
        typeText = T.pack . show . getValueType

showDateTime :: LocalTime -> Text
showDateTime = T.pack . formatTime defaultTimeLocale xmlRpcDateFormat

structMember :: (Text, Value) -> TagTree Text
structMember (name, value) =
    TagBranch "member" []
        [ TagBranch "name" [] [textLeaf name]
        , TagBranch "value" [] [valueToTree value] ]

--
-- XML content from / to
--

-- | TagSoup driven XML content encoder/decoder
class XmlContent a where
    toXml   :: a -> TagTree Text
    fromXml :: (Monad m, MonadError Text m) => TagTree Text -> m a

instance {-# OVERLAPPABLE #-} XmlRpcType a => XmlContent a where
    toXml   = valueToTree . toValue
    fromXml = treeToValue >=> fromValue

instance XmlContent Param where
    toXml p =
        TagBranch "param" [] [TagBranch "value" [] [valueToTree p]]

    fromXml (TagBranch "param" [] [p]) =
        case p of
            TagBranch "value" [] [v] -> treeToValue v
            _ -> throwError ("Broken param <value>: " <> renderTree [p])
    fromXml p = throwError ("Broken <param>: " <> renderTree [p])

instance XmlContent MethodCall where
    toXml (MethodCall name params) =
        TagBranch "methodCall" [] $
            TagBranch "methodName" [] [textLeaf name]
            : case params of
                [] -> []
                _  -> [TagBranch "params" [] (fmap toXml params)]

    fromXml (TagBranch "methodCall" [] [n, p]) =
        liftM2 MethodCall (fromXmlName n) (fromXmlParams p)

      where fromXmlName (TagBranch "methodName" [] inner) = leafText inner
            fromXmlName x = throwError ("Broken <methodName>: " <> renderTree [x])

            fromXmlParams (TagBranch "params" [] params) = mapM fromXml params
            fromXmlParams x = throwError ("Broken <params>: " <> renderTree [x])

    fromXml x = throwError ("Broken <methodCall>: " <> renderTree [x])

-- | Builds a fault struct
faultStruct :: Int -> Text -> Value
faultStruct code str = ValueStruct $
    M.fromList [("faultCode",   ValueInt code),
                ("faultString", ValueString str)]

instance XmlContent MethodResponse where
    toXml (Return params) = TagBranch "methodResponse" []
        [TagBranch "params" [] [toXml params]]

    toXml (Fault code str) = TagBranch "methodResponse" []
        [TagBranch "fault" [] [valueToTree (faultStruct code str)]]

    fromXml (TagBranch "methodResponse" [] [res]) =
        case res of
            TagBranch "params" [] [param] -> liftM Return (fromXml param)

            TagBranch "fault" [] [err] -> do
                errStruct <- fromXml err
                code <- getField "faultCode" errStruct
                str <- getField "faultString" errStruct
                return (Fault code str)

            x -> throwError ("Broken <methodResponse> content: " <> renderTree [x])

    fromXml x = throwError ("Broken <methodResponse>: " <> renderTree [x])

-- | Lookup 'TagBranch' with given name
lookupBranch :: (Monad m, MonadError Text m) => [TagTree Text] -> Text -> m (TagTree Text)
lookupBranch (TagLeaf _ : xs) n = lookupBranch xs n
lookupBranch (x@(TagBranch tagName _ _) : xs) name
    | tagName == name = return x
    | otherwise       = lookupBranch xs name
lookupBranch [] n = throwError ("Tag '" <> n <> "' not found")

--
-- Parsing calls and reponses from XML
--

-- | Parse XML branch with given name
parseXml :: (MonadError Text m, XmlContent a) => Text -> LBS.ByteString -> m a
parseXml tagName xml =
    fromXml =<< lookupBranch (parse xml) tagName
  where parse = parseTree . dropNL . decodeUtf8 . LBS.toStrict
        dropNL = T.concat . T.lines

--
-- Rendering calls and reponses to XML
--

-- | Serialize 'XmlContent' as XML
renderXml :: (XmlContent a, Show a) => a -> ByteString
renderXml = encodeUtf8 . renderTree . pure . toXml
