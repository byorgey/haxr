module Network.XmlRpc.DTD_XMLRPC where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN
import Data.Char (isSpace)


{-Type decls-}

newtype I4 = I4 String 		deriving (Eq,Show)
newtype AInt = AInt String 		deriving (Eq,Show)
newtype Boolean = Boolean String 		deriving (Eq,Show)
newtype AString = AString String 		deriving (Eq,Show)
newtype ADouble = ADouble String 		deriving (Eq,Show)
newtype DateTime_iso8601 = DateTime_iso8601 String 		deriving (Eq,Show)
newtype Base64 = Base64 String 		deriving (Eq,Show)
newtype Data = Data [Value] 		deriving (Eq,Show)
newtype Array = Array Data 		deriving (Eq,Show)
newtype Name = Name String 		deriving (Eq,Show)
data Member = Member Name Value
            deriving (Eq,Show)
newtype Struct = Struct [Member] 		deriving (Eq,Show)
newtype Value = Value [Value_] 		deriving (Eq,Show)
data Value_ = Value_Str String
            | Value_I4 I4
            | Value_AInt AInt
            | Value_Boolean Boolean
            | Value_AString AString
            | Value_DateTime_iso8601 DateTime_iso8601
            | Value_ADouble ADouble
            | Value_Base64 Base64
            | Value_Struct Struct
            | Value_Array Array
            deriving (Eq,Show)
newtype Param = Param Value 		deriving (Eq,Show)
newtype Params = Params [Param] 		deriving (Eq,Show)
newtype MethodName = MethodName String 		deriving (Eq,Show)
data MethodCall = MethodCall MethodName (Maybe Params)
                deriving (Eq,Show)
newtype Fault = Fault Value 		deriving (Eq,Show)
data MethodResponse = MethodResponseParams Params
                    | MethodResponseFault Fault
                    deriving (Eq,Show)


{-Instance decls-}

instance XmlContent I4 where
    fromElem (CElem (Elem "i4" [] c0):rest) =
        (\(a,ca)->
           (Just (I4 a), rest))
        (definite fromText "text" "i4" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (I4 a) =
        [CElem (Elem "i4" [] (toText a))]
instance XmlContent AInt where
    fromElem (CElem (Elem "int" [] c0):rest) =
        (\(a,ca)->
           (Just (AInt a), rest))
        (definite fromText "text" "int" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (AInt a) =
        [CElem (Elem "int" [] (toText a))]
instance XmlContent Boolean where
    fromElem (CElem (Elem "boolean" [] c0):rest) =
        (\(a,ca)->
           (Just (Boolean a), rest))
        (definite fromText "text" "boolean" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Boolean a) =
        [CElem (Elem "boolean" [] (toText a))]
instance XmlContent AString where
    fromElem (CElem (Elem "string" [] c0):rest) =
        (\(a,ca)->
           (Just (AString a), rest))
        (definite fromText "text" "string" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (AString a) =
        [CElem (Elem "string" [] (toText a))]
instance XmlContent ADouble where
    fromElem (CElem (Elem "double" [] c0):rest) =
        (\(a,ca)->
           (Just (ADouble a), rest))
        (definite fromText "text" "double" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (ADouble a) =
        [CElem (Elem "double" [] (toText a))]
instance XmlContent DateTime_iso8601 where
    fromElem (CElem (Elem "dateTime.iso8601" [] c0):rest) =
        (\(a,ca)->
           (Just (DateTime_iso8601 a), rest))
        (definite fromText "text" "dateTime.iso8601" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (DateTime_iso8601 a) =
        [CElem (Elem "dateTime.iso8601" [] (toText a))]
instance XmlContent Base64 where
    fromElem (CElem (Elem "base64" [] c0):rest) =
        (\(a,ca)->
           (Just (Base64 a), rest))
        (definite fromText "text" "base64" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Base64 a) =
        [CElem (Elem "base64" [] (toText a))]
instance XmlContent Data where
    fromElem (CElem (Elem "data" [] c0):rest) =
        (\(a,ca)->
           (Just (Data a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Data a) =
        [CElem (Elem "data" [] (concatMap toElem a))]
instance XmlContent Array where
    fromElem (CElem (Elem "array" [] c0):rest) =
        (\(a,ca)->
           (Just (Array a), rest))
        (definite fromElem "<data>" "array" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Array a) =
        [CElem (Elem "array" [] (toElem a))]
instance XmlContent Name where
    fromElem (CElem (Elem "name" [] c0):rest) =
        (\(a,ca)->
           (Just (Name a), rest))
        (definite fromText "text" "name" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Name a) =
        [CElem (Elem "name" [] (toText a))]
instance XmlContent Member where
    fromElem (CElem (Elem "member" [] c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (Just (Member a b), rest))
           (definite fromElem "<value>" "member" ca))
        (definite fromElem "<name>" "member" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Member a b) =
        [CElem (Elem "member" [] (toElem a ++ toElem b))]
instance XmlContent Struct where
    fromElem (CElem (Elem "struct" [] c0):rest) =
        (\(a,ca)->
           (Just (Struct a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Struct a) =
        [CElem (Elem "struct" [] (concatMap toElem a))]
instance XmlContent Value where
    fromElem (CElem (Elem "value" [] c0):rest) =
        (\(a,ca)->
           (Just (Value a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Value a) =
        [CElem (Elem "value" [] (concatMap toElem a))]
instance XmlContent Value_ where
    fromElem c0 =
        case (fromText c0) of
        (Just a,rest) -> (Just (Value_Str a), rest)
        (_,_) ->
                case (fromElem c0) of
                (Just a,rest) -> (Just (Value_I4 a), rest)
                (_,_) ->
                        case (fromElem c0) of
                        (Just a,rest) -> (Just (Value_AInt a), rest)
                        (_,_) ->
                                case (fromElem c0) of
                                (Just a,rest) -> (Just (Value_Boolean a), rest)
                                (_,_) ->
                                        case (fromElem c0) of
                                        (Just a,rest) -> (Just (Value_AString a), rest)
                                        (_,_) ->
                                                case (fromElem c0) of
                                                (Just a,rest) -> (Just (Value_DateTime_iso8601 a), rest)
                                                (_,_) ->
                                                        case (fromElem c0) of
                                                        (Just a,rest) -> (Just (Value_ADouble a), rest)
                                                        (_,_) ->
                                                                case (fromElem c0) of
                                                                (Just a,rest) -> (Just (Value_Base64 a), rest)
                                                                (_,_) ->
                                                                        case (fromElem c0) of
                                                                        (Just a,rest) -> (Just (Value_Struct a), rest)
                                                                        (_,_) ->
                                                                                case (fromElem c0) of
                                                                                (Just a,rest) -> (Just (Value_Array a), rest)
                                                                                (_,_) ->
                                                                                    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Value_Str a) = toText a
    toElem (Value_I4 a) = toElem a
    toElem (Value_AInt a) = toElem a
    toElem (Value_Boolean a) = toElem a
    toElem (Value_AString a) = toElem a
    toElem (Value_DateTime_iso8601 a) = toElem a
    toElem (Value_ADouble a) = toElem a
    toElem (Value_Base64 a) = toElem a
    toElem (Value_Struct a) = toElem a
    toElem (Value_Array a) = toElem a
instance XmlContent Param where
    fromElem (CElem (Elem "param" [] c0):rest) =
        (\(a,ca)->
           (Just (Param a), rest))
        (definite fromElem "<value>" "param" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Param a) =
        [CElem (Elem "param" [] (toElem a))]
instance XmlContent Params where
    fromElem (CElem (Elem "params" [] c0):rest) =
        (\(a,ca)->
           (Just (Params a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Params a) =
        [CElem (Elem "params" [] (concatMap toElem a))]
instance XmlContent MethodName where
    fromElem (CElem (Elem "methodName" [] c0):rest) =
        (\(a,ca)->
           (Just (MethodName a), rest))
        (definite fromText "text" "methodName" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (MethodName a) =
        [CElem (Elem "methodName" [] (toText a))]
instance XmlContent MethodCall where
    fromElem (CElem (Elem "methodCall" [] c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (Just (MethodCall a b), rest))
           (fromElem ca))
        (definite fromElem "<methodName>" "methodCall" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (MethodCall a b) =
        [CElem (Elem "methodCall" [] (toElem a ++ maybe [] toElem b))]
instance XmlContent Fault where
    fromElem (CElem (Elem "fault" [] c0):rest) =
        (\(a,ca)->
           (Just (Fault a), rest))
        (definite fromElem "<value>" "fault" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Fault a) =
        [CElem (Elem "fault" [] (toElem a))]
instance XmlContent MethodResponse where
    fromElem (CElem (Elem "methodResponse" [] c0):rest) =
        case (fromElem c0) of
        (Just a,_) -> (Just (MethodResponseParams a), rest)
        (_,_) ->
                case (fromElem c0) of
                (Just a,_) -> (Just (MethodResponseFault a), rest)
                (_,_) ->
                    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (MethodResponseParams a) = [CElem (Elem "methodResponse" [] (toElem a) )]
    toElem (MethodResponseFault a) = [CElem (Elem "methodResponse" [] (toElem a) )]


{-Done-}
