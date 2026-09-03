{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Closed JSON codec for 'ModuleIr'. Not a general JSON library.
module JShark.Bindgen.Json
  ( encodeModule
  , decodeModule
  )
where

import Data.Char (isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import JShark.Bindgen.Ir

encodeModule :: ModuleIr -> Text
encodeModule ir =
  obj
    [ ("module", js (irModule ir))
    , ("prefix", js (irPrefix ir))
    , ("source", js (irSource ir))
    , ("classes", arr (map encClass (irClasses ir)))
    , ("funs", arr (map encFun (irFuns ir)))
    , ("consts", arr (map encConst (irConsts ir)))
    , ("enums", arr (map encEnum (irEnums ir)))
    , ("skipped", arr (map encSkip (irSkipped ir)))
    ]

encClass :: ClassDecl -> Text
encClass c =
  obj
    [ ("name", js (clName c))
    , ("ffi", js (clFfi c))
    , ("ctors", arr (map encFun (clCtors c)))
    , ("props", arr (map encProp (clProps c)))
    , ("methods", arr (map encFun (clMethods c)))
    ]

encFun :: Fun -> Text
encFun f =
  obj
    [ ("name", js (fnName f))
    , ("ffi", js (fnFfi f))
    , ("params", arr (map encParam (fnParams f)))
    , ("ret", encTy (fnRet f))
    , ("ctor", jbool (fnIsCtor f))
    , ("static", jbool (fnStatic f))
    ]

encParam :: Param -> Text
encParam p =
  obj
    [ ("name", js (pName p))
    , ("ty", encTy (pTy p))
    , ("optional", jbool (pOptional p))
    ]

encProp :: Prop -> Text
encProp p =
  obj
    [ ("name", js (prName p))
    , ("ty", encTy (prTy p))
    , ("readonly", jbool (prReadonly p))
    ]

encConst :: ConstDecl -> Text
encConst c =
  obj
    [ ("name", js (cnName c))
    , ("ffi", js (cnFfi c))
    , ("ty", encTy (cnTy c))
    ]

encEnum :: EnumDecl -> Text
encEnum e =
  obj
    [ ("name", js (enName e))
    , ("members", arr (map encMember (enMembers e)))
    ]

encMember :: EnumMember -> Text
encMember m =
  obj
    [ ("name", js (emName m))
    , ("value", maybe jnull js (emValue m))
    , ("numeric", jbool (emNumeric m))
    ]

encSkip :: Skipped -> Text
encSkip s = obj [("name", js (skName s)), ("reason", js (skReason s))]

encTy :: Ty -> Text
encTy = \case
  TyNumber -> tag "num"
  TyBigInt -> tag "bigint"
  TyString -> tag "str"
  TyBool -> tag "bool"
  TyUnit -> tag "unit"
  TyUint8Array -> tag "u8"
  TyArray t -> obj [("k", js "arr"), ("el", encTy t)]
  TyOption t -> obj [("k", js "opt"), ("el", encTy t)]
  TyMap k v -> obj [("k", js "map"), ("key", encTy k), ("val", encTy v)]
  TySet t -> obj [("k", js "set"), ("el", encTy t)]
  TyPromise t -> obj [("k", js "promise"), ("el", encTy t)]
  TyFun as r ->
    obj [("k", js "fn"), ("args", arr (map encTy as)), ("ret", encTy r)]
  TyNamed n -> obj [("k", js "named"), ("n", js n)]
  TyUnknown n -> obj [("k", js "unk"), ("note", js n)]
 where
  tag k = obj [("k", js k)]

-- JSON helpers ----------------------------------------------------------

js :: Text -> Text
js t = T.singleton '"' <> T.concatMap esc t <> T.singleton '"'
 where
  esc c = case c of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ -> T.singleton c

jbool :: Bool -> Text
jbool True = "true"
jbool False = "false"

jnull :: Text
jnull = "null"

obj :: [(Text, Text)] -> Text
obj kvs =
  "{"
    <> T.intercalate
      ","
      [ js k <> ":" <> v
      | (k, v) <- kvs
      ]
    <> "}"

arr :: [Text] -> Text
arr xs = "[" <> T.intercalate "," xs <> "]"

-- Decode ----------------------------------------------------------------

data Json
  = JNull
  | JBool Bool
  | JStr Text
  | JArr [Json]
  | JObj [(Text, Json)]
  deriving (Eq, Show)

decodeModule :: Text -> Either String ModuleIr
decodeModule src = do
  j <- parseJson src
  parseModule j

parseModule :: Json -> Either String ModuleIr
parseModule j = do
  o <- asObj j
  moduleName <- strField o "module"
  prefix <- strField o "prefix"
  source <- strField o "source"
  classes <- arrField o "classes" parseClass
  funs <- arrField o "funs" parseFun
  consts <- arrField o "consts" parseConst
  enums <- arrField o "enums" parseEnum
  skipped <- arrField o "skipped" parseSkip
  pure
    ModuleIr
      { irModule = moduleName
      , irPrefix = prefix
      , irSource = source
      , irClasses = classes
      , irFuns = funs
      , irConsts = consts
      , irEnums = enums
      , irSkipped = skipped
      }

parseClass :: Json -> Either String ClassDecl
parseClass j = do
  o <- asObj j
  ClassDecl
    <$> strField o "name"
    <*> strField o "ffi"
    <*> arrField o "ctors" parseFun
    <*> arrField o "props" parseProp
    <*> arrField o "methods" parseFun

parseFun :: Json -> Either String Fun
parseFun j = do
  o <- asObj j
  Fun
    <$> strField o "name"
    <*> strField o "ffi"
    <*> arrField o "params" parseParam
    <*> (field o "ret" >>= parseTy)
    <*> boolField o "ctor"
    <*> optionalBool o "static"

parseParam :: Json -> Either String Param
parseParam j = do
  o <- asObj j
  Param
    <$> strField o "name"
    <*> (field o "ty" >>= parseTy)
    <*> boolField o "optional"

parseProp :: Json -> Either String Prop
parseProp j = do
  o <- asObj j
  Prop
    <$> strField o "name"
    <*> (field o "ty" >>= parseTy)
    <*> boolField o "readonly"

parseConst :: Json -> Either String ConstDecl
parseConst j = do
  o <- asObj j
  ConstDecl
    <$> strField o "name"
    <*> strField o "ffi"
    <*> (field o "ty" >>= parseTy)

parseEnum :: Json -> Either String EnumDecl
parseEnum j = do
  o <- asObj j
  EnumDecl
    <$> strField o "name"
    <*> arrField o "members" parseMember

parseMember :: Json -> Either String EnumMember
parseMember j = do
  o <- asObj j
  EnumMember
    <$> strField o "name"
    <*> optionalStr o "value"
    <*> boolField o "numeric"

parseSkip :: Json -> Either String Skipped
parseSkip j = do
  o <- asObj j
  Skipped <$> strField o "name" <*> strField o "reason"

parseTy :: Json -> Either String Ty
parseTy j = do
  o <- asObj j
  k <- strField o "k"
  case k of
    "num" -> pure TyNumber
    "bigint" -> pure TyBigInt
    "str" -> pure TyString
    "bool" -> pure TyBool
    "unit" -> pure TyUnit
    "u8" -> pure TyUint8Array
    "arr" -> TyArray <$> (field o "el" >>= parseTy)
    "opt" -> TyOption <$> (field o "el" >>= parseTy)
    "set" -> TySet <$> (field o "el" >>= parseTy)
    "promise" -> TyPromise <$> (field o "el" >>= parseTy)
    "map" ->
      TyMap
        <$> (field o "key" >>= parseTy)
        <*> (field o "val" >>= parseTy)
    "fn" ->
      TyFun
        <$> arrField o "args" parseTy
        <*> (field o "ret" >>= parseTy)
    "named" -> TyNamed <$> strField o "n"
    "unk" -> TyUnknown <$> strField o "note"
    _ -> Left ("unknown type tag: " <> T.unpack k)

-- JSON object field helpers ---------------------------------------------

asObj :: Json -> Either String [(Text, Json)]
asObj (JObj o) = Right o
asObj _ = Left "expected object"

field :: [(Text, Json)] -> Text -> Either String Json
field o k = case lookup k o of
  Just v -> Right v
  Nothing -> Left ("missing field " <> T.unpack k)

strField :: [(Text, Json)] -> Text -> Either String Text
strField o k = do
  v <- field o k
  case v of
    JStr t -> Right t
    _ -> Left ("field " <> T.unpack k <> " is not a string")

boolField :: [(Text, Json)] -> Text -> Either String Bool
boolField o k = do
  v <- field o k
  case v of
    JBool b -> Right b
    _ -> Left ("field " <> T.unpack k <> " is not a bool")

optionalStr :: [(Text, Json)] -> Text -> Either String (Maybe Text)
optionalStr o k = case lookup k o of
  Nothing -> Right Nothing
  Just JNull -> Right Nothing
  Just (JStr t) -> Right (Just t)
  Just _ -> Left ("field " <> T.unpack k <> " is not a string")

optionalBool :: [(Text, Json)] -> Text -> Either String Bool
optionalBool o k = case lookup k o of
  Nothing -> Right False
  Just (JBool b) -> Right b
  Just _ -> Left ("field " <> T.unpack k <> " is not a bool")

arrField ::
  [(Text, Json)] -> Text -> (Json -> Either String a) -> Either String [a]
arrField o k p = do
  v <- field o k
  case v of
    JArr xs -> traverse p xs
    _ -> Left ("field " <> T.unpack k <> " is not an array")

-- Tiny JSON parser ------------------------------------------------------

parseJson :: Text -> Either String Json
parseJson t = case runP (skip *> jsonP <* skip) t of
  Right (j, rest)
    | T.all isSpace rest -> Right j
    | otherwise -> Left "trailing JSON"
  Left e -> Left e

newtype P a = P {runP :: Text -> Either String (a, Text)}

instance Functor P where
  fmap f (P g) = P $ \t -> case g t of
    Left e -> Left e
    Right (a, r) -> Right (f a, r)

instance Applicative P where
  pure a = P $ \t -> Right (a, t)
  P f <*> P g = P $ \t -> do
    (h, t1) <- f t
    (a, t2) <- g t1
    Right (h a, t2)

instance Monad P where
  P g >>= f = P $ \t -> do
    (a, t1) <- g t
    runP (f a) t1

failP :: String -> P a
failP e = P $ \_ -> Left e

skip :: P ()
skip = P $ \t -> Right ((), T.dropWhile isSpace t)

peekC :: P (Maybe Char)
peekC = P $ \t -> Right (fmap fst (T.uncons t), t)

satisfy :: (Char -> Bool) -> P Char
satisfy p = P $ \t -> case T.uncons t of
  Just (c, r) | p c -> Right (c, r)
  _ -> Left "unexpected char"

charP :: Char -> P ()
charP c = () <$ satisfy (== c)

stringP :: Text -> P ()
stringP s = P $ \t -> case T.stripPrefix s t of
  Just r -> Right ((), r)
  Nothing -> Left ("expected " <> T.unpack s)

jsonP :: P Json
jsonP = do
  skip
  c <- peekC
  case c of
    Just 'n' -> JNull <$ stringP "null"
    Just 't' -> JBool True <$ stringP "true"
    Just 'f' -> JBool False <$ stringP "false"
    Just '"' -> JStr <$> jsonString
    Just '[' -> jsonArr
    Just '{' -> jsonObj
    Just d | isDigit d || d == '-' -> JStr <$> jsonNumber
    _ -> failP "expected JSON value"

jsonArr :: P Json
jsonArr = do
  charP '['
  skip
  c <- peekC
  case c of
    Just ']' -> JArr [] <$ charP ']'
    _ -> do
      x <- jsonP
      xs <- manyComma jsonP
      skip
      charP ']'
      pure (JArr (x : xs))

jsonObj :: P Json
jsonObj = do
  charP '{'
  skip
  c <- peekC
  case c of
    Just '}' -> JObj [] <$ charP '}'
    _ -> do
      kv <- jsonKv
      kvs <- manyComma jsonKv
      skip
      charP '}'
      pure (JObj (kv : kvs))

jsonKv :: P (Text, Json)
jsonKv = do
  skip
  k <- jsonString
  skip
  charP ':'
  v <- jsonP
  pure (k, v)

manyComma :: P a -> P [a]
manyComma p = go
 where
  go = do
    skip
    c <- peekC
    case c of
      Just ',' -> do
        charP ','
        x <- p
        xs <- go
        pure (x : xs)
      _ -> pure []

jsonString :: P Text
jsonString = do
  charP '"'
  cs <- strChars
  charP '"'
  pure (T.pack cs)

strChars :: P String
strChars = do
  c <- peekC
  case c of
    Just '"' -> pure []
    Just '\\' -> do
      charP '\\'
      e <- satisfy (const True)
      rest <- strChars
      pure (unescape e : rest)
    Just _ -> do
      x <- satisfy (/= '"')
      rest <- strChars
      pure (x : rest)
    Nothing -> failP "unterminated string"

unescape :: Char -> Char
unescape c = case c of
  'n' -> '\n'
  'r' -> '\r'
  't' -> '\t'
  '"' -> '"'
  '\\' -> '\\'
  _ -> c

-- Numbers are stored as strings; the IR does not need numeric JSON.
jsonNumber :: P Text
jsonNumber = P $ \t ->
  let
    (n, r) = T.span (\c -> isDigit c || c `elem` ("+-eE." :: String)) t
   in
    if T.null n then Left "expected number" else Right (n, r)
