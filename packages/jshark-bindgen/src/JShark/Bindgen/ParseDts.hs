{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Best-effort .d.ts / .ts declaration parser for bindgen.
-- Mapped / conditional types become 'TyUnknown'. Use the TypeScript
-- extractor when @typescript@ is installed.
module JShark.Bindgen.ParseDts
  ( parseDts
  )
where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import JShark.Bindgen.Ir

parseDts :: Text -> Text -> Text -> Either String ModuleIr
parseDts moduleName sourceName src =
  case runP (skip *> manyDecl) src of
    Left e -> Left e
    Right (decls, rest) ->
      let
        ir = applyDecls moduleName sourceName decls
        rest' = T.strip rest
       in
        if T.null rest'
          then Right ir
          else
            Right
              ir
                { irSkipped =
                    irSkipped ir
                      <> [ Skipped
                             "parser"
                             ( "unparsed tail: "
                                 <> T.take 80 rest'
                             )
                         ]
                }

data Decl
  = DFun Fun
  | DClass ClassDecl
  | DConst ConstDecl
  | DEnum EnumDecl
  | DSkip Skipped
  | DPrefix Text
  | DNs Text [Decl]

data Member
  = MFun Fun
  | MProp Prop
  | MSkip Skipped

applyDecls :: Text -> Text -> [Decl] -> ModuleIr
applyDecls moduleName sourceName decls =
  flatten
    (emptyModule moduleName sourceName)
      { irPrefix = firstPrefix decls
      }
    T.empty
    decls

firstPrefix :: [Decl] -> Text
firstPrefix [] = T.empty
firstPrefix (DPrefix p : _) = p
firstPrefix (_ : xs) = firstPrefix xs

flatten :: ModuleIr -> Text -> [Decl] -> ModuleIr
flatten ir ns = \case
  [] -> ir
  DFun f : xs ->
    flatten (ir {irFuns = irFuns ir <> [qualFun ns f]}) ns xs
  DClass c : xs ->
    flatten (ir {irClasses = irClasses ir <> [qualClass ns c]}) ns xs
  DConst c : xs ->
    flatten (ir {irConsts = irConsts ir <> [qualConst ns c]}) ns xs
  DEnum e : xs ->
    flatten (ir {irEnums = irEnums ir <> [e]}) ns xs
  DSkip s : xs ->
    flatten (ir {irSkipped = irSkipped ir <> [s]}) ns xs
  DPrefix _ : xs -> flatten ir ns xs
  DNs name inner : xs ->
    flatten (flatten ir (dot ns name) inner) ns xs

qualFun :: Text -> Fun -> Fun
qualFun ns f
  | T.null ns = f
  | fnFfi f == ns = f
  | (ns <> ".") `T.isPrefixOf` fnFfi f = f
  | otherwise = f {fnFfi = dot ns (fnFfi f)}

qualClass :: Text -> ClassDecl -> ClassDecl
qualClass ns c
  | T.null ns = c
  | clFfi c == ns || (ns <> ".") `T.isPrefixOf` clFfi c =
      c {clCtors = fmap (qualFun ns) (clCtors c)}
  | otherwise =
      c
        { clFfi = dot ns (clFfi c)
        , clCtors =
            fmap
              (\f -> f {fnFfi = dot ns (clName c), fnName = clName c})
              (clCtors c)
        }

qualConst :: Text -> ConstDecl -> ConstDecl
qualConst ns c
  | T.null ns = c
  | cnFfi c == ns = c
  | (ns <> ".") `T.isPrefixOf` cnFfi c = c
  | otherwise = c {cnFfi = dot ns (cnFfi c)}

dot :: Text -> Text -> Text
dot a b
  | T.null a = b
  | otherwise = a <> "." <> b

-- Parser ----------------------------------------------------------------

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
skip = P $ \t -> Right ((), dropTrivia t)

dropTrivia :: Text -> Text
dropTrivia t =
  let
    t1 = T.dropWhile isSpace t
   in
    case T.uncons t1 of
      Just ('/', r)
        | Just r2 <- T.stripPrefix "/" r ->
            dropTrivia (T.dropWhile (/= '\n') r2)
        | Just r2 <- T.stripPrefix "*" r ->
            dropTrivia (dropBlock r2)
      _ -> t1

dropBlock :: Text -> Text
dropBlock t = case T.breakOn "*/" t of
  (_, rest)
    | Just r <- T.stripPrefix "*/" rest -> r
    | otherwise -> T.empty

peekTok :: P (Maybe Text)
peekTok = P $ \t ->
  let
    t1 = dropTrivia t
   in
    Right (fst <$> tokHead t1, t1)

tokHead :: Text -> Maybe (Text, Text)
tokHead t = case T.uncons t of
  Nothing -> Nothing
  Just (c, r)
    | isIdentStart c ->
        let
          (n, rest) = T.span isIdentChar t
         in
          Just (n, rest)
    | c == '"' || c == '\'' ->
        let
          (s, rest) = takeString c r
         in
          Just (T.cons c s, rest)
    | T.isPrefixOf "=>" t -> Just ("=>", T.drop 2 t)
    | T.isPrefixOf "..." t -> Just ("...", T.drop 3 t)
    | c `elem` ("()[]{}<>,;:?.|&=" :: String) ->
        Just (T.singleton c, r)
    | otherwise -> Just (T.singleton c, r)

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_' || c == '$'

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '$'

takeString :: Char -> Text -> (Text, Text)
takeString q = go
 where
  go rest = case T.uncons rest of
    Nothing -> (T.empty, T.empty)
    Just ('\\', r) ->
      case T.uncons r of
        Nothing -> (T.pack "\\", T.empty)
        Just (e, r2) ->
          let
            (s, r3) = go r2
           in
            (T.cons '\\' (T.cons e s), r3)
    Just (c, r)
      | c == q -> (T.singleton c, r)
      | otherwise ->
          let
            (s, r2) = go r
           in
            (T.cons c s, r2)

satisfyTok :: (Text -> Bool) -> P Text
satisfyTok p = P $ \t ->
  let
    t1 = dropTrivia t
   in
    case tokHead t1 of
      Just (tok, r) | p tok -> Right (tok, r)
      _ -> Left "unexpected token"

sym :: Text -> P ()
sym s = () <$ satisfyTok (== s)

ident :: P Text
ident = satisfyTok $ \t -> case T.uncons t of
  Just (c, _) -> isIdentStart c
  Nothing -> False

kw :: Text -> P ()
kw s = () <$ satisfyTok (== s)

optionalP :: P a -> P (Maybe a)
optionalP p = P $ \t -> case runP p t of
  Right (a, r) -> Right (Just a, r)
  Left _ -> Right (Nothing, t)

choice :: [P a] -> P a
choice [] = failP "no parse"
choice (p : ps) = P $ \t -> case runP p t of
  Right x -> Right x
  Left _ -> runP (choice ps) t

manyP :: P a -> P [a]
manyP p = do
  mx <- optionalP p
  case mx of
    Nothing -> pure []
    Just x -> fmap (x :) (manyP p)

sepBy :: P a -> P sep -> P [a]
sepBy p sep = do
  mx <- optionalP p
  case mx of
    Nothing -> pure []
    Just x -> do
      xs <- manyP (sep *> p)
      _ <- optionalP sep
      pure (x : xs)

isIdentTok :: Text -> Bool
isIdentTok t = case T.uncons t of
  Just (c, _) -> isIdentStart c
  Nothing -> False

-- Declarations ----------------------------------------------------------

manyDecl :: P [Decl]
manyDecl = manyP decl

decl :: P Decl
decl = do
  skip
  _ <- manyP (choice [kw "export", kw "declare", kw "default"])
  mt <- peekTok
  case mt of
    Just "function" -> DFun <$> functionDecl
    Just "class" -> DClass <$> classDecl
    Just "interface" -> DClass <$> interfaceDecl
    Just "namespace" -> nsDecl
    Just "module" -> nsDecl
    Just "enum" -> DEnum <$> enumDecl
    Just "const" -> constOrEnum
    Just "type" -> typeAlias
    Just "let" -> DConst <$> constDecl
    Just "var" -> DConst <$> constDecl
    Just "as" -> asNamespace
    Just "import" -> skipImport
    Just "=" -> skipExportEq
    Just "{" -> skipExportBrace
    Just t | isIdentTok t -> ambientFun
    _ -> skipJunk

functionDecl :: P Fun
functionDecl = do
  kw "function"
  name <- ident
  _ <- optionalP typeParams
  params <- paramList
  ret <- optionalColonType
  finishStmt
  _ <- optionalP skipBody
  pure (Fun name name params ret False False)

ambientFun :: P Decl
ambientFun = do
  name <- ident
  _ <- optionalP typeParams
  t <- peekTok
  case t of
    Just "(" -> do
      params <- paramList
      ret <- optionalColonType
      finishStmt
      pure (DFun (Fun name name params ret False False))
    _ -> skipJunk

classDecl :: P ClassDecl
classDecl = do
  kw "class"
  name <- ident
  _ <- optionalP typeParams
  _ <- optionalP heritage
  members <- braceMembers
  pure (classFrom name members)

interfaceDecl :: P ClassDecl
interfaceDecl = do
  kw "interface"
  name <- ident
  _ <- optionalP typeParams
  _ <- optionalP heritage
  members <- braceMembers
  pure (classFrom name members)

classFrom :: Text -> [Member] -> ClassDecl
classFrom name members =
  ClassDecl
    { clName = name
    , clFfi = name
    , clCtors =
        [ f
            { fnName = name
            , fnFfi = name
            , fnRet = TyNamed name
            }
        | MFun f <- members
        , fnIsCtor f
        ]
    , clProps = [p | MProp p <- members]
    , clMethods = [f | MFun f <- members, not (fnIsCtor f)]
    }

braceMembers :: P [Member]
braceMembers = do
  sym "{"
  ms <- manyP memberDecl
  _ <- optionalP (sym "}")
  pure ms

memberDecl :: P Member
memberDecl = do
  skip
  _ <-
    manyP $
      choice
        [ kw "public"
        , kw "private"
        , kw "protected"
        , kw "abstract"
        , kw "override"
        , kw "async"
        , kw "declare"
        ]
  isStatic <- flag (kw "static")
  ro <- flag (kw "readonly")
  t <- peekTok
  case t of
    Just "constructor" -> MFun <$> ctorMember
    Just "get" -> kw "get" *> namedMember ro isStatic
    Just "set" -> kw "set" *> namedMember False isStatic
    Just "[" -> indexMember
    Just "}" -> failP "end"
    Just _ -> namedMember ro isStatic
    Nothing -> failP "eof"

ctorMember :: P Fun
ctorMember = do
  kw "constructor"
  params <- paramList
  finishStmt
  _ <- optionalP skipBody
  pure (Fun "constructor" "constructor" params TyUnit True False)

namedMember :: Bool -> Bool -> P Member
namedMember ro isStatic = do
  name <- ident
  _ <- optionalP (sym "?")
  t <- peekTok
  case t of
    Just "(" -> method name isStatic
    Just "<" -> typeParams *> method name isStatic
    _ -> do
      ty <- optionalColonType
      finishStmt
      _ <- optionalP (sym ",")
      pure (MProp (Prop name ty ro))

method :: Text -> Bool -> P Member
method name isStatic = do
  params <- paramList
  ret <- optionalColonType
  finishStmt
  _ <- optionalP skipBody
  pure (MFun (Fun name name params ret False isStatic))

indexMember :: P Member
indexMember = do
  skipBalanced '[' ']'
  _ <- optionalP (sym ":" *> typeP)
  finishStmt
  pure (MSkip (Skipped "[index]" "index signature"))

nsDecl :: P Decl
nsDecl = do
  _ <- choice [kw "namespace", kw "module"]
  name <- choice [ident, stringLit]
  sym "{"
  inner <- manyDecl
  _ <- optionalP (sym "}")
  pure (DNs name inner)

constOrEnum :: P Decl
constOrEnum = do
  kw "const"
  t <- peekTok
  case t of
    Just "enum" -> DEnum <$> enumDecl
    _ -> DConst <$> constAfter

enumDecl :: P EnumDecl
enumDecl = do
  _ <- optionalP (kw "const")
  kw "enum"
  name <- ident
  sym "{"
  members <- sepBy enumMember (sym ",")
  _ <- optionalP (sym "}")
  pure (EnumDecl name members)

enumMember :: P EnumMember
enumMember = do
  name <- ident
  val <- optionalP (sym "=" *> enumValue)
  case val of
    Just (num, v) -> pure (EnumMember name (Just v) num)
    Nothing -> pure (EnumMember name Nothing False)

enumValue :: P (Bool, Text)
enumValue = do
  t <- peekTok
  case t of
    Just s | isNumTok s -> (True, s) <$ satisfyTok isNumTok
    Just s | isStringTok s -> do
      _ <- satisfyTok isStringTok
      pure (False, unquote s)
    _ -> do
      n <- ident
      pure (False, n)

typeAlias :: P Decl
typeAlias = do
  kw "type"
  name <- ident
  _ <- optionalP typeParams
  sym "="
  _ <- typeP
  finishStmt
  pure (DSkip (Skipped name "type alias"))

constDecl :: P ConstDecl
constDecl = do
  _ <- choice [kw "const", kw "let", kw "var"]
  constAfter

constAfter :: P ConstDecl
constAfter = do
  name <- ident
  ty <- optionalColonType
  _ <- optionalP (sym "=" *> skipValue)
  finishStmt
  pure (ConstDecl name name ty)

asNamespace :: P Decl
asNamespace = do
  kw "as"
  kw "namespace"
  name <- ident
  finishStmt
  pure (DPrefix name)

skipImport :: P Decl
skipImport = do
  kw "import"
  skipUntil ";"
  pure (DSkip (Skipped "import" "import"))

skipExportEq :: P Decl
skipExportEq = do
  sym "="
  _ <- ident
  finishStmt
  pure (DSkip (Skipped "export=" "export equals"))

skipExportBrace :: P Decl
skipExportBrace = do
  skipBalanced '{' '}'
  _ <- optionalP (kw "from" *> stringLit)
  finishStmt
  pure (DSkip (Skipped "export{}" "re-export"))

skipJunk :: P Decl
skipJunk = do
  t <- peekTok
  case t of
    Nothing -> failP "eof"
    Just "}" -> failP "end"
    Just ";" -> DSkip (Skipped ";" "empty") <$ sym ";"
    Just "{" -> do
      skipBalanced '{' '}'
      pure (DSkip (Skipped "{}" "skipped block"))
    Just _ -> do
      _ <- satisfyTok (const True)
      skipUntilOneOf [";", "}", "export", "declare"]
      pure (DSkip (Skipped "junk" "unparsed"))

finishStmt :: P ()
finishStmt = () <$ optionalP (sym ";")

flag :: P a -> P Bool
flag p = maybe False (const True) <$> optionalP p

-- Types -----------------------------------------------------------------

optionalColonType :: P Ty
optionalColonType = maybe TyUnit id <$> optionalP (sym ":" *> typeP)

typeParams :: P ()
typeParams = skipBalanced '<' '>'

heritage :: P ()
heritage = do
  _ <- choice [kw "extends", kw "implements"]
  _ <- typeP
  _ <- manyP (sym "," *> typeP)
  _ <- optionalP (choice [kw "extends", kw "implements"] *> typeP)
  pure ()

typeP :: P Ty
typeP = do
  _ <- optionalP (sym "|")
  x <- interType
  xs <- manyP (sym "|" *> interType)
  pure (foldUnion (x : xs))

interType :: P Ty
interType = do
  x <- postfixType
  xs <- manyP (sym "&" *> postfixType)
  case xs of
    [] -> pure x
    _ -> pure (TyUnknown "intersection")

postfixType :: P Ty
postfixType = do
  t <- atomType
  go t
 where
  go t = do
    m <- peekTok
    case m of
      Just "[" -> do
        sym "["
        close <- peekTok
        case close of
          Just "]" -> sym "]" >> go (TyArray t)
          _ -> typeP >> optionalP (sym "]") >> go (TyUnknown "indexed")
      _ -> pure t

atomType :: P Ty
atomType = do
  t <- peekTok
  case t of
    Just "(" -> choice [fnType, parenType]
    Just "{" -> skipBalanced '{' '}' >> pure (TyUnknown "object")
    Just "[" -> skipBalanced '[' ']' >> pure (TyUnknown "tuple")
    Just "typeof" ->
      kw "typeof"
        >> ident
        >> manyP (sym "." *> ident)
        >> pure (TyUnknown "typeof")
    Just "keyof" -> kw "keyof" >> typeP >> pure (TyUnknown "keyof")
    Just "infer" -> kw "infer" >> ident >> pure (TyUnknown "infer")
    Just "readonly" -> kw "readonly" >> typeP
    Just "unique" -> kw "unique" >> typeP
    Just "new" ->
      kw "new"
        >> optionalP typeParams
        >> paramList
        >> optionalP (sym "=>" *> typeP)
        >> pure (TyUnknown "construct")
    Just s | isStringTok s -> TyString <$ satisfyTok isStringTok
    Just s | isNumTok s -> TyNumber <$ satisfyTok isNumTok
    Just "true" -> TyBool <$ kw "true"
    Just "false" -> TyBool <$ kw "false"
    Just s | isIdentTok s -> namedType
    _ -> failP "expected type"

parenType :: P Ty
parenType = do
  sym "("
  t <- typeP
  sym ")"
  m <- peekTok
  case m of
    Just "=>" -> do
      sym "=>"
      r <- typeP
      pure (TyFun [t] r)
    _ -> pure t

fnType :: P Ty
fnType = do
  params <- paramList
  sym "=>"
  r <- typeP
  pure (TyFun (fmap pTy params) r)

namedType :: P Ty
namedType = do
  name <- qualName
  args <- optionalP typeArgsFixed
  pure (mapNamed name args)

qualName :: P Text
qualName = do
  a <- ident
  rest <- manyP (sym "." *> ident)
  pure (T.intercalate "." (a : rest))

typeArgsFixed :: P [Ty]
typeArgsFixed = do
  sym "<"
  ts <- sepBy typeP (sym ",")
  _ <- optionalP (sym ">")
  pure ts

paramList :: P [Param]
paramList = do
  sym "("
  ps <- sepBy param (sym ",")
  _ <- optionalP (sym ")")
  pure ps

param :: P Param
param = do
  _ <- optionalP (choice [kw "public", kw "private", kw "protected"])
  _ <- optionalP (kw "readonly")
  _ <- optionalP (sym "...")
  t <- peekTok
  name <- case t of
    Just s | isIdentTok s -> ident
    Just "{" -> skipBalanced '{' '}' >> pure "opts"
    Just "[" -> skipBalanced '[' ']' >> pure "tuple"
    _ -> ident
  opt <- flag (sym "?")
  ty <- optionalColonType
  _ <- optionalP (sym "=" *> skipValue)
  pure (Param name ty opt)

-- Skip helpers ----------------------------------------------------------

skipBody :: P ()
skipBody = skipBalanced '{' '}'

skipValue :: P ()
skipValue = do
  t <- peekTok
  case t of
    Just "{" -> skipBalanced '{' '}'
    Just "[" -> skipBalanced '[' ']'
    Just "(" -> skipBalanced '(' ')'
    Just _ -> () <$ satisfyTok (const True)
    Nothing -> pure ()

skipUntil :: Text -> P ()
skipUntil end = P $ \t ->
  let
    t1 = dropTrivia t
   in
    case T.breakOn end t1 of
      (_, r)
        | Just r2 <- T.stripPrefix end r -> Right ((), r2)
        | otherwise -> Right ((), T.empty)

skipUntilOneOf :: [Text] -> P ()
skipUntilOneOf ends = P $ \t ->
  let
    t1 = dropTrivia t
    stop s = any (`T.isPrefixOf` s) ends
    go s
      | T.null s = s
      | stop s = s
      | otherwise = go (T.drop 1 s)
   in
    Right ((), go t1)

skipBalanced :: Char -> Char -> P ()
skipBalanced open close = P $ \t ->
  let
    t1 = dropTrivia t
   in
    case T.uncons t1 of
      Just (c, r) | c == open -> Right ((), eat 1 r)
      _ -> Left ("expected " <> [open])
 where
  eat :: Int -> Text -> Text
  eat 0 s = s
  eat n s = case T.uncons s of
    Nothing -> T.empty
    Just (c, r)
      | c == '"' || c == '\'' ->
          let
            (_, r2) = takeString c r
           in
            eat n r2
      | c == '/' && T.isPrefixOf "/" r ->
          eat n (T.dropWhile (/= '\n') r)
      | c == open -> eat (n + 1) r
      | c == close -> eat (n - 1) r
      | otherwise -> eat n r

stringLit :: P Text
stringLit = unquote <$> satisfyTok isStringTok

isStringTok :: Text -> Bool
isStringTok t = case T.uncons t of
  Just (c, _) -> c == '"' || c == '\''
  Nothing -> False

isNumTok :: Text -> Bool
isNumTok t = case T.uncons t of
  Just (c, _) -> isDigit c || c == '-'
  Nothing -> False

unquote :: Text -> Text
unquote t
  | T.length t >= 2 = T.dropEnd 1 (T.drop 1 t)
  | otherwise = t

mapNamed :: Text -> Maybe [Ty] -> Ty
mapNamed name margs = case (T.toLower (lastSeg name), margs) of
  ("number", _) -> TyNumber
  ("bigint", _) -> TyBigInt
  ("string", _) -> TyString
  ("boolean", _) -> TyBool
  ("bool", _) -> TyBool
  ("void", _) -> TyUnit
  ("undefined", _) -> TyUnit
  ("never", _) -> TyUnit
  ("null", _) -> TyUnit
  ("any", _) -> TyUnknown "any"
  ("unknown", _) -> TyUnknown "unknown"
  ("object", _) -> TyUnknown "object"
  ("this", _) -> TyUnknown "this"
  ("uint8array", _) -> TyUint8Array
  ("uint8clampedarray", _) -> TyUint8Array
  ("array", Just [el]) -> TyArray el
  ("readonlyarray", Just [el]) -> TyArray el
  ("array", _) -> TyArray (TyUnknown "elem")
  ("promise", Just [el]) -> TyPromise el
  ("promise", _) -> TyPromise (TyUnknown "promise")
  ("map", Just [k, v]) -> TyMap k v
  ("map", _) -> TyMap (TyUnknown "k") (TyUnknown "v")
  ("set", Just [el]) -> TySet el
  ("set", _) -> TySet (TyUnknown "elem")
  ("function", _) -> TyFun [] (TyUnknown "Function")
  _ -> TyNamed (lastSeg name)

lastSeg :: Text -> Text
lastSeg t = case T.breakOnEnd "." t of
  (_, x) | not (T.null x) -> x
  _ -> t

foldUnion :: [Ty] -> Ty
foldUnion tys =
  let
    nullish x =
      x == TyUnit || isUnk "null" x || isUnk "undefined" x
    isUnk n (TyUnknown m) = m == n
    isUnk _ _ = False
    core = filter (not . nullish) tys
    hasNull = any nullish tys
   in
    case (core, hasNull) of
      ([], _) -> TyUnit
      ([t], True) -> TyOption t
      ([t], False) -> t
      (cs, _)
        | all (== TyString) cs -> TyString
        | all (== TyNumber) cs -> TyNumber
        | all (== TyBool) cs -> TyBool
        | otherwise ->
            TyUnknown
              ("union " <> T.intercalate "|" (fmap tyNote cs))

tyNote :: Ty -> Text
tyNote = \case
  TyNumber -> "number"
  TyString -> "string"
  TyBool -> "bool"
  TyNamed n -> n
  TyUnknown n -> n
  _ -> "ty"
