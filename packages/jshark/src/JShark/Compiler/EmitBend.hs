{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Emit Bend source for the HVM2 pipeline (Bend → HVM2 → C → WASM).
-- Bend is the human-readable frontend; HVM2 is the interaction-net IR.
module JShark.Compiler.EmitBend
  ( Hvm2Error (..)
  , bendDefNames
  , emitBendKernel
  , emitKernelExportsC
  , sanitizeKernelCForWasm
  , bendDefExports
  , peelLambdas
  , sanitizeBendId
  )
where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as T
import JShark.Api.Types (Value (..))
import JShark.Compiler.Ir (IrNode (..), irPure, metaIr, data IrLiteral)
import Prelude

data BendType
  = BendU24
  | BendI24
  | BendF24
  | BendBool
  deriving (Eq, Show)

data Hvm2Error
  = Hvm2Unsupported Text
  | Hvm2ImpureKernel
  deriving (Eq, Show)

emitBendKernel :: Text -> IrNode -> Either Hvm2Error Text
emitBendKernel name ir = do
  guardPure ir
  let
    (paramTags, body) = peelLambdas ir
    paramNames = zipWith paramName paramTags [0 ..]
    paramTypes = inferParamTypes paramTags body
    env = IM.fromList (zip paramTags paramNames)
    retTy = bendTypeName (inferType body)
  bodyLines <- emitBody env body
  pure $
    T.unlines
      ( ( "def "
            <> sanitizeBendId name
            <> paramsLine paramNames paramTypes
            <> " -> "
            <> retTy
            <> ":"
        )
          : bodyLines
      )

paramsLine :: [Text] -> [BendType] -> Text
paramsLine ps ts =
  if null ps
    then "()"
    else
      "("
        <> T.intercalate ", " (zipWith (\p ty -> p <> ": " <> bendTypeName ty) ps ts)
        <> ")"

emitBody :: IntMap Text -> IrNode -> Either Hvm2Error [Text]
emitBody env = \case
  IrLetRec tag r b -> emitLetRec env tag r b
  IrIf c t e -> emitIfReturn env c t e
  IrLet tag _ x body -> do
    xTxt <- emitIrExpr env x
    let
      bindName = "v" <> T.pack (show tag)
      env' = IM.insert tag bindName env
    bodyLines <- emitBody env' body
    pure ("  " <> bindName <> " = " <> xTxt : bodyLines)
  e -> do
    eTxt <- emitIrExpr env e
    pure ["  return " <> eTxt]

indentLines :: Int -> [Text] -> [Text]
indentLines n =
  map (\line -> T.replicate n " " <> line)

emitIfReturn ::
  IntMap Text -> IrNode -> IrNode -> IrNode -> Either Hvm2Error [Text]
emitIfReturn env c t e = do
  cTxt <- emitIrExpr env c
  tTxt <- emitIrExpr env t
  eTxt <- emitIrExpr env e
  pure
    [ "  if (" <> cTxt <> ") != 0:"
    , "    return " <> tTxt
    , "  else:"
    , "    return " <> eTxt
    ]

emitLetRec ::
  IntMap Text -> Int -> IrNode -> IrNode -> Either Hvm2Error [Text]
emitLetRec env tag r b = do
  let
    recName = "rec" <> T.pack (show (abs tag))
    envRec = IM.insert tag recName env
  (fnTags, fnBody) <- peelRecFn tag r
  let
    fnNames = zipWith paramName fnTags [0 ..]
    fnTypes = inferParamTypes fnTags fnBody
    envFn =
      foldl
        (\e (t, n) -> IM.insert t n e)
        envRec
        (zip fnTags fnNames)
    retTy = bendTypeName (inferType fnBody)
  fnBodyLines <- indentLines 2 <$> emitBody envFn fnBody
  callLines <- emitBody envRec b
  pure $
    ( "  def "
        <> recName
        <> paramsLine fnNames fnTypes
        <> " -> "
        <> retTy
        <> ":"
        : fnBodyLines
    )
      ++ callLines

peelRecFn :: Int -> IrNode -> Either Hvm2Error ([Int], IrNode)
peelRecFn _ r =
  case r of
    IrLambda {} ->
      pure (peelLambdas r)
    _ ->
      Left (Hvm2Unsupported "letRec rhs must be a lambda")

emitIrExpr :: IntMap Text -> IrNode -> Either Hvm2Error Text
emitIrExpr env e =
  case e of
    IrLiteral v -> emitLiteral v
    IrVar i ->
      case IM.lookup i env of
        Just n -> pure n
        Nothing -> Left (Hvm2Unsupported ("free variable " <> T.pack (show i)))
    IrApply f x -> emitApplyCall env f x
    IrIf c t eF -> do
      cTxt <- emitIrExpr env c
      tTxt <- emitIrExpr env t
      eTxt <- emitIrExpr env eF
      pure ("(" <> tTxt <> " if (" <> cTxt <> ") != 0 else " <> eTxt <> ")")
    KPlus x y -> binop env "+" x y
    KMinus x y -> binop env "-" x y
    KTimes x y -> binop env "*" x y
    KFracDiv x y -> binop env "/" x y
    KRem x y -> binop env "%" x y
    KNegate x -> do
      xTxt <- emitIrExpr env x
      pure ("(-" <> xTxt <> ")")
    KAnd x y -> do
      xTxt <- emitIrExpr env x
      yTxt <- emitIrExpr env y
      pure ("((" <> xTxt <> ") * (" <> yTxt <> ")) != 0")
    KOr x y -> do
      xTxt <- emitIrExpr env x
      yTxt <- emitIrExpr env y
      pure ("((" <> xTxt <> ") + (" <> yTxt <> ")) != 0")
    KEq _ x y -> binop env "==" x y
    KNEq _ x y -> binop env "!=" x y
    KGTh x y -> binop env ">" x y
    KLTh x y -> binop env "<" x y
    KGTEq x y -> binop env ">=" x y
    KLTEq x y -> binop env "<=" x y
    IrLambda {} ->
      Left (Hvm2Unsupported "nested lambda in HVM2 kernel body")
    IrLet {} ->
      Left (Hvm2Unsupported "let outside kernel body walker")
    IrLetRec {} ->
      Left (Hvm2Unsupported "letrec in expression position")
    IrLift {} -> Left Hvm2ImpureKernel
    IrOptionCase {} ->
      Left (Hvm2Unsupported "Option")
    IrResultOk {} ->
      Left (Hvm2Unsupported "Result")
    IrResultErr {} ->
      Left (Hvm2Unsupported "Result")
    IrResultCase {} ->
      Left (Hvm2Unsupported "Result")
    IrIndex {} ->
      Left (Hvm2Unsupported "array index")
    IrU8Index {} ->
      Left (Hvm2Unsupported "Uint8Array")
    IrError {} ->
      Left (Hvm2Unsupported "Error")
    IrFixed {} ->
      Left (Hvm2Unsupported "stdlib fixed op")
    IrFnLit {} ->
      Left (Hvm2Unsupported "FnLit")
    IrUnsafeNullable {} ->
      Left (Hvm2Unsupported "nullable")
    IrFrozenLit {} ->
      Left (Hvm2Unsupported "object")
    IrGetField {} ->
      Left (Hvm2Unsupported "field access")
    IrHvm2Ref {} ->
      Left (Hvm2Unsupported "nested Hvm2Kernel")
    _ ->
      Left (Hvm2Unsupported "effect node in HVM2 kernel body")

emitApplyCall ::
  IntMap Text -> IrNode -> IrNode -> Either Hvm2Error Text
emitApplyCall env f x = do
  let
    (fn, args) = collectApplySpine f x
  fnTxt <- emitIrExpr env fn
  argTxts <- traverse (emitIrExpr env) args
  pure (fnTxt <> "(" <> T.intercalate ", " argTxts <> ")")

collectApplySpine :: IrNode -> IrNode -> (IrNode, [IrNode])
collectApplySpine f x =
  case f of
    IrApply f' x' ->
      let
        (fn, args) = collectApplySpine f' x'
       in
        (fn, args ++ [x])
    _ ->
      (f, [x])

binop :: IntMap Text -> Text -> IrNode -> IrNode -> Either Hvm2Error Text
binop env op x y = do
  xTxt <- emitIrExpr env x
  yTxt <- emitIrExpr env y
  pure ("(" <> xTxt <> " " <> op <> " " <> yTxt <> ")")

emitLiteral :: Value u -> Either Hvm2Error Text
emitLiteral = \case
  -- JS numbers are floats; emit f24 literals (always with a decimal point)
  -- so Bend kernels compute the same math the JS reference does.
  ValueNumber d -> pure (T.pack (show d))
  ValueBool b -> pure (if b then "1" else "0")
  ValueBigInt {} ->
    Left (Hvm2Unsupported "BigInt literal")
  ValueString {} ->
    Left (Hvm2Unsupported "string literal")
  ValueUnit ->
    Left (Hvm2Unsupported "unit")
  ValueOption {} ->
    Left (Hvm2Unsupported "option")
  ValueResult {} ->
    Left (Hvm2Unsupported "result")
  ValueRegex {} ->
    Left (Hvm2Unsupported "regex")
  ValueUint8Array {} ->
    Left (Hvm2Unsupported "Uint8Array")
  ValueArray {} ->
    Left (Hvm2Unsupported "array")
  ValueFunction {} ->
    Left (Hvm2Unsupported "function")
  ValueFrozen {} ->
    Left (Hvm2Unsupported "object")

peelLambdas :: IrNode -> ([Int], IrNode)
peelLambdas ir =
  case ir of
    IrLambda tag _ body ->
      let
        (rest, inner) = peelLambdas body
       in
        (tag : rest, inner)
    _ ->
      ([], ir)

paramName :: Int -> Int -> Text
paramName tag _ = "a" <> T.pack (show (abs tag))

inferParamTypes :: [Int] -> IrNode -> [BendType]
inferParamTypes tags body = map (`inferParamType` body) tags

-- | JShark 'Number' is an f64; f24 is Bend's float, so every numeric
-- parameter maps to f24.
inferParamType :: Int -> IrNode -> BendType
inferParamType _ _ = BendF24

inferType :: IrNode -> BendType
inferType e =
  case e of
    IrLiteral (ValueBool _) -> BendBool
    IrLiteral (ValueNumber _) -> BendF24
    KAnd _ _ -> BendBool
    KOr _ _ -> BendBool
    KEq _ _ _ -> BendBool
    KNEq _ _ _ -> BendBool
    KGTh _ _ -> BendBool
    KLTh _ _ -> BendBool
    KGTEq _ _ -> BendBool
    KLTEq _ _ -> BendBool
    _ -> BendF24

bendTypeName :: BendType -> Text
bendTypeName = \case
  BendU24 -> "u24"
  BendI24 -> "i24"
  BendF24 -> "f24"
  BendBool -> "Bool"

sanitizeBendId :: Text -> Text
sanitizeBendId t =
  let
    base = T.map (\c -> if c `elem` (['_', '-'] :: [Char]) then '_' else c) t
   in
    if T.null base || not (isAlpha (T.head base))
      then "k_" <> base
      else base
 where
  isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

bendDefNames :: Text -> [Text]
bendDefNames src = map fst (bendDefExports src)

bendDefExports :: Text -> [(Text, Int)]
bendDefExports src =
  [ (name, arity)
  | line <- T.lines src
  , isTopLevelDef line
  , Just rest <- [T.stripPrefix "def " (T.stripStart line)]
  , (name, after) <- [T.breakOn "(" rest]
  , not (T.null name)
  , T.all (\c -> c /= ':' && c /= ' ') name
  , name /= "main"
  , -- pipeline-internal driver, bridged by hand (8-ary, tuple-tree result)
  name /= "jshark_grid"
  , T.isPrefixOf "(" after
  , let
      params = T.takeWhile (/= ')') (T.drop 1 after)
      arity = paramArity params
  ]
 where
  isTopLevelDef line =
    not (T.isPrefixOf " " line)
      && not (T.isPrefixOf "\t" line)
  paramArity params
    | T.null (T.filter (not . isSpace) params) = 0
    | otherwise = 1 + T.length (T.filter (== ',') params)
  isSpace c = c == ' ' || c == '\t'

emitKernelExportsC :: [(Text, Int)] -> Text
emitKernelExportsC _exports =
  T.unlines $
    [ "/* WASM exports live in kernel.c (emitKernelWasmBridge). */"
    , "typedef int jshark_hvm2_exports_stub;"
    , ""
    ]

guardPure :: IrNode -> Either Hvm2Error ()
guardPure ir =
  if irPure (metaIr ir)
    then Right ()
    else Left Hvm2ImpureKernel

-- | Strip Bend @gen-c@ output for freestanding WASM (no IO/dlfcn/main).
-- Also shrinks HVM2 buffers to a one-net wasm32 size and fixes two upstream constants
-- that silently break in that environment: @ROOT@ indexes the last var slot
-- of the original 2^29 buffer (out of bounds once shrunk), and @TPC_L2@ is
-- generated from the build host's core count — clamp via @#ifndef TPC_L2@ so
-- the Zig build (-Dtpc-l2=…) controls browser thread count.
sanitizeKernelCForWasm :: Text -> Text
sanitizeKernelCForWasm src =
  let
    noIo = T.replace "#define IO\n" "" src
    noMain = T.replace "#define WITH_MAIN\n" "" noIo
    noInttypes =
      T.replace "#include <inttypes.h>\n" "" noMain
    shrunk =
      T.replace "#define G_NODE_LEN (1ul << 29)" "#define G_NODE_LEN (1ul << 23)"
        . T.replace "#define G_VARS_LEN (1ul << 29)" "#define G_VARS_LEN (1ul << 23)"
        . T.replace "#define RLEN (1ul << 24)" "#define RLEN (1ul << 18)"
        . T.replace "Def defs_buf[0x4000]" "Def defs_buf[32]"
        . T.replace "FFn ffns_buf[0x4000]" "FFn ffns_buf[32]"
        . T.replace
          "  while (get_tag(var) == VAR) {"
          "  u32 enter_lim = 4096;\n  while (get_tag(var) == VAR && enter_lim-- > 0) {"
        . T.replace
          "  while (true) {\n    tick += 1;"
          "  while (tick < 50000000) {\n    tick += 1;"
        . T.replace
          "#define ROOT 0xFFFFFFF8"
          "#define ROOT ((Port)((G_VARS_LEN - 1) << 3)) // last var slot (shrunk)"
        . T.replace
          ( "static inline u64 time64() {\n"
              <> "  struct timespec ts;\n"
              <> "  clock_gettime(CLOCK_MONOTONIC, &ts);\n"
              <> "  return (u64)ts.tv_sec * 1000000000ULL + (u64)ts.tv_nsec;\n"
              <> "}"
          )
          "static inline u64 time64() { return 0; }"
        $ noInttypes
    trimmed =
      case T.breakOn "#include <dlfcn.h>" shrunk of
        (before, _) -> T.stripEnd before
    clampTpc = clampTpcFromZig trimmed
   in
    "#include \"hvm2_wasm.h\"\n" <> clampTpc <> "\n"
 where
  clampTpcFromZig =
    T.unlines . map fixTpc . T.lines
  fixTpc line
    | "#define TPC_L2 " `T.isPrefixOf` line =
        -- TPC_L2 comes from the Zig build (-Dtpc-l2=…); do not pin to 0.
        "#ifndef TPC_L2\n#define TPC_L2 2\n#endif"
    | otherwise = line
