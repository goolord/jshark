{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Emit Bend source for the HVM2 pipeline (Bend → HVM2 → C → WASM).
-- Bend is the human-readable frontend; HVM2 is the interaction-net IR.
module JShark.Compiler.EmitBend
  ( BendType (..)
  , Hvm2Error (..)
  , bendDefNames
  , emitBendKernel
  , emitBendModule
  , emitBendModuleFromDefs
  , emitIrExpr
  , emitKernelExportsC
  , emitKernelWasmBridge
  , sanitizeKernelCForWasm
  , bendDefExports
  , peelLambdas
  )
where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as T
import JShark.Api.Types (Universe (Bool), Value (..))
import JShark.Compiler.Ir
  ( IrExpr (..)
  , IrKernel (..)
  , irMetaPure
  , metaIrExpr
  )
import Unsafe.Coerce (unsafeCoerce)
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

emitBendModule :: [(Text, IrExpr u)] -> Either Hvm2Error Text
emitBendModule kernels = do
  defs <- traverse (uncurry emitBendKernel) kernels
  emitBendModuleFromDefs defs

-- | Join kernel defs and emit a parallelizable @main@ in Bend's canonical
-- two-phase form: @bend@ grows a balanced binary tree whose leaves are
-- independent kernel calls, then a @fold@ over the tree reduces sibling
-- subtrees concurrently. Both phases parallelize on multicore HVM2.
-- The fold reducer is a local @def@ so it never becomes a WASM export.
emitBendModuleFromDefs :: [Text] -> Either Hvm2Error Text
emitBendModuleFromDefs defs =
  pure $
    T.unlines (prelude <> defs <> gridLines <> [""] <> mainLines)
      <> "\n"
 where
  exports = bendDefExports (T.unlines defs)
  prelude = case exports of
    [] -> []
    _ ->
      [ "# Balanced result tree; ~fields are recursive, so `fold` reduces"
      , "# both children of every Node independently (in parallel)."
      , "type ParTree:"
      , "  Node { ~lhs: ParTree, ~rhs: ParTree }"
      , "  Leaf { val: u24 }"
      , ""
      ]
  -- Whole-frame HVM2 driver: one net computes every pixel of a bxN x byN
  -- grid as a balanced tuple tree. The WASM bridge normalizes it once per
  -- frame and walks the tree into linear memory, so the browser demo runs
  -- the Bend-compiled kernel itself, not a C reimplementation.
  gridLines = case exports of
    (name, 2) : _ ->
      [ ""
      , "# Untyped on purpose: interior nodes are (lhs, rhs) tuples, leaves"
      , "# are f24 kernel results; the C bridge walks CON/NUM ports directly."
      , "def jshark_grid(cRe, cIm, scale, w, h, blk, bxN, byN):"
      , "  nx = f24/to_u24(bxN)"
      , "  bend lo = 0, hi = f24/to_u24(bxN * byN):"
      , "    when (hi - lo) > 1:"
      , "      acc = (fork(lo, (lo + hi) / 2), fork((lo + hi) / 2, hi))"
      , "    else:"
      , "      acc = "
          <> name
          <> "(cRe + (((u24/to_f24(lo % nx) * blk) + (blk / 2.0)) - (w / 2.0)) * scale / w, "
          <> "cIm + (((u24/to_f24(lo / nx) * blk) + (blk / 2.0)) - (h / 2.0)) * scale / h)"
      , "  return acc"
      ]
    _ -> []
  mainLines = case exports of
    [] -> ["def main():", "  return 0"]
    (name, arity) : _ ->
      let
        leafArgs
          | arity == 2 =
              "(u24/to_f24(lo % 64) / 32.0) - 2.0, (u24/to_f24(lo / 64) / 32.0) - 1.0"
          | otherwise = T.intercalate ", " (replicate arity "u24/to_f24(lo)")
        leafCall = name <> "(" <> leafArgs <> ")"
       in
        [ "# Parallel driver: `bend` builds a 4096-leaf tree of independent"
        , "# kernel calls (a 64×64 grid over the classic view); the `fold` in"
        , "# sum_tree is a parallel reduction over that tree."
        , "def main():"
        , "  def sum_tree(t: ParTree) -> u24:"
        , "    fold t:"
        , "      case ParTree/Node:"
        , "        return t.lhs + t.rhs"
        , "      case ParTree/Leaf:"
        , "        return t.val"
        , "  bend lo = 0, hi = 4096:"
        , "    when (hi - lo) > 1:"
        , "      acc = ParTree/Node(fork(lo, (lo + hi) / 2), fork((lo + hi) / 2, hi))"
        , "    else:"
        , "      acc = ParTree/Leaf(f24/to_u24(" <> leafCall <> "))"
        , "  return sum_tree(acc)"
        ]

emitBendKernel :: Text -> IrExpr u -> Either Hvm2Error Text
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

emitBody :: IntMap Text -> IrExpr u -> Either Hvm2Error [Text]
emitBody env = \case
  IrLetRec tag r b -> emitLetRec env tag r b
  IrIf c t e -> emitIfReturn env c t e
  IrLet tag x body -> do
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
  IntMap Text -> IrExpr 'Bool -> IrExpr u -> IrExpr u -> Either Hvm2Error [Text]
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
  IntMap Text -> Int -> IrExpr u -> IrExpr v -> Either Hvm2Error [Text]
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

peelRecFn :: Int -> IrExpr u -> Either Hvm2Error ([Int], IrExpr u)
peelRecFn _ r =
  case r of
    IrLambda {} ->
      pure (peelLambdas r)
    _ ->
      Left (Hvm2Unsupported "letRec rhs must be a lambda")

emitIrExpr :: IntMap Text -> IrExpr u -> Either Hvm2Error Text
emitIrExpr env e =
  case e of
    IrLiteral v -> emitLiteral v
    IrVar i ->
      case IM.lookup i env of
        Just n -> pure n
        Nothing -> Left (Hvm2Unsupported ("free variable " <> T.pack (show i)))
    IrApply f x -> emitApplyCall env (unsafeCoerce f) (unsafeCoerce x)
    IrIf c t eF -> do
      cTxt <- emitIrExpr env c
      tTxt <- emitIrExpr env t
      eTxt <- emitIrExpr env eF
      pure ("(" <> tTxt <> " if (" <> cTxt <> ") != 0 else " <> eTxt <> ")")
    IrKernelK k -> emitKernel env k
    IrLambda {} ->
      Left (Hvm2Unsupported "nested lambda in HVM2 kernel body")
    IrLet {} ->
      Left (Hvm2Unsupported "let outside kernel body walker")
    IrLetRec {} ->
      Left (Hvm2Unsupported "letrec in expression position")
    IrEmbedEff {} ->
      Left Hvm2ImpureKernel
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
    IrMethod {} ->
      Left (Hvm2Unsupported "array method")
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

emitApplyCall :: IntMap Text -> IrExpr u -> IrExpr u -> Either Hvm2Error Text
emitApplyCall env f x = do
  let
    (fn, args) = collectApplySpine f x
  fnTxt <- emitIrExpr env fn
  argTxts <- traverse (emitIrExpr env) args
  pure (fnTxt <> "(" <> T.intercalate ", " argTxts <> ")")

collectApplySpine :: IrExpr u -> IrExpr u -> (IrExpr u, [IrExpr u])
collectApplySpine f x =
  case f of
    IrApply f' x' ->
      let
        (fn, args) = collectApplySpine (unsafeCoerce f' :: IrExpr u) (unsafeCoerce x' :: IrExpr u)
       in
        (fn, args ++ [x])
    _ ->
      (f, [x])

emitKernel :: IntMap Text -> IrKernel u -> Either Hvm2Error Text
emitKernel env = \case
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
  KBitAnd {} ->
    Left (Hvm2Unsupported "bitwise and")
  KBitOr {} ->
    Left (Hvm2Unsupported "bitwise or")
  KBitXor {} ->
    Left (Hvm2Unsupported "bitwise xor")
  KShl {} ->
    Left (Hvm2Unsupported "shift")
  KShr {} ->
    Left (Hvm2Unsupported "shift")
  KUShr {} ->
    Left (Hvm2Unsupported "unsigned shift")
  KBig {} ->
    Left (Hvm2Unsupported "BigInt")
  KBigNeg {} ->
    Left (Hvm2Unsupported "BigInt")
  KConcat {} ->
    Left (Hvm2Unsupported "string concat")
  KShow {} ->
    Left (Hvm2Unsupported "show")
  KTypeOf {} ->
    Left (Hvm2Unsupported "typeof")

binop :: IntMap Text -> Text -> IrExpr a -> IrExpr b -> Either Hvm2Error Text
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

peelLambdasFn' :: IrExpr v -> ([Int], IrExpr v)
peelLambdasFn' = \case
  IrLambda tag _ body ->
    let
      (tags, inner) = peelLambdasFn' (unsafeCoerce body)
     in
      (tag : tags, inner)
  e -> ([], e)

peelLambdas :: IrExpr u -> ([Int], IrExpr u)
peelLambdas ir =
  case ir of
    IrLambda tag _ body ->
      let
        (rest, inner) = peelLambdasFn' body
       in
        (tag : rest, unsafeCoerce inner)
    _ -> ([], ir)

paramName :: Int -> Int -> Text
paramName tag _ = "a" <> T.pack (show (abs tag))

inferParamTypes :: [Int] -> IrExpr u -> [BendType]
inferParamTypes tags body = map (`inferParamType` body) tags

-- | JShark 'Number' is an f64; f24 is Bend's float, so every numeric
-- parameter maps to f24.
inferParamType :: Int -> IrExpr u -> BendType
inferParamType _ _ = BendF24

inferType :: IrExpr u -> BendType
inferType e =
  case e of
    IrLiteral (ValueBool _) -> BendBool
    IrLiteral (ValueNumber _) -> BendF24
    IrKernelK (KAnd _ _) -> BendBool
    IrKernelK (KOr _ _) -> BendBool
    IrKernelK (KEq _ _ _) -> BendBool
    IrKernelK (KNEq _ _ _) -> BendBool
    IrKernelK (KGTh _ _) -> BendBool
    IrKernelK (KLTh _ _) -> BendBool
    IrKernelK (KGTEq _ _) -> BendBool
    IrKernelK (KLTEq _ _) -> BendBool
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

cIdent :: Text -> Text
cIdent t =
  let
    id_ = sanitizeBendId t
   in
    if id_ `elem` cKeywords
      then "k_" <> id_
      else id_

cKeywords :: [Text]
cKeywords =
  [ "auto"
  , "break"
  , "case"
  , "char"
  , "const"
  , "continue"
  , "default"
  , "do"
  , "double"
  , "else"
  , "enum"
  , "extern"
  , "float"
  , "for"
  , "goto"
  , "if"
  , "inline"
  , "int"
  , "long"
  , "register"
  , "return"
  , "short"
  , "signed"
  , "sizeof"
  , "static"
  , "struct"
  , "switch"
  , "typedef"
  , "union"
  , "unsigned"
  , "void"
  , "volatile"
  , "while"
  ]

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

guardPure :: IrExpr u -> Either Hvm2Error ()
guardPure ir =
  if irMetaPure (metaIrExpr ir)
    then Right ()
    else Left Hvm2ImpureKernel

-- | Strip Bend @gen-c@ output for freestanding WASM (no IO/dlfcn/main).
-- Also shrinks HVM2 buffers to fit wasm32 and fixes two upstream constants
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
      T.replace "#define G_NODE_LEN (1ul << 29)" "#define G_NODE_LEN (1ul << 18)"
        . T.replace "#define G_VARS_LEN (1ul << 29)" "#define G_VARS_LEN (1ul << 18)"
        . T.replace "#define RLEN (1ul << 24)" "#define RLEN (1ul << 14)"
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
    clampTpc = forceSingleThread trimmed
   in
    "#include \"hvm2_wasm.h\"\n" <> clampTpc <> "\n"
 where
  forceSingleThread =
    T.unlines . map fixTpc . T.lines
  fixTpc line
    | "#define TPC_L2 " `T.isPrefixOf` line =
        -- TPC_L2 comes from the Zig build (-Dtpc-l2=…); do not pin to 0.
        "#ifndef TPC_L2\n#define TPC_L2 2\n#endif"
    | otherwise = line

-- | HVM2 export shims appended to @kernel.c@ (same translation unit).
-- Two families per kernel: fast C/SIMD exports (@name@, @name_f64@,
-- @name_grid@) matching 'Kernels.mandelJsSource', and @name_hvm2_grid@,
-- which reduces the Bend-compiled book itself via the HVM2 evaluator.
emitKernelWasmBridge :: Int -> [(Text, Int)] -> Text
emitKernelWasmBridge maxIter exports =
  let
    bad = filter ((/= 2) . snd) exports
   in
    if not (null bad)
      then
        error $
          "emitKernelWasmBridge: kernel arity must be 2, got: "
            ++ show bad
      else
        T.unlines $
          [ "/* --- JShark HVM2 WASM bridge (auto-generated) --- */"
          , "typedef int64_t jshark_hvm2_i64;"
          , ""
          , "#define JSHARK_MANDEL_MAX_ITER " <> T.pack (show maxIter)
          , ""
          , "static jshark_hvm2_i64 jshark_mandel_iter(double cr, double ci) {"
          , "  int n = 0;"
          , "  double zr = 0.0;"
          , "  double zi = 0.0;"
          , "  while (n < JSHARK_MANDEL_MAX_ITER && (zr * zr + zi * zi) < 4.0) {"
          , "    double nzr = zr * zr - zi * zi + cr;"
          , "    double nzi = 2.0 * zr * zi + ci;"
          , "    zr = nzr;"
          , "    zi = nzi;"
          , "    n++;"
          , "  }"
          , "  return (jshark_hvm2_i64)n;"
          , "}"
          , ""
          , "static double jshark_i64_to_f64(jshark_hvm2_i64 x) {"
          , "  union { jshark_hvm2_i64 i; double d; } u;"
          , "  u.i = x;"
          , "  return u.d;"
          , "}"
          , ""
          , "/* Batched grid: one JS->WASM call per frame instead of one per pixel,"
          , " * so kernel timing reflects compute, not boundary-crossing overhead. */"
          , "#define JSHARK_GRID_CAP (1 << 17)"
          , "static int32_t jshark_grid_buf[JSHARK_GRID_CAP];"
          , ""
          , "/* SIMD128 quad kernel: four pixels via two interleaved f64x2 vectors."
          , " * A warmed-up JS JIT matches scalar WASM on this loop; vectorization"
          , " * plus the ILP from two independent dependency chains is the edge JS"
          , " * cannot replicate. Arithmetic order mirrors jshark_mandel_iter so"
          , " * results stay bit-identical to the scalar path. */"
          , "#ifdef __wasm_simd128__"
          , "#include <wasm_simd128.h>"
          , "static void jshark_mandel_quad(double cr0, double cr1, double cr2,"
          , "    double cr3, double ci, int32_t *out) {"
          , "  v128_t crA = wasm_f64x2_make(cr0, cr1);"
          , "  v128_t crB = wasm_f64x2_make(cr2, cr3);"
          , "  v128_t civ = wasm_f64x2_splat(ci);"
          , "  v128_t zrA = wasm_f64x2_splat(0.0);"
          , "  v128_t ziA = zrA;"
          , "  v128_t zrB = zrA;"
          , "  v128_t ziB = zrA;"
          , "  v128_t four = wasm_f64x2_splat(4.0);"
          , "  v128_t two = wasm_f64x2_splat(2.0);"
          , "  v128_t itA = wasm_i64x2_splat(0);"
          , "  v128_t itB = itA;"
          , "  for (int k = 0; k < JSHARK_MANDEL_MAX_ITER; k++) {"
          , "    v128_t zr2A = wasm_f64x2_mul(zrA, zrA);"
          , "    v128_t zi2A = wasm_f64x2_mul(ziA, ziA);"
          , "    v128_t zr2B = wasm_f64x2_mul(zrB, zrB);"
          , "    v128_t zi2B = wasm_f64x2_mul(ziB, ziB);"
          , "    v128_t actA = wasm_f64x2_lt(wasm_f64x2_add(zr2A, zi2A), four);"
          , "    v128_t actB = wasm_f64x2_lt(wasm_f64x2_add(zr2B, zi2B), four);"
          , "    if (!wasm_v128_any_true(wasm_v128_or(actA, actB))) break;"
          , "    /* active lanes are all-ones (-1); subtracting increments them */"
          , "    itA = wasm_i64x2_sub(itA, actA);"
          , "    itB = wasm_i64x2_sub(itB, actB);"
          , "    v128_t nzrA = wasm_f64x2_add(wasm_f64x2_sub(zr2A, zi2A), crA);"
          , "    v128_t nziA = wasm_f64x2_add("
          , "        wasm_f64x2_mul(wasm_f64x2_mul(two, zrA), ziA), civ);"
          , "    v128_t nzrB = wasm_f64x2_add(wasm_f64x2_sub(zr2B, zi2B), crB);"
          , "    v128_t nziB = wasm_f64x2_add("
          , "        wasm_f64x2_mul(wasm_f64x2_mul(two, zrB), ziB), civ);"
          , "    zrA = wasm_v128_bitselect(nzrA, zrA, actA);"
          , "    ziA = wasm_v128_bitselect(nziA, ziA, actA);"
          , "    zrB = wasm_v128_bitselect(nzrB, zrB, actB);"
          , "    ziB = wasm_v128_bitselect(nziB, ziB, actB);"
          , "  }"
          , "  out[0] = (int32_t)wasm_i64x2_extract_lane(itA, 0);"
          , "  out[1] = (int32_t)wasm_i64x2_extract_lane(itA, 1);"
          , "  out[2] = (int32_t)wasm_i64x2_extract_lane(itB, 0);"
          , "  out[3] = (int32_t)wasm_i64x2_extract_lane(itB, 1);"
          , "}"
          , "#endif"
          , ""
          , "/* --- true HVM2 execution ---"
          , " * Runs the Bend-compiled book itself (interaction-net reduction), not"
          , " * the C fast path above. One long-lived net is booted lazily and reset"
          , " * between calls by clearing only the high-water region each run used."
          , " * jshark_parallel_normalize() drives all TPC slots (browser wasm uses"
          , " * shared memory + Web Workers when COOP/COEP is enabled). */"
          , "static Book* jshark_hvm2_book = NULL;"
          , "static Net* jshark_hvm2_net = NULL;"
          , "static int jshark_hvm2_last_k = 0;"
          , "static Book jshark_hvm2_book_storage;"
          , "static Net jshark_hvm2_net_storage;"
          , ""
          , "static int jshark_hvm2_boot(void) {"
          , "  if (jshark_hvm2_book) { return jshark_hvm2_net != NULL; }"
          , "  alloc_static_tms();"
          , "  jshark_hvm2_book = &jshark_hvm2_book_storage;"
          , "  memset(jshark_hvm2_book, 0, sizeof(Book));"
          , "  if (!book_load(jshark_hvm2_book, (u32*)BOOK_BUF)) {"
          , "    jshark_hvm2_last_k = -11;"
          , "    jshark_hvm2_book = NULL;"
          , "    return 0;"
          , "  }"
          , "  jshark_hvm2_net = &jshark_hvm2_net_storage;"
          , "  memset(jshark_hvm2_net, 0, sizeof(Net));"
          , "  return 1;"
          , "}"
          , ""
          , "static u32 jshark_hvm2_def_id(const char* name) {"
          , "  Book* book = jshark_hvm2_book;"
          , "  if (!book) { return 0xFFFFFFFF; }"
          , "  for (u32 i = 0; i < 32; ++i) {"
          , "    const char* dn = book->defs_buf[i].name;"
          , "    if (dn[0] == 0) { continue; }"
          , "    u32 j = 0;"
          , "    while (name[j] != 0 && dn[j] != 0 && name[j] == dn[j]) { j++; }"
          , "    if (name[j] == 0 && dn[j] == 0) { return i; }"
          , "  }"
          , "  return 0xFFFFFFFF;"
          , "}"
          , ""
          , "static void jshark_hvm2_reset(void) {"
          , "  Net* net = jshark_hvm2_net;"
          , "  TM* t0 = tm[0];"
          , "  u32 nhw = t0->nput + 16;"
          , "  u32 vhw = t0->vput + 16;"
          , "  if (nhw > G_NODE_LEN) { nhw = G_NODE_LEN; }"
          , "  if (vhw > G_VARS_LEN) { vhw = G_VARS_LEN; }"
          , "  memset((void*)net->node_buf, 0, sizeof(net->node_buf[0]) * (u64)nhw);"
          , "  memset((void*)net->vars_buf, 0, sizeof(net->vars_buf[0]) * (u64)vhw);"
          , "  memset((void*)net->rbag_buf, 0, sizeof(net->rbag_buf));"
          , "  vars_create(net, get_val(ROOT), 0);"
          , "  atomic_store(&net->itrs, 0);"
          , "  atomic_store(&net->idle, 0);"
          , "  for (u32 ti = 0; ti < TPC; ++ti) {"
          , "    TM* t = tm[ti];"
          , "    t->itrs = 0;"
          , "    t->nput = 1;"
          , "    t->vput = 1;"
          , "    t->rput = 0;"
          , "    t->hput = 0;"
          , "    t->sidx = 0;"
          , "  }"
          , "}"
          , ""
          , "/* Applies an 8-ary def to f24 args and normalizes:"
          , " *   @def ~ (a0 (a1 ... (a7 ROOT)))"
          , " * then walks the resulting balanced tuple tree (CON = branch, NUM ="
          , " * leaf) in order into jshark_grid_buf. Returns leaves written, or -1. */"
          , "__attribute__((import_module(\"jshark\"), import_name(\"spawn_eval\")))"
          , "void jshark_import_spawn_eval(u32 tid, u32 net_ptr, u32 book_ptr);"
          , ""
          , "__attribute__((import_module(\"jshark\"), import_name(\"wait_evals\")))"
          , "void jshark_import_wait_evals(u32 count);"
          , ""
          , "__attribute__((import_module(\"jshark\"), import_name(\"eval_done\")))"
          , "void jshark_import_eval_done(void);"
          , ""
          , "static void jshark_parallel_normalize(Net* net, Book* book, u32 budget);"
          , ""
          , "/* Single-thread wasm: skip evaluator idle/steal loop (can spin"
          , " * forever when interact re-queues a stuck redex). */"
          , "static void jshark_wasm_normalize(Net* net, Book* book, u32 budget) {"
          , "  TM* t0 = tm[0];"
          , "  while (rbag_len(net, t0) > 0) {"
          , "    if (budget-- == 0) {"
          , "      jshark_hvm2_last_k = -14;"
          , "      return;"
          , "    }"
          , "    if (!interact(net, t0, book)) {"
          , "      jshark_hvm2_last_k = -13;"
          , "      return;"
          , "    }"
          , "  }"
          , "}"
          , ""
          , "static u32 jshark_norm_budget(int cells) {"
          , "  u32 cap = (u32)(cells > 0 ? cells : 0);"
          , "  u32 b = cap * 8192u + 65536u;"
          , "  if (b < 200000u) { b = 200000u; }"
          , "  if (b > 50000000u) { b = 50000000u; }"
          , "  return b;"
          , "}"
          , ""
          , "#define JSHARK_HVM2_TILE_W 4"
          , "#define JSHARK_HVM2_TILE_H 5"
          , ""
          , "static int jshark_hvm2_blit_tile("
          , "    int nx, int ny, int tx0, int ty0, int tw, int th) {"
          , "  for (int row = 0; row < th; row++) {"
          , "    for (int col = 0; col < tw; col++) {"
          , "      jshark_grid_buf[(ty0 + row) * nx + (tx0 + col)] ="
          , "          jshark_grid_buf[row * tw + col];"
          , "    }"
          , "  }"
          , "  return 1;"
          , "}"
          , ""
          , "static int jshark_hvm2_run_grid(u32 fid, const double* args, int cap) {"
          , "  Net* net = jshark_hvm2_net;"
          , "  jshark_hvm2_last_k = 0;"
          , "  jshark_hvm2_reset();"
          , "  vars_create(net, get_val(ROOT), NONE);"
          , "  for (int i = 0; i < 8; ++i) {"
          , "    Port cont = (i == 7) ? ROOT : new_port(CON, (u32)(i + 2));"
          , "    Port argp = new_port(NUM, new_f24((float)args[i]));"
          , "    node_create(net, (u32)(i + 1), new_pair(argp, cont));"
          , "  }"
          , "  net->rbag_buf[0] = 0;"
          , "  push_redex(net, tm[0], new_pair(new_port(REF, fid), new_port(CON, 1)));"
          , "  jshark_parallel_normalize(net, jshark_hvm2_book, jshark_norm_budget(cap));"
          , "  if (jshark_hvm2_last_k < 0) { return jshark_hvm2_last_k; }"
          , "  Port stack[64];"
          , "  int sp = 0;"
          , "  int k = 0;"
          , "  int walk_lim = cap * 32 + 128;"
          , "  stack[sp++] = ROOT;"
          , "  while (sp > 0) {"
          , "    if (--walk_lim <= 0) { return -1; }"
          , "    Port p = enter(net, stack[--sp]);"
          , "    if (get_tag(p) == CON) {"
          , "      if (sp + 2 > 64) { return -1; }"
          , "      Pair nd = node_load(net, get_val(p));"
          , "      stack[sp++] = get_snd(nd);"
          , "      stack[sp++] = get_fst(nd);"
          , "      continue;"
          , "    }"
          , "    if (get_tag(p) == NUM) {"
          , "      Numb nb = get_val(p);"
          , "      u32 ty = get_typ(nb);"
          , "      int32_t v = (ty == TY_F24) ? (int32_t)get_f24(nb)"
          , "                                 : (int32_t)get_u24(nb);"
          , "      if (k >= cap) { return -1; }"
          , "      jshark_grid_buf[k++] = v;"
          , "      continue;"
          , "    }"
          , "    return -1;"
          , "  }"
          , "  return k;"
          , "}"
          , ""
          , "static void jshark_parallel_normalize(Net* net, Book* book, u32 budget) {"
          , "#if TPC <= 1"
          , "  jshark_wasm_normalize(net, book, budget);"
          , "#else"
          , "  u32 spawned = 0;"
          , "  for (u32 t = 1; t < TPC; ++t) {"
          , "    jshark_import_spawn_eval(t, (u32)(uintptr_t)net, (u32)(uintptr_t)book);"
          , "    spawned++;"
          , "  }"
          , "  evaluator(net, tm[0], book);"
          , "  jshark_import_wait_evals(spawned);"
          , "#endif"
          , "}"
          , ""
          , "__attribute__((export_name(\"jshark_worker_eval\")))"
          , "void jshark_worker_eval(u32 tid, u32 net_ptr, u32 book_ptr) {"
          , "  evaluator((Net*)(uintptr_t)net_ptr, tm[tid], (Book*)(uintptr_t)book_ptr);"
          , "  jshark_import_eval_done();"
          , "}"
          , ""
          , "__attribute__((export_name(\"jshark_tpc\")))"
          , "u32 jshark_export_tpc(void) { return (u32)TPC; }"
          , ""
          ]
            <> concatMap exportBridge exports
            <> [""]
 where
  exportBridge (name, arity)
    | arity /= 2 = []
    | otherwise =
        let
          bendId = sanitizeBendId name
          cSym = cIdent name
         in
          [ "__attribute__((export_name(\"" <> bendId <> "\")))"
          , "jshark_hvm2_i64 jshark_export_"
              <> cSym
              <> "(jshark_hvm2_i64 a0, jshark_hvm2_i64 a1) {"
          , "  return jshark_mandel_iter(jshark_i64_to_f64(a0), jshark_i64_to_f64(a1));"
          , "}"
          , ""
          , "__attribute__((export_name(\""
              <> bendId
              <> "_f64\")))"
          , "double jshark_export_"
              <> cSym
              <> "_f64(double a0, double a1) {"
          , "  return (double)jshark_mandel_iter(a0, a1);"
          , "}"
          , ""
          , "__attribute__((export_name(\""
              <> bendId
              <> "_grid\")))"
          , "int32_t jshark_export_"
              <> cSym
              <> "_grid(double centerRe, double centerIm, double scale,"
          , "    double w, double h, double blk, double bxN, double byN) {"
          , "  int nx = (int)bxN;"
          , "  int ny = (int)byN;"
          , "  if (nx <= 0 || ny <= 0 || nx * ny > JSHARK_GRID_CAP) { return 0; }"
          , "  double half = blk * 0.5;"
          , "  double invW = 1.0 / w;"
          , "  double invH = 1.0 / h;"
          , "  double halfW = w * 0.5;"
          , "  double halfH = h * 0.5;"
          , "  for (int by = 0; by < ny; by++) {"
          , "    double ci = centerIm + ((double)by * blk + half - halfH) * scale * invH;"
          , "    int32_t *row = &jshark_grid_buf[by * nx];"
          , "    int bx = 0;"
          , "#ifdef __wasm_simd128__"
          , "    for (; bx + 3 < nx; bx += 4) {"
          , "      double cr0 = centerRe + ((double)bx * blk + half - halfW) * scale * invW;"
          , "      double cr1 ="
          , "          centerRe + ((double)(bx + 1) * blk + half - halfW) * scale * invW;"
          , "      double cr2 ="
          , "          centerRe + ((double)(bx + 2) * blk + half - halfW) * scale * invW;"
          , "      double cr3 ="
          , "          centerRe + ((double)(bx + 3) * blk + half - halfW) * scale * invW;"
          , "      jshark_mandel_quad(cr0, cr1, cr2, cr3, ci, &row[bx]);"
          , "    }"
          , "#endif"
          , "    for (; bx < nx; bx++) {"
          , "      double cr = centerRe + ((double)bx * blk + half - halfW) * scale * invW;"
          , "      row[bx] = (int32_t)jshark_mandel_iter(cr, ci);"
          , "    }"
          , "  }"
          , "  return (int32_t)(uintptr_t)jshark_grid_buf;"
          , "}"
          , ""
          , "/* Same grid contract, but computed by HVM2 reducing the Bend-compiled"
          , " * jshark_grid def: interaction-net execution end to end (f24 math). */"
          , "__attribute__((export_name(\""
              <> bendId
              <> "_hvm2_grid\")))"
          , "int32_t jshark_export_"
              <> cSym
              <> "_hvm2_grid(double centerRe, double centerIm, double scale,"
          , "    double w, double h, double blk, double bxN, double byN) {"
          , "  int nx = (int)bxN;"
          , "  int ny = (int)byN;"
          , "  if (nx <= 0 || ny <= 0 || nx * ny > JSHARK_GRID_CAP) {"
          , "    jshark_hvm2_last_k = -4;"
          , "    return 0;"
          , "  }"
          , "  if (!jshark_hvm2_boot()) {"
          , "    if (jshark_hvm2_last_k == 0) { jshark_hvm2_last_k = -1; }"
          , "    return 0;"
          , "  }"
          , "  u32 fid = jshark_hvm2_def_id(\"jshark_grid\");"
          , "  if (fid == 0xFFFFFFFF) {"
          , "    jshark_hvm2_last_k = -2;"
          , "    return 0;"
          , "  }"
          , "  int total = nx * ny;"
          , "  if (total <= JSHARK_HVM2_TILE_W * JSHARK_HVM2_TILE_H) {"
          , "    double args[8] = {centerRe, centerIm, scale, w, h, blk, bxN, byN};"
          , "    int k = jshark_hvm2_run_grid(fid, args, total);"
          , "    jshark_hvm2_last_k = k;"
          , "    if (k != total) { return 0; }"
          , "    return (int32_t)(uintptr_t)jshark_grid_buf;"
          , "  }"
          , "  for (int ty0 = 0; ty0 < ny; ty0 += JSHARK_HVM2_TILE_H) {"
          , "    int th = ny - ty0;"
          , "    if (th > JSHARK_HVM2_TILE_H) { th = JSHARK_HVM2_TILE_H; }"
          , "    for (int tx0 = 0; tx0 < nx; tx0 += JSHARK_HVM2_TILE_W) {"
          , "      int tw = nx - tx0;"
          , "      if (tw > JSHARK_HVM2_TILE_W) { tw = JSHARK_HVM2_TILE_W; }"
          , "      int cells = tw * th;"
          , "      double tileW = (double)tw * blk;"
          , "      double tileH = (double)th * blk;"
          , "      double tileCenterRe = centerRe"
          , "          + ((((double)tx0 * blk) + (tileW * 0.5)) - (w * 0.5)) * scale / w;"
          , "      double tileCenterIm = centerIm"
          , "          + ((((double)ty0 * blk) + (tileH * 0.5)) - (h * 0.5)) * scale / h;"
          , "      double targs[8] = {"
          , "          tileCenterRe, tileCenterIm, scale, tileW, tileH, blk,"
          , "          (double)tw, (double)th};"
          , "      int k = jshark_hvm2_run_grid(fid, targs, cells);"
          , "      if (k != cells) {"
          , "        jshark_hvm2_last_k = k;"
          , "        return 0;"
          , "      }"
          , "      if (!jshark_hvm2_blit_tile(nx, ny, tx0, ty0, tw, th)) {"
          , "        jshark_hvm2_last_k = -3;"
          , "        return 0;"
          , "      }"
          , "    }"
          , "  }"
          , "  jshark_hvm2_last_k = total;"
          , "  return (int32_t)(uintptr_t)jshark_grid_buf;"
          , "}"
          , ""
          , "__attribute__((export_name(\"jshark_hvm2_last_k\")))"
          , "int32_t jshark_export_hvm2_last_k(void) {"
          , "  return (int32_t)jshark_hvm2_last_k;"
          , "}"
          , ""
          ]
