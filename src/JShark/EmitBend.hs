{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Emit Bend source for the HVM2 pipeline (Bend → HVM2 → C → WASM).
-- Bend is the human-readable frontend; HVM2 is the interaction-net IR.
module JShark.EmitBend
  ( BendType (..)
  , Hvm2Error (..)
  , bendDefNames
  , emitBendKernel
  , emitBendModule
  , emitBendModuleFromDefs
  , emitIrExpr
  , emitKernelExportsC
  , bendDefExports
  , peelLambdas
  )
where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as T
import JShark.Ir
  ( IrExpr (..)
  , IrKernel (..)
  , irMetaPure
  , metaIrExpr
  )
import JShark.Types (Universe (Bool), Value (..))
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

emitBendModuleFromDefs :: [Text] -> Either Hvm2Error Text
emitBendModuleFromDefs defs =
  pure $
    T.unlines (defs <> ["", "def main():", "  return 0"])
      <> "\n"

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
      ( ("def "
          <> sanitizeBendId name
          <> paramsLine paramNames paramTypes
          <> " -> "
          <> retTy
          <> ":")
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

emitIfReturn ::
  IntMap Text -> IrExpr 'Bool -> IrExpr u -> IrExpr u -> Either Hvm2Error [Text]
emitIfReturn env c t e = do
  cTxt <- emitIrExpr env c
  tTxt <- emitIrExpr env t
  eTxt <- emitIrExpr env e
  pure
    [ "  if " <> cTxt <> ":"
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
  fnBodyLines <- emitBody envFn fnBody
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
    IrApply f x -> do
      fTxt <- emitIrExpr env f
      xTxt <- emitIrExpr env x
      pure (fTxt <> "(" <> xTxt <> ")")
    IrIf c t eF -> do
      cTxt <- emitIrExpr env c
      tTxt <- emitIrExpr env t
      eTxt <- emitIrExpr env eF
      pure ("(" <> tTxt <> " if " <> cTxt <> " else " <> eTxt <> ")")
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
  KAnd x y -> binop env "and" x y
  KOr x y -> binop env "or" x y
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
  ValueNumber d
    | d == fromIntegral (truncate d :: Integer) ->
        pure (T.pack (show (truncate d :: Integer)))
    | otherwise ->
        pure (T.pack (show d))
  ValueBool b -> pure (if b then "True" else "False")
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
  IrLambda tag body ->
    let (tags, inner) = peelLambdasFn' (unsafeCoerce body)
     in (tag : tags, inner)
  e -> ([], e)

peelLambdas :: IrExpr u -> ([Int], IrExpr u)
peelLambdas ir =
  case ir of
    IrLambda tag body ->
      let (rest, inner) = peelLambdasFn' body
       in (tag : rest, unsafeCoerce inner)
    _ -> ([], ir)

paramName :: Int -> Int -> Text
paramName tag _ = "a" <> T.pack (show (abs tag))

inferParamTypes :: [Int] -> IrExpr u -> [BendType]
inferParamTypes tags body = map (`inferParamType` body) tags

inferParamType :: Int -> IrExpr u -> BendType
inferParamType tag body
  | varInBoolCtx tag body = BendBool
  | varInFloatCtx tag body = BendF24
  | otherwise = BendU24

varInBoolCtx :: Int -> IrExpr u -> Bool
varInBoolCtx tag = \case
  IrVar i -> i == tag
  IrKernelK (KAnd x y) -> mentions tag x || mentions tag y
  IrKernelK (KOr x y) -> mentions tag x || mentions tag y
  IrKernelK (KEq _ x y) -> mentions tag x || mentions tag y
  IrKernelK (KNEq _ x y) -> mentions tag x || mentions tag y
  IrKernelK (KGTh x y) -> mentions tag x || mentions tag y
  IrKernelK (KLTh x y) -> mentions tag x || mentions tag y
  IrKernelK (KGTEq x y) -> mentions tag x || mentions tag y
  IrKernelK (KLTEq x y) -> mentions tag x || mentions tag y
  IrIf c t eF -> varInBoolCtx tag c || varInBoolCtx tag t || varInBoolCtx tag eF
  IrApply f x -> varInBoolCtx tag f || varInBoolCtx tag x
  IrLet _ x g -> varInBoolCtx tag x || varInBoolCtx tag g
  _ -> False

varInFloatCtx :: Int -> IrExpr u -> Bool
varInFloatCtx tag = \case
  IrVar i -> i == tag
  IrKernelK (KFracDiv x y) ->
    mentions tag x
      || mentions tag y
      || isFloatLiteral x
      || isFloatLiteral y
  IrIf c t eF -> varInFloatCtx tag c || varInFloatCtx tag t || varInFloatCtx tag eF
  IrApply f x -> varInFloatCtx tag f || varInFloatCtx tag x
  IrLet _ x g -> varInFloatCtx tag x || varInFloatCtx tag g
  _ -> False

mentions :: Int -> IrExpr v -> Bool
mentions tag = \case
  IrVar i -> i == tag
  IrApply f x -> mentions tag f || mentions tag x
  IrIf c t eF -> mentions tag c || mentions tag t || mentions tag eF
  IrLet _ x g -> mentions tag x || mentions tag g
  IrKernelK k -> mentionsInKernel tag k
  _ -> False
 where
  mentionsInKernel t = \case
    KPlus x y -> mentions t x || mentions t y
    KMinus x y -> mentions t x || mentions t y
    KTimes x y -> mentions t x || mentions t y
    KFracDiv x y -> mentions t x || mentions t y
    KRem x y -> mentions t x || mentions t y
    KNegate x -> mentions t x
    KAnd x y -> mentions t x || mentions t y
    KOr x y -> mentions t x || mentions t y
    KEq _ x y -> mentions t x || mentions t y
    KNEq _ x y -> mentions t x || mentions t y
    KGTh x y -> mentions t x || mentions t y
    KLTh x y -> mentions t x || mentions t y
    KGTEq x y -> mentions t x || mentions t y
    KLTEq x y -> mentions t x || mentions t y
    _ -> False

isFloatLiteral :: IrExpr u -> Bool
isFloatLiteral = \case
  IrLiteral (ValueNumber d) -> d /= fromIntegral (truncate d :: Integer)
  _ -> False

inferType :: IrExpr u -> BendType
inferType e =
  case e of
    IrLiteral (ValueBool _) -> BendBool
    IrLiteral (ValueNumber d)
      | d == fromIntegral (truncate d :: Integer) -> BendU24
      | otherwise -> BendF24
    IrKernelK (KAnd _ _) -> BendBool
    IrKernelK (KOr _ _) -> BendBool
    IrKernelK (KEq _ _ _) -> BendBool
    IrKernelK (KNEq _ _ _) -> BendBool
    IrKernelK (KGTh _ _) -> BendBool
    IrKernelK (KLTh _ _) -> BendBool
    IrKernelK (KGTEq _ _) -> BendBool
    IrKernelK (KLTEq _ _) -> BendBool
    _ -> BendU24

bendTypeName :: BendType -> Text
bendTypeName = \case
  BendU24 -> "u24"
  BendI24 -> "i24"
  BendF24 -> "f24"
  BendBool -> "Bool"

sanitizeBendId :: Text -> Text
sanitizeBendId t =
  let base = T.map (\c -> if c `elem` (['_', '-'] :: [Char]) then '_' else c) t
   in if T.null base || not (isAlpha (T.head base))
        then "k_" <> base
        else base
 where
  isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

cIdent :: Text -> Text
cIdent t =
  let id_ = sanitizeBendId t
   in if id_ `elem` cKeywords
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
emitKernelExportsC exports =
  T.unlines $
    [ "/* Auto-generated WASM export shims for JShark HVM2 kernels. */"
    , "#include <stdint.h>"
    , ""
    , "typedef int64_t jshark_hvm2_i64;"
    , ""
    ]
      <> concatMap (uncurry exportDef) exports
      <> [""]
 where
  exportDef name arity =
    let
      bendId = sanitizeBendId name
      cSym = cIdent name
      exportName = bendId
      args = map (\i -> "a" <> T.pack (show i)) [0 .. arity - 1]
      argList = T.intercalate ", " (map (\a -> "jshark_hvm2_i64 " <> a) args)
      callArgs = T.intercalate ", " args
      externSig =
        if null args
          then cSym <> "()"
          else
            cSym
              <> "("
              <> T.intercalate ", " (replicate arity "jshark_hvm2_i64")
              <> ")"
    in
      [ "__attribute__((export_name(\"" <> exportName <> "\")))"
      , "jshark_hvm2_i64 jshark_hvm2_export_" <> cSym <> "(" <> argList <> ") {"
      , "  extern jshark_hvm2_i64 " <> externSig <> ";"
      , "  return " <> cSym <> "(" <> callArgs <> ");"
      , "}"
      , ""
      ]

guardPure :: IrExpr u -> Either Hvm2Error ()
guardPure ir =
  if irMetaPure (metaIrExpr ir)
    then Right ()
    else Left Hvm2ImpureKernel
