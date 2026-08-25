{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module JShark
  ( Expr
      ( Literal
      , Concat
      , Plus
      , Times
      , Minus
      , Negate
      , FracDiv
      , Rem
      , BitAnd
      , BitOr
      , BitXor
      , Shl
      , Shr
      , UShr
      , And
      , Or
      , Eq
      , NEq
      , GTh
      , LTh
      , GTEq
      , LTEq
      , Let
      , LetRec
      , Lambda
      , Apply
      , Show
      , TypeOf
      , Var
      , If
      , OptionCase
      , ResultOk
      , ResultErr
      , ResultCase
      , Index
      , U8Index
      , Error
      , Std
      , FnLit
      , UnsafeNullable
      , FrozenLit
      , GetField
      , Hvm2Kernel
      )
  , FnBody (..)
  , Value (..)
  , GroupBy
  , Arg (..)
  , Hvm2KernelEntry (..)
  , ClosedExpr
  , ClosedEffect
  , Effect
    ( Lift
    , FFI
    , UnsafeObject
    , UnsafeObjectGet
    , UnsafeObjectAssign
    , CallMethod
    , Bind
    , ThenE
    , BindRec
    , LambdaE
    , ApplyE
    , IfE
    , While
    , ForRange
    , U8Set
    , U8Fill
    , OptionCaseE
    , ResultCaseE
    , StringCaseE
    , Throw
    , Try
    , ObjectLit
    , DeleteProp
    , ArrayLit
    )
  -- Evaluation
  , evaluate
  , evaluateNumber
  , evaluateBigInt
  , evaluateCached
  -- Optimization
  , optimize
  , optimizeEffect
  , optimizeEffectIr
  , nodeCountExpr
  , nodeCountEff
  , closedEffectNodes
  , optIrLargeThreshold
  , optimizedExprSize
  , optimizedEffectSize
  -- Codegen
  , pureAST
  , effectfulAST
  , effectfulASTFromFlat
  , effectfulASTIr
  , irEffectFromClosed
  , irExprFromClosed
  , irOptimizedEffectFromClosed
  , irOptimizedExprFromClosed
  , collectHvm2Kernels
  , pureProgram
  , effectfulProgram
  , printComputation
  , renderJS
  , renderJSCompact
  , escapeJsString
  , structuralEq
  , structuralNEq
  )
where

-- Indexed PHOAS (binders @f u@, closed terms @forall f@) in the style of
-- Chlipala / Kmett's parametric HOAS, with host-language sharing as in
-- https://www.reddit.com/r/haskell/comments/85een6/sharing_from_phoas_multiple_interpreters_from_free/dvxhlba
--
-- 'Expr' is the pure tree; 'Effect' is the impure tree. They join at FFI
-- through 'Arg', not by treating effects as expressions.

import Control.Monad (foldM)
import Data.Array.Byte (ByteArray (..))
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Char (digitToInt, isSpace)
import qualified Data.Char as Char
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Int (Int32)
import qualified Data.IntMap.Strict as IM
import Data.List (mapAccumL)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Monoid (All (..), Any (..), Sum (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable (Typeable, eqT, type (:~:) (..))
import Data.Word (Word32)
import GHC.Exts (Int (..), indexWord8Array#, sizeofByteArray#)
import GHC.TypeLits (KnownSymbol, sameSymbol, symbolVal)
import GHC.Word (Word8 (..))
import qualified Data.Vector as V
import qualified JShark.Flat as Flat
import qualified JShark.FlatSoA as FlatSoA
import qualified JShark.Ir as Ir
import JShark.Prim
  ( MathBinary (..)
  , MathUnary (..)
  , isPureFixed
  , matchMathBinary
  , matchMathUnary
  )
import qualified JShark.Prim as Prim
import JShark.Rec
import JShark.Types
import JShark.Emit
  ( JS
  , ($$)
  , (<+>)
  , braces
  , blockBody
  , brackets
  , colon
  , dquotes
  , hcat
  , jsDecimal
  , jsDouble
  , jsString
  , jsText
  , iifeBody
  , nonEmpty
  , parens
  , punctuate
  , renderJS
  , renderJSCompact
  , semi
  , vcat
  , vcatNonEmpty
  )
import Numeric (readInt, showHex)
import Unsafe.Coerce (unsafeCoerce)

unNumber :: Value 'Number -> Double
unNumber (ValueNumber d) = d

unBigInt :: Value 'BigInt -> Integer
unBigInt (ValueBigInt n) = n

unBool :: Value 'Bool -> Bool
unBool (ValueBool b) = b

unString :: Value 'String -> Text
unString (ValueString s) = s

unFunction :: Value ('Function u v) -> Value u -> Value v
unFunction (ValueFunction f) = f

valueEq :: Value u -> Value u -> Bool
valueEq (ValueNumber a) (ValueNumber b) = a == b
valueEq (ValueBigInt a) (ValueBigInt b) = a == b
valueEq (ValueString a) (ValueString b) = a == b
valueEq (ValueBool a) (ValueBool b) = a == b
valueEq ValueUnit ValueUnit = True
valueEq (ValueArray as) (ValueArray bs) =
  length as == length bs && and (zipWith valueEq as bs)
valueEq (ValueOption a) (ValueOption b) = case (a, b) of
  (Nothing, Nothing) -> True
  (Just x, Just y) -> valueEq x y
  _ -> False
valueEq (ValueResult a) (ValueResult b) = case (a, b) of
  (Left x, Left y) -> valueEq x y
  (Right x, Right y) -> valueEq x y
  _ -> False
valueEq (ValueRegex a) (ValueRegex b) = a == b
valueEq (ValueUint8Array a) (ValueUint8Array b) = a == b
valueEq (ValueFrozen as) (ValueFrozen bs) = frozenEq as bs
valueEq (ValueFunction _) (ValueFunction _) =
  error "evaluate: functions cannot be compared for equality"

-- | Last-wins records. JS @===@ is identity; we keep value equality
-- because a frozen object is a Good Parts record, not a mutable handle.
frozenEq :: [FieldLit Value r] -> [FieldLit Value r] -> Bool
frozenEq as bs =
  let
    as' = lastWinsFields as
    bs' = lastWinsFields bs
   in
    length as' == length bs' && all (\fa -> any (fieldLitEq fa) bs') as'

lastWinsFields :: [FieldLit Value r] -> [FieldLit Value r]
lastWinsFields = reverse . keep [] . reverse
 where
  keep acc [] = acc
  keep acc (f : fs)
    | fieldKey f `elem` map fieldKey acc = keep acc fs
    | otherwise = keep (f : acc) fs

evalFieldLit ::
  Monad m =>
  (forall w. Expr Value w -> m (Value w))
  -> FieldLit Value r
  -> m (FieldLit Value r)
evalFieldLit rec (FieldLit @k e) = FieldLit @k . Literal <$> rec e
evalFieldLit rec (FieldLitEffect @k (Lift e)) = FieldLit @k . Literal <$> rec e
evalFieldLit rec (FieldLitExtra @k e) = FieldLitExtra @k . Literal <$> rec e
evalFieldLit rec (FieldLitExtraEffect @k (Lift e)) = FieldLitExtra @k . Literal <$> rec e
evalFieldLit _ (FieldLitEffect _) =
  error "evaluate: effectful object field (FieldLitEffect); not a pure Lift"
evalFieldLit _ (FieldLitExtraEffect _) =
  error "evaluate: effectful object field (FieldLitExtraEffect); not a pure Lift"

fieldLitEq :: forall r. FieldLit Value r -> FieldLit Value r -> Bool
fieldLitEq (FieldLit @k a) (FieldLit @k' b) = forcedFieldEq @k @k' @r a b
fieldLitEq (FieldLitEffect @k (Lift a)) (FieldLitEffect @k' (Lift b)) =
  forcedFieldEq @k @k' @r a b
fieldLitEq (FieldLitExtra @k a) (FieldLitExtra @k' b) = extraFieldEq @k @k' a b
fieldLitEq (FieldLitExtraEffect @k (Lift a)) (FieldLitExtraEffect @k' (Lift b)) =
  extraFieldEq @k @k' a b
fieldLitEq _ _ = False

forcedFieldEq ::
  forall k k' r.
  (KnownSymbol k, KnownSymbol k') =>
  Expr Value (Field r k) -> Expr Value (Field r k') -> Bool
forcedFieldEq a b =
  case sameSymbol (Proxy @k) (Proxy @k') of
    Nothing -> False
    Just Refl -> case (a, b) of
      (Literal x, Literal y) -> valueEq x y
      _ -> error "evaluate: frozen field was not forced"

extraFieldEq ::
  forall k k' u v.
  (KnownSymbol k, KnownSymbol k', Typeable u, Typeable v) =>
  Expr Value u -> Expr Value v -> Bool
extraFieldEq a b =
  case sameSymbol (Proxy @k) (Proxy @k') of
    Nothing -> False
    Just Refl -> case eqT @u @v of
      Nothing -> False
      Just Refl -> case (a, b) of
        (Literal x, Literal y) -> valueEq x y
        _ -> error "evaluate: frozen extra field was not forced"

-- | Only numbers, bigints, strings, and booleans support ordering comparisons.
valueCompare :: Value u -> Value u -> Ordering
valueCompare (ValueNumber a) (ValueNumber b) = compare a b
valueCompare (ValueBigInt a) (ValueBigInt b) = compare a b
valueCompare (ValueString a) (ValueString b) = compare a b
valueCompare (ValueBool a) (ValueBool b) = compare a b
valueCompare _ _ =
  error
    "evaluate: only numbers, bigints, strings, and booleans support ordering comparisons"

-- | Mimics JS's @String(x)@ coercion closely enough for the reference interpreter.
jsShow :: Value u -> Text
jsShow (ValueNumber d) = T.pack (jsShowNumber d)
jsShow (ValueBigInt n) = T.pack (show n)
jsShow (ValueString s) = s
jsShow (ValueBool True) = "true"
jsShow (ValueBool False) = "false"
jsShow ValueUnit = "undefined"
jsShow (ValueArray xs) = T.intercalate "," (map jsJoinElem xs)
jsShow (ValueOption Nothing) = "null"
jsShow (ValueOption (Just x)) = jsShow x
jsShow ValueResult {} = "[object Object]"
jsShow (ValueRegex s) = s
jsShow (ValueUint8Array ba) = jsShowUint8Array ba
jsShow ValueFrozen {} = "[object Object]"
jsShow (ValueFunction _) = error "evaluate: cannot show a function"

-- | One element of @Array.prototype.join@ (and of a nested array's
-- @toString@). JS renders @null@ and @undefined@ as the empty string
-- there, not as @\"null\"@ / @\"undefined\"@.
jsJoinElem :: Value u -> Text
jsJoinElem = \case
  ValueOption Nothing -> ""
  ValueOption (Just v) -> jsJoinElem v
  ValueUnit -> ""
  v -> jsShow v

-- | JS @typeof@. @null@ is @\"object\"@.
typeOfValue :: Value u -> Text
typeOfValue = \case
  ValueNumber {} -> "number"
  ValueBigInt {} -> "bigint"
  ValueString {} -> "string"
  ValueBool {} -> "boolean"
  ValueUnit -> "undefined"
  ValueFunction {} -> "function"
  ValueArray {} -> "object"
  ValueOption Nothing -> "object"
  ValueOption (Just v) -> typeOfValue v
  ValueResult {} -> "object"
  ValueRegex {} -> "object"
  ValueUint8Array {} -> "object"
  ValueFrozen {} -> "object"

jsShowNumber :: Double -> String
jsShowNumber d
  | isInt = show (truncate d :: Integer)
  | otherwise = show d
 where
  isInt = not (isNaN d) && not (isInfinite d) && d == fromInteger (truncate d)

isFiniteDouble :: Double -> Bool
isFiniteDouble d = not (isNaN d) && not (isInfinite d)

-- | JS @Math.floor@ / @ceil@ / @round@ / @trunc@: non-finite inputs are
-- the identity. Haskell's 'Integral' conversion is undefined on NaN /
-- infinity, and would throw from 'evaluate' of @Array.index@ on NaN.
jsToIntegral :: (Double -> Integer) -> Double -> Double
jsToIntegral f d
  | isFiniteDouble d = fromIntegral (f d)
  | otherwise = d

cannotEval :: String -> a
cannotEval what = error ("evaluate: cannot evaluate " ++ what)

arrayValues :: Value ('Array u) -> [Value u]
arrayValues (ValueArray vs) = vs

isOrderableValue :: Value u -> Bool
isOrderableValue = \case
  ValueNumber {} -> True
  ValueBigInt {} -> True
  ValueString {} -> True
  ValueBool {} -> True
  _ -> False

eqFoldableValue :: Value u -> Bool
eqFoldableValue ValueFunction {} = False
eqFoldableValue _ = True

-- | JS ToInt32 / ToUint32 for bitwise ops and @>>>@.
toInt32 :: Double -> Int32
toInt32 d
  | isNaN d || isInfinite d = 0
  | otherwise = fromInteger (truncate d)

toUint32 :: Double -> Word32
toUint32 d
  | isNaN d || isInfinite d = 0
  | otherwise = fromInteger (truncate d)

jsBit2 :: (Int32 -> Int32 -> Int32) -> Double -> Double -> Double
jsBit2 f a b = fromIntegral (f (toInt32 a) (toInt32 b))

jsShl, jsShr, jsUShr :: Double -> Double -> Double
jsShl a b = fromIntegral (shiftL (toInt32 a) (fromIntegral (toUint32 b .&. 31)))
jsShr a b = fromIntegral (shiftR (toInt32 a) (fromIntegral (toUint32 b .&. 31)))
jsUShr a b = fromIntegral (shiftR (toUint32 a) (fromIntegral (toUint32 b .&. 31)))

-- | JS @%@ : remainder after truncating division, not Haskell @mod@.
jsRem :: Double -> Double -> Double
jsRem a b
  | isNaN a || isNaN b || isInfinite a || b == 0 = 0 / 0
  | isInfinite b = a
  | otherwise = a - b * fromInteger (truncate (a / b))

jsParseInt :: Text -> Int -> Double
jsParseInt s r
  | r < 2 || r > 36 = 0 / 0
  | otherwise =
      let
        t0 = dropWhile isSpace (T.unpack s)
        (neg, t1) = case t0 of
          '-' : xs -> (True, xs)
          '+' : xs -> (False, xs)
          xs -> (False, xs)
       in
        case readInt (fromIntegral r :: Integer) okDigit digitToInt t1 of
          (n, _) : _ -> fromInteger (if neg then negate n else n)
          [] -> 0 / 0
 where
  okDigit c =
    let
      v
        | c >= '0' && c <= '9' = Char.ord c - Char.ord '0'
        | c >= 'a' && c <= 'z' = Char.ord c - Char.ord 'a' + 10
        | c >= 'A' && c <= 'Z' = Char.ord c - Char.ord 'A' + 10
        | otherwise = 99
     in
      v < r

numberToBigInt :: Double -> Integer
numberToBigInt d
  | isFiniteDouble d && d == fromInteger n = n
  | otherwise =
      error
        "evaluate: Number cannot be converted to BigInt because it is not an integer"
 where
  n = truncate d

parseBigIntText :: Text -> Integer
parseBigIntText s =
  case parseBigIntString (T.unpack s) of
    Just n -> n
    Nothing -> error "evaluate: invalid BigInt string"

parseBigIntString :: String -> Maybe Integer
parseBigIntString raw =
  let
    stripped = reverse (dropWhile isSpace (reverse (dropWhile isSpace raw)))
    (neg, rest0) = case stripped of
      '-' : xs -> (True, xs)
      '+' : xs -> (False, xs)
      xs -> (False, xs)
    (base, digits) = case rest0 of
      '0' : 'x' : xs -> (16, xs)
      '0' : 'X' : xs -> (16, xs)
      '0' : 'b' : xs -> (2, xs)
      '0' : 'B' : xs -> (2, xs)
      '0' : 'o' : xs -> (8, xs)
      '0' : 'O' : xs -> (8, xs)
      xs -> (10, xs)
   in
    case digits of
      [] -> Nothing
      _ ->
        case readInt (fromIntegral base :: Integer) (okBigDigit base) digitToInt digits of
          (n, []) : _ -> Just (if neg then negate n else n)
          _ -> Nothing

okBigDigit :: Int -> Char -> Bool
okBigDigit base c =
  let
    v
      | c >= '0' && c <= '9' = Char.ord c - Char.ord '0'
      | c >= 'a' && c <= 'z' = Char.ord c - Char.ord 'a' + 10
      | c >= 'A' && c <= 'Z' = Char.ord c - Char.ord 'A' + 10
      | otherwise = 99
   in
    v < base

evalBigBin :: BigBinOp -> Integer -> Integer -> Integer
evalBigBin BPlus = (+)
evalBigBin BMinus = (-)
evalBigBin BTimes = (*)
evalBigBin BQuot = quot
evalBigBin BRem = rem
evalBigBin BBitAnd = (.&.)
evalBigBin BBitOr = (.|.)
evalBigBin BBitXor = xor
evalBigBin BShl = bigShl
evalBigBin BShr = bigShr

tryEvalBigBin :: BigBinOp -> Integer -> Integer -> Maybe Integer
tryEvalBigBin BQuot _ 0 = Nothing
tryEvalBigBin BRem _ 0 = Nothing
tryEvalBigBin BShl _ b | b < 0 = Nothing
tryEvalBigBin BShr _ b | b < 0 = Nothing
tryEvalBigBin op a b = Just (evalBigBin op a b)

bigShl :: Integer -> Integer -> Integer
bigShl a b
  | b < 0 = error "evaluate: BigInt shift count is negative"
  | otherwise = shiftL a (fromInteger b)

bigShr :: Integer -> Integer -> Integer
bigShr a b
  | b < 0 = error "evaluate: BigInt shift count is negative"
  | otherwise = shiftR a (fromInteger b)

jsBigIntLit :: Integer -> JS
jsBigIntLit n
  | n >= 0 = jsString (shows n "n")
  | otherwise = parens (jsString (shows n "n"))

bigOpJS :: BigBinOp -> Text
bigOpJS = \case
  BPlus -> "+"
  BMinus -> "-"
  BTimes -> "*"
  BQuot -> "/"
  BRem -> "%"
  BBitAnd -> "&"
  BBitOr -> "|"
  BBitXor -> "^"
  BShl -> "<<"
  BShr -> ">>"

-- | JS @Array.prototype.slice@: ToInteger, negatives from the end, clamp.
jsArraySlice :: [a] -> Double -> Double -> [a]
jsArraySlice vs start end =
  let
    len = length vs
    k = jsSliceClamp len start
    final = jsSliceClamp len end
   in
    take (max 0 (final - k)) (drop k vs)

jsSliceClamp :: Int -> Double -> Int
jsSliceClamp len x
  | isNaN x = 0
  | isInfinite x && x < 0 = 0
  | isInfinite x = len
  | otherwise =
      let
        n = truncate x :: Int
       in
        if n < 0 then max 0 (len + n) else min n len

jsQuote :: Text -> JS
jsQuote s = dquotes (jsString (escapeJsString (T.unpack s)))

escapeJsString :: String -> String
escapeJsString = concatMap esc
 where
  esc '\\' = "\\\\"
  esc '"' = "\\\""
  esc '\n' = "\\n"
  esc '\r' = "\\r"
  esc '\t' = "\\t"
  esc c
    | Char.ord c < 32 =
        let
          h = showHex (Char.ord c) ""
         in
          "\\u" ++ replicate (4 - length h) '0' ++ h
    | otherwise = [c]

uint8Elems :: ByteArray -> [Word8]
uint8Elems (ByteArray ba#) =
  [W8# (indexWord8Array# ba# i#) | I# i# <- [0 .. I# (sizeofByteArray# ba#) - 1]]

-- | JS @String(uint8arr)@ is @Array.prototype.toString@: comma-joined bytes.
jsShowUint8Array :: ByteArray -> Text
jsShowUint8Array = T.intercalate "," . map (T.pack . show) . uint8Elems

jsUint8ArrayLit :: ByteArray -> JS
jsUint8ArrayLit ba =
  let
    elems = uint8Elems ba
    n = length elems
   in
    if all (== 0) elems
      then "new Uint8Array" <> parens (jsDecimal n)
      else
        "new Uint8Array"
          <> parens
            ( brackets
                ( hcat
                    (punctuate ", " (map (jsDecimal . (fromIntegral :: Word8 -> Int)) elems))
                )
            )

-- | Optimizer / codegen name. 'Stamp' is an untyped tag for use-counting.
-- 'Embed' / 'EmbedEff' are typed hole fillers for bind inlining.
data Stamp (u :: Universe) where
  Stamp :: Int -> Stamp u
  Embed :: Expr Stamp u -> Stamp u
  EmbedEff :: Effect Stamp u -> Stamp u

-- | Codegen / dummy binder. Same as 'Stamp'; kept so call sites that
-- only need a name stay readable.
pattern Name :: Int -> Stamp u
pattern Name i = Stamp i

{-# COMPLETE Stamp, Embed, EmbedEff #-}

stampId :: Stamp u -> Int
stampId (Stamp i) = i
stampId (Embed _) = error "JShark.stampId: Embed (flatten first)"
stampId (EmbedEff _) = error "JShark.stampId: EmbedEff (flatten first)"

peelResult ::
  Expr Stamp ('Result e a) -> Maybe (Either (Expr Stamp e) (Expr Stamp a))
peelResult = \case
  Literal (ValueResult (Left v)) -> Just (Left (Literal v))
  Literal (ValueResult (Right v)) -> Just (Right (Literal v))
  ResultOk x -> Just (Right x)
  ResultErr x -> Just (Left x)
  _ -> Nothing

-- | Tag equality is binder identity; each tag is allocated at one @u@.
peelOption :: Expr Stamp ('Option u) -> Maybe (Maybe (Expr Stamp u))
peelOption = \case
  Literal (ValueOption Nothing) -> Just Nothing
  Literal (ValueOption (Just v)) -> Just (Just (Literal v))
  -- Host literals are never JS null. FFI / vars stay unpeeled so
  -- 'Storage.getItem' keeps its @=== null@ check.
  UnsafeNullable (Literal v) -> Just (Just (Literal v))
  _ -> Nothing

peelBoolEffect :: Effect Stamp 'Bool -> Maybe Bool
peelBoolEffect (Lift (Literal (ValueBool b))) = Just b
peelBoolEffect _ = Nothing

peelString :: Expr Stamp 'String -> Maybe Text
peelString (Literal (ValueString s)) = Just s
peelString _ = Nothing

evaluateNumber :: ClosedExpr 'Number -> Double
evaluateNumber e = unNumber (evaluate e)

evaluateBigInt :: ClosedExpr 'BigInt -> Integer
evaluateBigInt e = unBigInt (evaluate e)

-- | Pure reference interpreter. Shared Haskell heap nodes are walked once
-- per occurrence (no memo table).
evaluate :: ClosedExpr u -> Value u
evaluate = evalValue

evalValue :: Expr Value v -> Value v
evalValue = runIdentity . evalAlg (Identity . evalValue) (\g v -> evalValue (g v))

evalAlg ::
  Monad m =>
  (forall w. Expr Value w -> m (Value w))
  -> (forall a b. (Value a -> Expr Value b) -> Value a -> Value b)
  -> Expr Value v
  -> m (Value v)
evalAlg rec apply = \case
  Literal v -> pure v
  Var x -> pure x
  Apply g x -> unFunction <$> rec g <*> rec x
  Lambda g -> pure (ValueFunction (apply g))
  Let x g -> rec x >>= rec . g
  LetRec r b ->
    let
      recV = apply r recV
     in
      rec (b recV)
  If c t e -> do
    cv <- rec c
    if unBool cv then rec t else rec e
  OptionCase opt none' someF -> do
    ov <- rec opt
    case ov of
      ValueOption Nothing -> rec none'
      ValueOption (Just x) -> rec (someF x)
  ResultOk x -> ValueResult . Right <$> rec x
  ResultErr x -> ValueResult . Left <$> rec x
  ResultCase res errF okF -> do
    rv <- rec res
    case rv of
      ValueResult (Left e) -> rec (errF e)
      ValueResult (Right a) -> rec (okF a)
  FnLit {} -> cannotEval "Fn (fn)"
  Index xs i -> do
    iv <- rec i
    evalAsArray rec xs $ \vs ->
      let
        d = unNumber iv
        idx = truncate d :: Int
       in
        if isFiniteDouble d && idx >= 0 && idx < length vs
          then pure (vs !! idx)
          else error "evaluate: array index out of bounds"
  U8Index buf i -> do
    iv <- rec i
    evalAsUint8Array rec buf $ \ba ->
      let
        d = unNumber iv
        idx = truncate d :: Int
        elems = uint8Elems ba
       in
        if isFiniteDouble d && idx >= 0 && idx < length elems
          then pure (ValueNumber (fromIntegral (elems !! idx)))
          else error "evaluate: uint8 index out of bounds"
  Error msg -> do
    m <- rec msg
    error ("evaluate: " ++ T.unpack (unString m))
  Std s -> evalStd rec s
  UnsafeNullable x -> ValueOption . Just <$> rec x
  FrozenLit fs -> ValueFrozen <$> traverse (evalFieldLit rec) fs
  GetField @k o -> do
    ov <- rec o
    withFrozenField @k ov rec
  Hvm2Kernel {} -> cannotEval "Hvm2Kernel (use WASM export)"

-- | Force an array 'Value' and continue. Every array node is a
-- 'ValueArray' constructor; the case is here so call sites stay linear.
evalAsArray ::
  Monad m =>
  (forall w. Expr Value w -> m (Value w))
  -> Expr Value ('Array u)
  -> ([Value u] -> m a)
  -> m a
evalAsArray rec xs k = do
  arr <- rec xs
  case arr of
    ValueArray vs -> k vs

evalAsUint8Array ::
  Monad m =>
  (forall w. Expr Value w -> m (Value w))
  -> Expr Value 'Uint8Array
  -> (ByteArray -> m a)
  -> m a
evalAsUint8Array rec buf k = do
  arr <- rec buf
  case arr of
    ValueUint8Array ba -> k ba

sortByM :: Monad m => (a -> a -> m Ordering) -> [a] -> m [a]
sortByM cmp xs = mergeSort cmp xs

mergeSort :: Monad m => (a -> a -> m Ordering) -> [a] -> m [a]
mergeSort _ [] = pure []
mergeSort _ [x] = pure [x]
mergeSort cmp xs = do
  let
    (l, r) = splitAt (Prelude.length xs `div` 2) xs
  ls <- mergeSort cmp l
  rs <- mergeSort cmp r
  mergeByM cmp ls rs

mergeByM :: Monad m => (a -> a -> m Ordering) -> [a] -> [a] -> m [a]
mergeByM _ [] ys = pure ys
mergeByM _ xs [] = pure xs
mergeByM cmp (x : xs) (y : ys) = do
  o <- cmp x y
  case o of
    GT -> (y :) <$> mergeByM cmp (x : xs) ys
    _ -> (x :) <$> mergeByM cmp xs (y : ys)

evalStd ::
  Monad m =>
  (forall w. Expr Value w -> m (Value w))
  -> Std Value u
  -> m (Value u)
evalStd rec = \case
  Fixed op args -> evalFixed rec op args
  Method m -> evalMethod rec m
  Kernel k -> evalKernel rec k

evalKernel ::
  Monad m =>
  (forall w. Expr Value w -> m (Value w))
  -> Kernel Value u
  -> m (Value u)
evalKernel rec = \case
  KPlus x y -> num2 (+) x y
  KTimes x y -> num2 (*) x y
  KMinus x y -> num2 (-) x y
  KNegate x -> num1 negate x
  KFracDiv x y -> num2 (/) x y
  KRem x y -> num2 jsRem x y
  KBitAnd x y -> num2 (jsBit2 (.&.)) x y
  KBitOr x y -> num2 (jsBit2 (.|.)) x y
  KBitXor x y -> num2 (jsBit2 xor) x y
  KShl x y -> num2 jsShl x y
  KShr x y -> num2 jsShr x y
  KUShr x y -> num2 jsUShr x y
  KBig op x y -> do
    a <- rec x
    b <- rec y
    pure (ValueBigInt (evalBigBin op (unBigInt a) (unBigInt b)))
  KBigNeg x -> ValueBigInt . negate . unBigInt <$> rec x
  KConcat x y -> do
    a <- rec x
    b <- rec y
    pure (ValueString (unString a <> unString b))
  KShow x -> ValueString . jsShow <$> rec x
  KTypeOf x -> ValueString . typeOfValue <$> rec x
  KAnd x y -> do
    a <- rec x
    if unBool a then rec y else pure (ValueBool False)
  KOr x y -> do
    a <- rec x
    if unBool a then pure (ValueBool True) else rec y
  KEq _ x y -> ValueBool <$> (valueEq <$> rec x <*> rec y)
  KNEq _ x y -> ValueBool . not <$> (valueEq <$> rec x <*> rec y)
  KGTh x y -> ValueBool . (== GT) <$> (valueCompare <$> rec x <*> rec y)
  KLTh x y -> ValueBool . (== LT) <$> (valueCompare <$> rec x <*> rec y)
  KGTEq x y -> ValueBool . (/= LT) <$> (valueCompare <$> rec x <*> rec y)
  KLTEq x y -> ValueBool . (/= GT) <$> (valueCompare <$> rec x <*> rec y)
 where
  num1 f x = ValueNumber . f . unNumber <$> rec x
  num2 f x y = ValueNumber <$> (f <$> (unNumber <$> rec x) <*> (unNumber <$> rec y))

evalMethod ::
  Monad m =>
  (forall w. Expr Value w -> m (Value w))
  -> Method Value u
  -> m (Value u)
evalMethod rec = \case
  MethMap xs f ->
    evalAsArray rec xs $ \vs -> ValueArray <$> traverse (rec . f) vs
  MethFilter xs f ->
    evalAsArray rec xs $ \vs -> do
      keep <-
        traverse
          ( \v -> do
              b <- rec (f v)
              pure (unBool b, v)
          )
          vs
      pure (ValueArray [v | (True, v) <- keep])
  MethReduce xs z f -> do
    z0 <- rec z
    evalAsArray rec xs $ foldM (\acc v -> rec (f acc v)) z0
  MethReduceRight xs z f -> do
    z0 <- rec z
    evalAsArray rec xs $ foldr (\v next -> next >>= \acc -> rec (f acc v)) (pure z0)
  MethToSorted xs f ->
    evalAsArray rec xs $ \vs ->
      ValueArray
        <$> sortByM (\a b -> do n <- unNumber <$> rec (f a b); pure (compare n 0)) vs
  MethFrom n f -> do
    nv <- rec n
    let
      d = unNumber nv
      len
        | isFiniteDouble d && d > 0 = truncate d :: Int
        | otherwise = 0
    ValueArray
      <$> traverse
        (\i -> rec (f (ValueNumber (fromIntegral i))))
        [0 .. len - 1]

evalFixed ::
  Monad m =>
  (forall w. Expr Value w -> m (Value w))
  -> FixedOp a b c u
  -> FixedArgs Value a b c
  -> m (Value u)
evalFixed rec op args = case (op, args) of
  (n, ArgsU x)
    | Just (MathUnary n') <- matchMathUnary n ->
        ValueNumber . Prim.mathUnaryFn n' . unNumber <$> rec x
  (n, ArgsB x y)
    | Just (MathBinary n') <- matchMathBinary n ->
        ValueNumber
          <$> (Prim.mathBinaryFn n' <$> (unNumber <$> rec x) <*> (unNumber <$> rec y))
  (FixArrLen, ArgsU xs) ->
    evalAsArray rec xs $ \vs ->
      pure (ValueNumber (fromIntegral (Prelude.length vs)))
  (FixU8Len, ArgsU buf) ->
    evalAsUint8Array rec buf $ \ba ->
      pure (ValueNumber (fromIntegral (Prelude.length (uint8Elems ba))))
  (FixParseInt, ArgsB s r) -> do
    sv <- rec s
    rv <- rec r
    pure (ValueNumber (jsParseInt (unString sv) (truncate (unNumber rv))))
  (FixToBigInt, ArgsU x) ->
    ValueBigInt . numberToBigInt . unNumber <$> rec x
  (FixFromBigInt, ArgsU x) ->
    ValueNumber . fromInteger . unBigInt <$> rec x
  (FixParseBigInt, ArgsU x) ->
    ValueBigInt . parseBigIntText . unString <$> rec x
  (FixConcat, ArgsB x y) -> do
    as <- arrayValues <$> rec x
    bs <- arrayValues <$> rec y
    pure (ValueArray (as ++ bs))
  (FixIncludes, ArgsB xs y) -> do
    yv <- rec y
    evalAsArray rec xs $ \vs ->
      pure (ValueBool (any (valueEq yv) vs))
  (FixJoin, ArgsB xs sep) ->
    evalAsArray rec xs $ \vs -> do
      sv <- rec sep
      pure (ValueString (T.intercalate (unString sv) (map jsJoinElem vs)))
  (FixArrSlice, ArgsT xs a b) -> do
    av <- rec a
    bv <- rec b
    evalAsArray rec xs $ \vs ->
      pure (ValueArray (jsArraySlice vs (unNumber av) (unNumber bv)))
  -- String/regex fixed ops are codegen-only (same as old Un/Bin/Tern gaps).
  _ -> cannotEval "a fixed stdlib op"

mapFixedArgs ::
  forall f a b c.
  (forall v. Expr f v -> Expr f v)
  -> FixedArgs f a b c
  -> FixedArgs f a b c
mapFixedArgs ge = \case
  ArgsU x -> ArgsU (ge x)
  ArgsB x y -> ArgsB (ge x) (ge y)
  ArgsT x y z -> ArgsT (ge x) (ge y) (ge z)

foldFixed ::
  forall f m a b c u.
  Monoid m =>
  (forall v. f v)
  -> (forall v. Expr f v -> m)
  -> FixedOp a b c u
  -> FixedArgs f a b c
  -> m
foldFixed _ se _ = \case
  ArgsU x -> se x
  ArgsB x y -> se x <> se y
  ArgsT x y z -> se x <> se y <> se z

data Metadata = Metadata
  { mdSize :: !Int
  , mdIsPure :: !Bool
  , mdIsCheap :: !Bool
  }

instance Semigroup Metadata where
  Metadata s1 p1 c1 <> Metadata s2 p2 c2 =
    Metadata (s1 + s2) (p1 && p2) (c1 && c2)

instance Monoid Metadata where
  mempty = Metadata 0 True True

-- | 'evaluate' in 'IO'. Same semantics as 'evaluate'.
evaluateCached :: ClosedExpr u -> IO (Value u)
evaluateCached e = pure (evaluate e)

printComputation :: JS -> IO ()
printComputation computation = T.putStrLn (renderJSCompact computation)

helperDecls :: CG -> JS
helperDecls s =
  vcat
    [ ("const" <+> jsText name <+> "=" <+> jsText src) <> semi
    | (name, src) <- M.toAscList (cgHelpers s)
    ]

jsValueEq :: JS -> JS -> JS
jsValueEq a b = "$valueEq" <> parens (a <> ("," <+> b))

jsValueNEq :: JS -> JS -> JS
jsValueNEq a b = "!" <> parens (jsValueEq a b)

useEqHelpers :: CG -> CG
useEqHelpers s0 = foldr (uncurry useHelperSrc) s0 jsEqHelpers

-- | Integer slot + throw on a hole. Raw @a[i]@ would use the string key
-- (@a[1.9]@ is @undefined@) and invent @undefined@ at an arbitrary @u@.
-- Emitted once as @$checkedIndex@; inlining the lambda at every index
-- site blows up Life-sized programs (materializing huge emit trees never finishes).
jsCheckedIndexSrc :: Text
jsCheckedIndexSrc =
  "function(a,i){var n=Math.trunc(i);if(!(n>=0&&n<a.length))throw new Error(\"jshark: index\");return a[n];}"

jsCheckedIndex :: JS -> JS -> JS
jsCheckedIndex arr idx =
  "$checkedIndex" <> parens (arr <> ("," <+> idx))

valueNeedsStructuralEq :: Value u -> Bool
valueNeedsStructuralEq = \case
  ValueArray _ -> True
  ValueFrozen _ -> True
  ValueUint8Array _ -> True
  _ -> False

stdNeedsStructuralEq :: Std Stamp u -> Bool
stdNeedsStructuralEq = \case
  Fixed {} -> False
  Method {} -> True
  -- 'Kernel' itself never forces structural @===@; 'renderKernel' checks
  -- 'needsStructuralEq' on 'KEq'/'KNEq' operands via 'foldKernel'.
  Kernel {} -> False

needsStructuralEq :: Expr Stamp u -> Bool
needsStructuralEq e = case e of
  Var (Embed e') -> needsStructuralEq (flattenExpr e')
  Var _ -> True
  _ ->
    getAny
      ( foldExpr
          nestedDummy
          p
          (const mempty)
          (const mempty)
          e
      )
 where
  p x =
    Any
      ( case x of
          Literal v -> valueNeedsStructuralEq v
          Std s -> stdNeedsStructuralEq s
          Index {} -> True
          U8Index {} -> True
          FrozenLit {} -> True
          GetField {} -> True
          _ -> False
      )

-- | @o.foo@ when @foo@ is an identifier; @o.a.b@ for a dotted ident
-- path ('location.hash'); @o["0"]@ otherwise. A single key that is
-- not an ident must stay bracketed — @window["location.hash"]@ is
-- @undefined@, which made TodoMVC hash filters a no-op.
jsDotOrBracket :: JS -> Text -> JS
jsDotOrBracket obj key
  | jsIdent key = obj <> "." <> jsText key
  | (seg, rest) <- T.break (== '.') key
  , not (T.null rest)
  , jsIdent seg =
      jsDotOrBracket (jsDotOrBracket obj seg) (T.drop 1 rest)
  | otherwise = obj <> "[" <> dquotes (jsText key) <> "]"

jsIdent :: Text -> Bool
jsIdent t = case T.uncons t of
  Nothing -> False
  Just (c, cs) -> jsIdStart c && T.all jsIdPart cs
 where
  jsIdStart x = Char.isAscii x && (Char.isLetter x || x == '_' || x == '$')
  jsIdPart x = jsIdStart x || Char.isDigit x

data Code = MkCode
  { codeDecl :: !(Maybe (JS))
  , codeRef :: !(Maybe (JS))
  , codeRefFX :: !Bool
  }

-- | Two-field sugar for a non-effectful leftover ref.
pattern Code :: JS -> JS -> Code
pattern Code d r <- MkCode (fromMaybe mempty -> d) (fromMaybe mempty -> r) _
 where
  Code d r = MkCode (nonEmpty d) (nonEmpty r) False

{-# COMPLETE Code #-}

fxCode :: JS -> JS -> Code
fxCode d r = MkCode (nonEmpty d) (nonEmpty r) True

-- | New decls, same ref and effectfulness as the source 'Code'.
keepRef :: JS -> Code -> Code
keepRef d (MkCode _ r f) = MkCode (nonEmpty d) r f

instance Semigroup (Code) where
  MkCode a b f <> MkCode x y g = MkCode (a <> x) (b <> y) (f || g)

instance Monoid (Code) where
  mempty = MkCode Nothing Nothing False

renderCode :: Code -> JS
renderCode (MkCode a b _) = fromMaybe mempty a $$ fromMaybe mempty b

-- | Wrap helpers + generated decls + result in an IIFE so a minifier treats
-- the result as live (plain expression statements get DCE'd).
renderIIFE :: CG -> Code -> JS
renderIIFE s (MkCode decls ref _) =
  let
    stmts = helperDecls s $$ fromMaybe mempty decls
    body = case ref of
      Nothing -> stmts
      Just r -> stmts $$ (("return" <+> r) <> semi)
   in
    "(() => {" <> iifeBody body <> "})()"

-- | Helper definitions ahead of a snippet's own declarations.
renderWithHelpers :: CG -> Code -> JS
renderWithHelpers s code = helperDecls s $$ renderCode code

-- | Pure expression compiled to a self-contained JS program (IIFE).
pureProgram :: ClosedExpr u -> JS
pureProgram e = uncurry renderIIFE (pureAST' startCG IM.empty (optimize e))

-- | Effectful computation compiled to a self-contained JS program (IIFE).
effectfulProgram :: ClosedEffect u -> JS
effectfulProgram e
  | closedEffectNodes e >= optIrLargeThreshold =
      uncurry renderIIFE (flatEffectfulCodegen e)
  | otherwise =
      uncurry renderIIFE (effectfulAST' IM.empty startCG (optimizeEffectTree e))

codesDecls :: [Code] -> JS
codesDecls cs = vcat (mapMaybe (\(MkCode a _ _) -> a) cs)

codesRefs :: [Code] -> [JS]
codesRefs = map (\(MkCode _ b _) -> arrayElemRef b)

-- | 'ValueUnit' renders as nothing, since a unit statement emits nothing.
-- As an array element it still occupies a slot, so it has to print — a
-- dropped ref would shorten the literal.
arrayElemRef :: Maybe (JS) -> JS
arrayElemRef = fromMaybe "undefined"

-- Codegen counters: `cgIdent` is the next emitted JS name (`n0`, `n1`, …);
-- `cgTag` is a decreasing negative id used only for use-counting/inlining
-- so nested Lets/Binds cannot collide (tags are never valid JS idents).
-- `cgHelpers` is the set of runtime functions the program has called.
--
-- `cgTag` walks the odd negatives and the optimizer's tags
-- ('optimizeEffect') the even ones, so the two numberings can never name
-- the same binder.
data CG = CG
  { cgIdent :: {-# UNPACK #-} !Int
  , cgTag :: {-# UNPACK #-} !Int
  , cgHelpers :: !(M.Map Text Text)
  }

startCG :: CG
startCG = CG 0 (-3) M.empty

allocTag :: CG -> (Int, CG)
allocTag s = (cgTag s, s {cgTag = cgTag s - 2})

allocIdent :: CG -> (Int, CG)
allocIdent s = (cgIdent s, s {cgIdent = cgIdent s + 1})

useHelperSrc :: Text -> Text -> CG -> CG
useHelperSrc name src s = s {cgHelpers = M.insert name src (cgHelpers s)}

nestedDummyId :: Int
nestedDummyId = minBound

nestedDummy :: Stamp u
nestedDummy = Name nestedDummyId

-- | Codegen binder: @n0@, @n1@, …
nName :: Int -> Text
nName n = "n" <> T.pack (show n)

nJS :: Int -> JS
nJS n = jsText (nName n)

constBind :: Int -> JS -> JS
constBind n ref = ("const" <+> nJS n <+> "=" <+> ref) <> semi

-- | Optimizer tags (negative) map to emitted `n*` ids during codegen.
type Env = IM.IntMap Int

varStampJS :: Env -> Stamp u -> JS
varStampJS env s =
  let
    i = stampId s
   in
    if i < 0
      then maybe mempty nJS (IM.lookup i env)
      else nJS i

-- | Single-pass binder probe for codegen and elim (no per-node 'IntMap').
data BinderScan = BinderScan
  { bsMinNeg :: Maybe Int
  , bsUses :: !Int
  }

instance Semigroup BinderScan where
  BinderScan mn u <> BinderScan mn' u' =
    BinderScan (minMaybe mn mn') (u + u')

instance Monoid BinderScan where
  mempty = BinderScan Nothing 0

minNegStampEff :: Effect Stamp u -> Maybe Int
minNegStampEff e = bsMinNeg (scanMinNegEff e)

scanMinNegVar :: Int -> BinderScan
scanMinNegVar i = BinderScan (if i < 0 then Just i else Nothing) 0

scanMinNegExpr :: Expr Stamp u -> BinderScan
scanMinNegExpr = \case
  Var (Embed e') -> scanMinNegExpr e'
  Var (EmbedEff e') -> scanMinNegEff e'
  Var (Stamp i) -> scanMinNegVar i
  e ->
    foldExpr
      nestedDummy
      scanMinNegExpr
      scanMinNegExpr
      scanMinNegEff
      e

scanMinNegEff :: Effect Stamp u -> BinderScan
scanMinNegEff e =
  foldEff
    nestedDummy
    scanMinNegExpr
    scanMinNegEff
    scanMinNegEff
    e

effectBindUses :: Int -> Effect Stamp u -> Int
effectBindUses tag e =
  let
    n = countEffect tag e
   in
    if n == 0 && occursVarInEff tag e then 2 else n

-- | Codegen only asks whether the bound value is referenced at all, so
-- this stops at the first reference instead of counting every one.
bindProbeTag :: Int -> Effect Stamp u -> (Int, Bool)
bindProbeTag probeTag tagged =
  (probeTag, occursVarInEff probeTag tagged)

letProbeTag :: Int -> Expr Stamp u -> (Int, Int)
letProbeTag probeTag tagged =
  (probeTag, elimExprUses probeTag tagged mempty)

elimExprUses :: Int -> Expr Stamp v -> Metadata -> Int
elimExprUses tag body _ =
  let
    n = countExpr tag body
   in
    if n == 0 && occursVarInExpr tag body then 1 else n

elimEffUses :: Int -> Effect Stamp v -> Metadata -> Int
elimEffUses tag body _ = effectBindUses tag body

minMaybe :: Maybe Int -> Maybe Int -> Maybe Int
minMaybe Nothing y = y
minMaybe x Nothing = x
minMaybe (Just a) (Just b) = Just (min a b)

probeContEff ::
  CG -> (Stamp u -> Effect Stamp v) -> (CG, Effect Stamp v, Int)
probeContEff s f =
  let
    (probeTag, s') = allocTag s
    probed = f (Stamp probeTag)
   in
    (s', probed, probeTag)

probeContExpr ::
  CG -> (Stamp u -> Expr Stamp v) -> (CG, Expr Stamp v, Int)
probeContExpr s g =
  let
    (probeTag, s') = allocTag s
    probed = g (Stamp probeTag)
   in
    (s', probed, probeTag)

-- | Ident already allocated for this effect (@Lift (Var n1)@). Not a
-- counter guess: only a binder that is already in the tree.
liveBinder :: Effect Stamp u -> Maybe Int
liveBinder (Lift e) = liveBinderExpr e
liveBinder _ = Nothing

liveBinderExpr :: Expr Stamp u -> Maybe Int
liveBinderExpr (Var (EmbedEff e)) = liveBinder e
liveBinderExpr (Var (Stamp n)) | n >= 0 = Just n
liveBinderExpr (UnsafeNullable e) = liveBinderExpr e
liveBinderExpr _ = Nothing

-- | @const n = x; k n@ aliases. Dropping them leaks the body stamp.
isAliasBind :: Effect Stamp u -> Bool
isAliasBind (Lift (Var (EmbedEff e))) = isAliasBind e
isAliasBind (Lift (Var _)) = True
isAliasBind (Lift (UnsafeNullable (Var _))) = True
isAliasBind _ = False

jsCall :: JS -> JS -> JS
jsCall f a = parens f <> parens a

-- | Needs no parentheses as an operand: already a primary JS expression.
isSimple :: Expr Stamp u -> Bool
isSimple = \case
  Literal {} -> True
  Var (EmbedEff e) -> isSimpleEffect e
  Var {} -> True
  Std (Kernel (KShow {})) -> True
  Std (Kernel (KTypeOf {})) -> True
  Std (Kernel (KNegate {})) -> True
  Std (Kernel (KBigNeg {})) -> True
  Std (Kernel _) -> False
  Std {} -> True
  FnLit {} -> True
  Index {} -> True
  U8Index {} -> True
  Error {} -> False
  UnsafeNullable x -> isSimple x
  FrozenLit {} -> True
  GetField {} -> True
  Hvm2Kernel {} -> True
  _ -> False

isSimpleEffect :: Effect Stamp u -> Bool
isSimpleEffect = \case
  Lift x -> isSimple x
  FFI {} -> True
  CallMethod {} -> True
  UnsafeObject {} -> True
  UnsafeObjectGet {} -> True
  ArrayLit es -> all isSimpleEffect es
  _ -> False

wrapOperand :: Expr Stamp u -> JS -> JS
wrapOperand e d = if isSimple e then d else parens d

-- A use under a lambda, loop, `&&`/`||` RHS, or `?:` branch is not a
-- candidate for inlining: the binder would be re-run or skipped.
countLazyExpr :: Int -> Expr Stamp u -> Int
countLazyExpr t e = if countExpr t e == 0 then 0 else 2

countLazyEffect :: Int -> Effect Stamp u -> Int
countLazyEffect t e = if countEffect t e == 0 then 0 else 2

countExpr :: Int -> Expr Stamp u -> Int
countExpr t e = case e of
  Var (Stamp i) -> if i == t then 1 else 0
  Var (Embed e') -> countExpr t e'
  Var (EmbedEff e') -> countEffect t e'
  _ ->
    getSum
      ( foldExpr
          nestedDummy
          (Sum . countExpr t)
          (Sum . countLazyExpr t)
          (Sum . countEffect t)
          e
      )

countEffect :: Int -> Effect Stamp u -> Int
countEffect t =
  getSum
    . foldEff
      nestedDummy
      (Sum . countExpr t)
      (Sum . countEffect t)
      (Sum . countLazyEffect t)

newtype Occ = Occ {getOcc :: Bool}

instance Semigroup Occ where
  Occ a <> Occ b = Occ (a || b)

instance Monoid Occ where
  mempty = Occ False

occursVarInExpr :: Int -> Expr Stamp u -> Bool
occursVarInExpr t = \case
  Var (Stamp i) -> i == t
  Var (Embed e') -> occursVarInExpr t e'
  Var (EmbedEff e') -> occursVarInEff t e'
  e ->
    getOcc $
      foldExpr
        nestedDummy
        (Occ . occursVarInExpr t)
        (Occ . occursVarInExpr t)
        (Occ . occursVarInEff t)
        e

occursVarInEff :: Int -> Effect Stamp u -> Bool
occursVarInEff t =
  getOcc
    . foldEff
      nestedDummy
      (Occ . occursVarInExpr t)
      (Occ . occursVarInEff t)
      (Occ . occursVarInEff t)

-- | Structural node count. Lazy children (lambda bodies, @?:@ arms,
-- @&&@ RHS) are part of the tree, so they count: a size gate that
-- skipped them under-measured a whole paint body as a leaf.
nodeCountExpr :: Expr Stamp u -> Int
nodeCountExpr = \case
  Var (Embed e') -> nodeCountExpr e'
  Var (EmbedEff e') -> nodeCountEff e'
  e ->
    1
      + getSum
        ( foldExpr
            nestedDummy
            (Sum . nodeCountExpr)
            (Sum . nodeCountExpr)
            (Sum . nodeCountEff)
            e
        )

nodeCountEff :: Effect Stamp u -> Int
nodeCountEff e =
  1
    + getSum
      ( foldEff
          nestedDummy
          (Sum . nodeCountExpr)
          (Sum . nodeCountEff)
          (Sum . nodeCountEff)
          e
      )

closedEffectNodes :: ClosedEffect u -> Int
closedEffectNodes (e :: ClosedEffect u) =
  let
    (_, ir) = lowerEffectAt (-2) (flattenEff (e :: Effect Stamp u))
   in
    Ir.irMetaSize (Ir.metaIrEffect ir)
{-# NOINLINE closedEffectNodes #-}

cheapExpr :: Expr Stamp u -> Bool
cheapExpr = \case
  Literal v -> isCheapValue v
  Var (Embed e') -> cheapExpr e'
  Var (EmbedEff e') -> cheapEffect e'
  Var _ -> True
  e ->
    let
      here = case e of
        UnsafeNullable {} -> True
        GetField {} -> True
        _ -> False
     in
      here
        && getAll
          ( foldExpr
              nestedDummy
              (All . cheapExpr)
              (const mempty)
              (All . cheapEffect)
              e
          )

cheapEffect :: Effect Stamp u -> Bool
cheapEffect e =
  let
    here = case e of
      Lift {} -> True
      _ -> False
   in
    here
      && getAll
        ( foldEff
            nestedDummy
            (All . cheapExpr)
            (All . cheapEffect)
            (const mempty)
            e
        )

pureExpr :: Expr Stamp u -> Bool
pureExpr = \case
  Literal _ -> True
  Var (Embed e') -> pureExpr e'
  Var (EmbedEff e') -> pureEffect e'
  Var _ -> True
  e ->
    let
      here = case e of
        Std (Fixed op _) -> Prim.isPureFixed op
        _ -> True
     in
      here
        && getAll
          ( foldExpr
              nestedDummy
              (All . pureExpr)
              (const mempty)
              (All . pureEffect)
              e
          )

pureEffect :: Effect Stamp u -> Bool
pureEffect e =
  let
    here = case e of
      FFI {} -> False
      UnsafeObjectGet {} -> False
      UnsafeObjectAssign {} -> False
      CallMethod {} -> False
      ApplyE {} -> False
      While {} -> False
      ForRange {} -> False
      U8Set {} -> False
      U8Fill {} -> False
      Throw {} -> False
      Try {} -> False
      DeleteProp {} -> False
      _ -> True
   in
    here
      && getAll
        ( foldEff
            nestedDummy
            (All . pureExpr)
            (All . pureEffect)
            (const mempty)
            e
        )

-- Re-opt only small trees. A second walk of a @bindRec@ / do-chain
-- paint body is what hung todo-mvc and breakout.
optSmall :: Int
optSmall = 16

-- | PHOAS 'optEffect' is quadratic on long bind chains; IR opt for huge ASTs.
optIrLargeThreshold :: Int
optIrLargeThreshold = 50000

-- | First-order reopen: rename the tag allocated by 'optUnder'. Never
-- re-applies the original PHOAS @f@. Same tag is identity. The fold dummy
-- is inspect-only (no copy); count/codegen probe with `allocTag`, never
-- `nestedDummyId` as this binder.
rebindExpr :: Int -> Expr Stamp v -> Stamp u -> Expr Stamp v
rebindExpr tag body s
  | i == tag || i == nestedDummyId = body
  | otherwise = renameExpr tag i body
 where
  i = stampId s

rebindEff :: Int -> Effect Stamp v -> Stamp u -> Effect Stamp v
rebindEff tag body s
  | i == tag || i == nestedDummyId = body
  | otherwise = renameEff tag i body
 where
  i = stampId s

rebindExpr2 :: Int -> Int -> Expr Stamp v -> Stamp a -> Stamp b -> Expr Stamp v
rebindExpr2 tA tB body a b = rebindExpr tA (rebindExpr tB body b) a

keepExprCont ::
  Int
  -> Int
  -> Expr Stamp v
  -> Metadata
  -> (Stamp u -> Expr Stamp v)
  -> Stamp u
  -> Expr Stamp v
keepExprCont t tag body _ f
  | nodeCountExpr body <= optSmall = reoptExpr t f
  | otherwise = rebindExpr tag body

keepEffCont ::
  Int
  -> Int
  -> Effect Stamp v
  -> Metadata
  -> (Stamp u -> Effect Stamp v)
  -> Stamp u
  -> Effect Stamp v
keepEffCont t tag body _ f
  | nodeCountEff body <= optSmall = reoptEff t f
  | otherwise = rebindEff tag body

keepExprCont2 ::
  Int
  -> Int
  -> Int
  -> Expr Stamp v
  -> Metadata
  -> (Stamp a -> Stamp b -> Expr Stamp v)
  -> Stamp a
  -> Stamp b
  -> Expr Stamp v
keepExprCont2 _ tA tB body _ _ a b = rebindExpr2 tA tB body a b

mapFnBody ::
  forall f.
  (forall v. Expr f v -> Expr f v)
  -> forall us r.
  FnBody f us r
  -> FnBody f us r
mapFnBody ge = \case
  JfNil e -> JfNil (ge e)
  JfCons k -> JfCons (\x -> mapFnBody ge (k x))

foldFnBody ::
  forall f us r m.
  (forall v. f v)
  -> (forall v. Expr f v -> m)
  -> FnBody f us r
  -> m
foldFnBody dummy le body =
  le (evalFnBodyWith dummy body)

evalFnBodyWith :: forall f us r. (forall v. f v) -> FnBody f us r -> Expr f r
evalFnBodyWith dummy = \case
  JfNil e -> e
  JfCons k -> evalFnBodyWith dummy (k dummy)

evalFnBody :: forall us r. FnBody Stamp us r -> [Int] -> Expr Stamp r
evalFnBody body tags =
  unsafeCoerce (evalAny (unsafeCoerce body) tags :: Expr Stamp r)
 where
  evalAny (JfNil e) [] = e
  evalAny (JfCons k) (t : ts) = evalAny (unsafeCoerce (k (Name t))) ts
  evalAny _ _ = error "JShark.evalFnBody: arity mismatch"

rebindFn :: [Int] -> Expr Stamp v -> FnBody Stamp us v
rebindFn tags expr = unsafeCoerce (rebindGo tags expr)
 where
  rebindGo [] e = JfNil e
  rebindGo (t : ts) e = unsafeCoerce (JfCons $ \s -> rebindGo ts (rebindExpr t e s))

fnDepthStamp :: FnBody Stamp us r -> Int
fnDepthStamp = \case
  JfNil _ -> 0
  JfCons k -> 1 + fnDepthStamp (k (Stamp minBound))

allocFnTags :: Int -> FnBody Stamp us r -> ([Int], Int)
allocFnTags t0 body =
  let
    n = fnDepthStamp body
    tags = take n [t0, t0 - optStep ..]
    tEnd = t0 - n * optStep
   in
    (tags, tEnd)

optUnderFn ::
  Int
  -> FnBody Stamp us v
  -> (Int, [Int], Expr Stamp v, Metadata, FnBody Stamp us v)
optUnderFn t0 body =
  let
    (tags, tEnd) = allocFnTags t0 body
    expr = evalFnBody body tags
    (t1, expr', md) = optExpr tEnd expr
   in
    (t1, tags, expr', md, body)

keepFnCont ::
  [Int] -> Expr Stamp v -> FnBody Stamp us v -> FnBody Stamp us v
-- Rebuild the 'FnBody' spine from optimized @expr'@ only; the pre-opt @body@
-- tag layout must not be re-run through 'optExpr' (that left negative tags).
keepFnCont tags expr' _body = rebindFn tags expr'

allocNIdents :: CG -> Int -> ([Int], CG)
allocNIdents s 0 = ([], s)
allocNIdents s n =
  let
    (i, s1) = allocIdent s
    (is, s2) = allocNIdents s1 (n - 1)
   in
    (i : is, s2)

fnArity :: FnBody Stamp us r -> Int
fnArity = fnDepthStamp

renderFn :: forall us r. Env -> CG -> FnBody Stamp us r -> (CG, Code)
renderFn env s0 body =
  let
    n = fnArity body
    (ids, s1) = allocNIdents s0 n
    (s2, Code d r) = pureAST' s1 env (evalFnBody body ids)
   in
    (s2, Code mempty (jsCallback (map nJS ids) d r))

mapFieldLit ::
  (forall u. Expr f u -> Expr f u)
  -> (forall u. Effect f u -> Effect f u)
  -> FieldLit f r
  -> FieldLit f r
mapFieldLit ge _ (FieldLit @k e) = FieldLit @k (ge e)
mapFieldLit _ gf (FieldLitEffect @k e) = FieldLitEffect @k (gf e)
mapFieldLit ge _ (FieldLitExtra @k e) = FieldLitExtra @k (ge e)
mapFieldLit _ gf (FieldLitExtraEffect @k e) = FieldLitExtraEffect @k (gf e)

mapArg ::
  (forall v. Expr f v -> Expr f v)
  -> (forall v. Effect f v -> Effect f v)
  -> Arg f u
  -> Arg f u
mapArg ge _ (ArgExpr e) = ArgExpr (ge e)
mapArg _ gf (ArgEffect e) = ArgEffect (gf e)

-- | Rebuild by rewriting immediate children. 'Var' / 'Literal' are leaves.
mapExpr ::
  (forall v. Expr f v -> Expr f v)
  -> (forall v. Effect f v -> Effect f v)
  -> Expr f u
  -> Expr f u
mapExpr ge gf = \case
  Literal x -> Literal x
  Var s -> Var s
  Let x g -> Let (ge x) (ge . g)
  LetRec rhs body -> LetRec (ge . rhs) (ge . body)
  Lambda g -> Lambda (ge . g)
  Apply f x -> Apply (ge f) (ge x)
  If c u v -> If (ge c) (ge u) (ge v)
  OptionCase o n s -> OptionCase (ge o) (ge n) (ge . s)
  ResultOk x -> ResultOk (ge x)
  ResultErr x -> ResultErr (ge x)
  ResultCase o e s -> ResultCase (ge o) (ge . e) (ge . s)
  Index x i -> Index (ge x) (ge i)
  U8Index x i -> U8Index (ge x) (ge i)
  Error x -> Error (ge x)
  Std s -> Std (mapStd ge s)
  FnLit body -> FnLit (mapFnBody ge body)
  UnsafeNullable x -> UnsafeNullable (ge x)
  FrozenLit fs -> FrozenLit (map (mapFieldLit ge gf) fs)
  GetField @k o -> GetField @k (ge o)
  Hvm2Kernel name k -> Hvm2Kernel name k

mapStd ::
  (forall v. Expr f v -> Expr f v)
  -> Std f u
  -> Std f u
mapStd ge = \case
  Fixed op args -> Fixed op (mapFixedArgs ge args)
  Method m -> Method (mapMethod ge m)
  Kernel k -> Kernel (mapKernel ge k)

mapKernel ::
  (forall v. Expr f v -> Expr f v)
  -> Kernel f u
  -> Kernel f u
mapKernel ge = \case
  KPlus x y -> KPlus (ge x) (ge y)
  KTimes x y -> KTimes (ge x) (ge y)
  KMinus x y -> KMinus (ge x) (ge y)
  KNegate x -> KNegate (ge x)
  KFracDiv x y -> KFracDiv (ge x) (ge y)
  KRem x y -> KRem (ge x) (ge y)
  KBitAnd x y -> KBitAnd (ge x) (ge y)
  KBitOr x y -> KBitOr (ge x) (ge y)
  KBitXor x y -> KBitXor (ge x) (ge y)
  KShl x y -> KShl (ge x) (ge y)
  KShr x y -> KShr (ge x) (ge y)
  KUShr x y -> KUShr (ge x) (ge y)
  KBig op x y -> KBig op (ge x) (ge y)
  KBigNeg x -> KBigNeg (ge x)
  KConcat x y -> KConcat (ge x) (ge y)
  KShow x -> KShow (ge x)
  KTypeOf x -> KTypeOf (ge x)
  KAnd x y -> KAnd (ge x) (ge y)
  KOr x y -> KOr (ge x) (ge y)
  KEq structural x y -> KEq structural (ge x) (ge y)
  KNEq structural x y -> KNEq structural (ge x) (ge y)
  KGTh x y -> KGTh (ge x) (ge y)
  KLTh x y -> KLTh (ge x) (ge y)
  KGTEq x y -> KGTEq (ge x) (ge y)
  KLTEq x y -> KLTEq (ge x) (ge y)

mapMethod ::
  (forall v. Expr f v -> Expr f v)
  -> Method f u
  -> Method f u
mapMethod ge = \case
  MethMap x f -> MethMap (ge x) (ge . f)
  MethFilter x f -> MethFilter (ge x) (ge . f)
  MethReduce x z f -> MethReduce (ge x) (ge z) (\a b -> ge (f a b))
  MethReduceRight x z f -> MethReduceRight (ge x) (ge z) (\a b -> ge (f a b))
  MethToSorted x f -> MethToSorted (ge x) (\a b -> ge (f a b))
  MethFrom n f -> MethFrom (ge n) (ge . f)

mapEff ::
  (forall v. Expr f v -> Expr f v)
  -> (forall v. Effect f v -> Effect f v)
  -> Effect f u
  -> Effect f u
mapEff ge gf = \case
  Lift x -> Lift (ge x)
  FFI n args -> FFI n (mapRec (mapArg ge gf) args)
  UnsafeObject o -> UnsafeObject o
  UnsafeObjectGet x s -> UnsafeObjectGet (gf x) s
  UnsafeObjectAssign x y -> UnsafeObjectAssign (gf x) (gf y)
  CallMethod x n args -> CallMethod (gf x) n (mapRec (mapArg ge gf) args)
  Bind x f -> Bind (gf x) (gf . f)
  ThenE x y -> ThenE (gf x) (gf y)
  BindRec rhs body -> BindRec (gf . rhs) (gf . body)
  LambdaE f -> LambdaE (gf . f)
  ApplyE f x -> ApplyE (gf f) (gf x)
  IfE c u v -> IfE (gf c) (gf u) (gf v)
  While c b -> While (gf c) (gf b)
  ForRange s e b -> ForRange (ge s) (ge e) (gf . b)
  U8Set b i v -> U8Set (ge b) (ge i) (ge v)
  U8Fill b v -> U8Fill (ge b) (ge v)
  OptionCaseE o n s -> OptionCaseE (ge o) (gf n) (gf . s)
  ResultCaseE o e s -> ResultCaseE (ge o) (gf . e) (gf . s)
  StringCaseE o arms d ->
    StringCaseE (ge o) (map (fmap gf) arms) (gf d)
  Throw x -> Throw (ge x)
  Try a k -> Try (gf a) (gf . k)
  ObjectLit fs -> ObjectLit (map (mapFieldLit ge gf) fs)
  DeleteProp o k -> DeleteProp (gf o) (ge k)
  ArrayLit es -> ArrayLit (map gf es)

-- | Immediate children. Lazy positions (&&/|| RHS, lambda, ?: arms)
-- use @le@. Binders are applied to @dummy@.
-- 'Expr' has no lazy 'Effect' child; see 'foldEff' for @lf@.
foldExpr ::
  forall f m u.
  Monoid m =>
  (forall v. f v)
  -> (forall v. Expr f v -> m)
  -> (forall v. Expr f v -> m)
  -> (forall v. Effect f v -> m)
  -> Expr f u
  -> m
foldExpr dummy se le sf = \case
  Literal {} -> mempty
  Var {} -> mempty
  Let x g -> se x <> se (g dummy)
  LetRec r b -> le (r dummy) <> se (b dummy)
  Lambda g -> le (g dummy)
  Apply f x -> se f <> se x
  If c u v -> se c <> le u <> le v
  OptionCase o n s -> se o <> le n <> le (s dummy)
  ResultOk x -> se x
  ResultErr x -> se x
  ResultCase o e s -> se o <> le (e dummy) <> le (s dummy)
  Index x i -> se x <> se i
  U8Index x i -> se x <> se i
  Error x -> se x
  Std s -> foldStd dummy se le s
  FnLit body -> foldFnBody dummy le body
  UnsafeNullable x -> se x
  FrozenLit fs -> foldMap (foldFieldLit se sf) fs
  GetField o -> se o
  Hvm2Kernel {} -> mempty

foldStd ::
  forall f m u.
  Monoid m =>
  (forall v. f v)
  -> (forall v. Expr f v -> m)
  -> (forall v. Expr f v -> m)
  -> Std f u
  -> m
foldStd dummy se le = \case
  Fixed op args -> foldFixed dummy se op args
  Method m -> foldMethod dummy se le m
  Kernel k -> foldKernel se le k

foldKernel ::
  forall f m u.
  Monoid m =>
  (forall v. Expr f v -> m)
  -> (forall v. Expr f v -> m)
  -> Kernel f u
  -> m
foldKernel se le = \case
  KPlus x y -> se x <> se y
  KTimes x y -> se x <> se y
  KMinus x y -> se x <> se y
  KNegate x -> se x
  KFracDiv x y -> se x <> se y
  KRem x y -> se x <> se y
  KBitAnd x y -> se x <> se y
  KBitOr x y -> se x <> se y
  KBitXor x y -> se x <> se y
  KShl x y -> se x <> se y
  KShr x y -> se x <> se y
  KUShr x y -> se x <> se y
  KBig _ x y -> se x <> se y
  KBigNeg x -> se x
  KConcat x y -> se x <> se y
  KShow x -> se x
  KTypeOf x -> se x
  KAnd x y -> se x <> le y
  KOr x y -> se x <> le y
  KEq _ x y -> se x <> se y
  KNEq _ x y -> se x <> se y
  KGTh x y -> se x <> se y
  KLTh x y -> se x <> se y
  KGTEq x y -> se x <> se y
  KLTEq x y -> se x <> se y

foldMethod ::
  forall f m u.
  Monoid m =>
  (forall v. f v)
  -> (forall v. Expr f v -> m)
  -> (forall v. Expr f v -> m)
  -> Method f u
  -> m
foldMethod dummy se le = \case
  MethMap x f -> se x <> le (f dummy)
  MethFilter x f -> se x <> le (f dummy)
  MethReduce x z f -> se x <> se z <> le (f dummy dummy)
  MethReduceRight x z f -> se x <> se z <> le (f dummy dummy)
  MethToSorted x f -> se x <> le (f dummy dummy)
  MethFrom n f -> se n <> le (f dummy)

foldFieldLit ::
  (forall v. Expr f v -> m)
  -> (forall v. Effect f v -> m)
  -> FieldLit f r
  -> m
foldFieldLit se _ (FieldLit e) = se e
foldFieldLit _ sf (FieldLitEffect e) = sf e
foldFieldLit se _ (FieldLitExtra e) = se e
foldFieldLit _ sf (FieldLitExtraEffect e) = sf e

foldEff ::
  forall f m u.
  Monoid m =>
  (forall v. f v)
  -> (forall v. Expr f v -> m)
  -> (forall v. Effect f v -> m)
  -> (forall v. Effect f v -> m)
  -> Effect f u
  -> m
foldEff dummy se sf lf = \case
  Lift x -> se x
  FFI _ args -> recFold (\n a -> n <> foldArg a) mempty args
  UnsafeObject {} -> mempty
  UnsafeObjectGet x _ -> sf x
  UnsafeObjectAssign x y -> sf x <> sf y
  CallMethod x _ args -> sf x <> recFold (\n a -> n <> foldArg a) mempty args
  Bind x f -> sf x <> sf (f dummy)
  ThenE x y -> sf x <> sf y
  BindRec r b -> lf (r dummy) <> sf (b dummy)
  LambdaE f -> lf (f dummy)
  ApplyE f x -> sf f <> sf x
  IfE c u v -> sf c <> lf u <> lf v
  While c b -> lf c <> lf b
  ForRange s e b -> se s <> se e <> lf (b dummy)
  U8Set b i v -> se b <> se i <> se v
  U8Fill b v -> se b <> se v
  OptionCaseE o n s -> se o <> lf n <> lf (s dummy)
  ResultCaseE o e s -> se o <> lf (e dummy) <> lf (s dummy)
  StringCaseE o arms d -> se o <> foldMap (lf . snd) arms <> lf d
  Throw x -> se x
  Try a k -> sf a <> lf (k dummy)
  ObjectLit fs -> foldMap (foldFieldLit se sf) fs
  DeleteProp o k -> sf o <> se k
  ArrayLit es -> foldMap sf es
 where
  foldArg :: forall x. Arg f x -> m
  foldArg (ArgExpr e) = se e
  foldArg (ArgEffect e) = sf e

lookupField ::
  forall k r f. KnownSymbol k => [FieldLit f r] -> Maybe (Expr f (Field r k))
lookupField = findLit . reverse
 where
  findLit [] = Nothing
  findLit (FieldLit @k' e : rest) =
    case sameSymbol (Proxy @k) (Proxy @k') of
      Just Refl -> Just e
      Nothing -> findLit rest
  findLit (_ : rest) = findLit rest

fieldsPure :: PhoasDummy f => [FieldLit f r] -> Bool
fieldsPure = all $ \case
  FieldLit e -> isPureExpr e
  FieldLitExtra e -> isPureExpr e
  FieldLitEffect {} -> False
  FieldLitExtraEffect {} -> False

-- | Last-wins, and only when every sibling is observationally pure
-- (so projecting @.b@ cannot DCE @JSON.stringify@ in @.a@).
projectFrozenField ::
  forall k r f.
  (KnownSymbol k, PhoasDummy f) => [FieldLit f r] -> Maybe (Expr f (Field r k))
projectFrozenField fs
  | fieldsPure fs = lookupField @k fs
  | otherwise = Nothing

withFrozenField ::
  forall k r a.
  KnownSymbol k =>
  Value ('Object r)
  -> (Expr Value (Field r k) -> a)
  -> a
withFrozenField (ValueFrozen fs) k =
  case projectFrozenField @k fs of
    Just e -> k e
    Nothing -> cannotEval "GetField of a frozen object with effectful fields"

foldGetField ::
  forall k r f.
  (KnownSymbol k, PhoasDummy f) =>
  Expr f ('Object r) -> Maybe (Expr f (Field r k))
foldGetField = \case
  FrozenLit fs -> projectFrozenField @k fs
  If (Literal (ValueBool True)) t _ -> foldGetField @k t
  If (Literal (ValueBool False)) _ e -> foldGetField @k e
  _ -> Nothing

-- | Unwrap 'Embed' holes. The universe of the hole is the universe of the
-- 'Var', so this is ordinary GADT coverage — not a cast.
-- | Remove 'Embed' nodes from the tree. Phantom in the universe, so this
-- does not need a cast.
flattenExpr :: Expr Stamp u -> Expr Stamp u
flattenExpr = \case
  Var (Embed x) -> flattenExpr x
  Var (EmbedEff (Lift x)) -> flattenExpr x
  Var (EmbedEff x) -> Var (EmbedEff (flattenEff x))
  e -> mapExpr flattenExpr flattenEff e

flattenEff :: Effect Stamp u -> Effect Stamp u
flattenEff = \case
  Lift (Var (EmbedEff x)) -> flattenEff x
  e -> mapEff flattenExpr flattenEff e

-- | Replace 'Stamp' @old@ with @new@. Phantom in the universe, so this
-- does not need a cast. Used after the one 'optUnder' apply of @f@.
-- The occurrence check answers "is there anything to do here", so it
-- belongs at the top of a rename only. Repeating it at every node of the
-- descent re-reads each subtree once per ancestor, which turns one
-- rename into work proportional to size times depth.
renameExpr :: Int -> Int -> Expr Stamp u -> Expr Stamp u
renameExpr old new e
  | old == new = e
  | not (occursVarInExpr old e) = e
  | otherwise = renameExprGo old new e

renameExprGo :: Int -> Int -> Expr Stamp u -> Expr Stamp u
renameExprGo old new = \case
  Var (Embed e') -> renameExprGo old new (flattenExpr e')
  Var (EmbedEff (Lift e')) -> renameExprGo old new (flattenExpr e')
  Var (EmbedEff e') -> Var (EmbedEff (renameEffGo old new e'))
  Var (Stamp t) | t == old -> Var (Stamp new)
  Var s -> Var s
  e -> mapExpr (renameExprGo old new) (renameEffGo old new) e

renameEff :: Int -> Int -> Effect Stamp u -> Effect Stamp u
renameEff old new e
  | old == new = e
  | not (occursVarInEff old e) = e
  | otherwise = renameEffGo old new e

renameEffGo :: Int -> Int -> Effect Stamp u -> Effect Stamp u
renameEffGo old new = \case
  Lift (Var (EmbedEff e')) -> renameEffGo old new (flattenEff e')
  e -> mapEff (renameExprGo old new) (renameEffGo old new) e

inlineExpr :: (Stamp u -> Expr Stamp v) -> Expr Stamp u -> Expr Stamp v
inlineExpr f x = flattenExpr (f (Embed x))

inlineEff :: (Stamp u -> Effect Stamp v) -> Effect Stamp u -> Effect Stamp v
inlineEff f x = flattenEff (f (EmbedEff x))

-- | Re-apply a PHOAS continuation and optimize at the next free tag @t@
-- (never a reset @-2@ — that collides with 'Stamp's already in the tree).
reoptExpr :: Int -> (Stamp u -> Expr Stamp v) -> Stamp u -> Expr Stamp v
reoptExpr t f b = let (_, e, _) = optExpr t (flattenExpr (f b)) in e

reoptEff :: Int -> (Stamp u -> Effect Stamp v) -> Stamp u -> Effect Stamp v
reoptEff t f b = let (_, e, _) = optEffect t (flattenEff (f b)) in e

reoptExpr2 ::
  Int
  -> (Stamp u -> Stamp w -> Expr Stamp v)
  -> Stamp u
  -> Stamp w
  -> Expr Stamp v
reoptExpr2 t f a b = let (_, e, _) = optExpr t (flattenExpr (f a b)) in e

lowerArg :: Arg Stamp u -> Ir.IrArg u
lowerArg = \case
  ArgExpr e -> Ir.IrArgExpr (lowerExpr e)
  ArgEffect e -> Ir.IrArgEffect (lowerEffect e)

lowerArgAt :: Int -> Arg Stamp u -> (Int, Ir.IrArg u)
lowerArgAt t0 = \case
  ArgExpr e ->
    let
      (t1, e') = lowerExprAt t0 e
     in
      (t1, Ir.IrArgExpr e')
  ArgEffect e ->
    let
      (t1, e') = lowerEffectAt t0 e
     in
      (t1, Ir.IrArgEffect e')

lowerRecArgsAt ::
  Int -> Rec (Arg Stamp) us -> (Int, Rec (Ir.IrArg) us)
lowerRecArgsAt t0 = \case
  RecNil -> (t0, RecNil)
  RecCons x xs ->
    let
      (t1, x') = lowerArgAt t0 x
      (t2, xs') = lowerRecArgsAt t1 xs
     in
      (t2, RecCons x' xs')

lowerArgsAt ::
  Int -> Rec (Arg Stamp) us -> (Int, Rec (Ir.IrArg) us)
lowerArgsAt = lowerRecArgsAt

lowerFixedArgsAt ::
  Int -> FixedArgs Stamp a b c -> (Int, Ir.IrFixedArgs a b c)
lowerFixedArgsAt t0 = \case
  ArgsU x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.IrArgsU x')
  ArgsB x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.IrArgsB x' y')
  ArgsT x y z ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
      (t3, z') = lowerExprAt t2 z
     in
      (t3, Ir.IrArgsT x' y' z')

lowerKernelKAt :: Int -> Kernel Stamp u -> (Int, Ir.IrKernel u)
lowerKernelKAt t0 = \case
  KPlus x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KPlus x' y')
  KTimes x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KTimes x' y')
  KMinus x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KMinus x' y')
  KNegate x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.KNegate x')
  KFracDiv x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KFracDiv x' y')
  KRem x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KRem x' y')
  KBitAnd x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KBitAnd x' y')
  KBitOr x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KBitOr x' y')
  KBitXor x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KBitXor x' y')
  KShl x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KShl x' y')
  KShr x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KShr x' y')
  KUShr x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KUShr x' y')
  KBig op x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KBig op x' y')
  KBigNeg x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.KBigNeg x')
  KConcat x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KConcat x' y')
  KShow x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.KShow x')
  KTypeOf x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.KTypeOf x')
  KAnd x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KAnd x' y')
  KOr x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KOr x' y')
  KEq s x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KEq s x' y')
  KNEq s x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KNEq s x' y')
  KGTh x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KGTh x' y')
  KLTh x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KLTh x' y')
  KGTEq x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KGTEq x' y')
  KLTEq x y ->
    let
      (t1, x') = lowerExprAt t0 x
      (t2, y') = lowerExprAt t1 y
     in
      (t2, Ir.KLTEq x' y')

reifyFixedArgs ::
  Ir.IrFixedArgs a b c -> FixedArgs Stamp a b c
reifyFixedArgs = \case
  Ir.IrArgsU x -> ArgsU (reifyExpr x)
  Ir.IrArgsB x y -> ArgsB (reifyExpr x) (reifyExpr y)
  Ir.IrArgsT x y z -> ArgsT (reifyExpr x) (reifyExpr y) (reifyExpr z)

lowerFixedArgs ::
  FixedArgs Stamp a b c -> Ir.IrFixedArgs a b c
lowerFixedArgs args = snd (lowerFixedArgsAt (-2) args)

lowerKernelK :: Kernel Stamp u -> Ir.IrKernel u
lowerKernelK k = snd (lowerKernelKAt (-2) k)

reifyKernelK :: Ir.IrKernel u -> Kernel Stamp u
reifyKernelK = \case
  Ir.KPlus x y -> KPlus (reifyExpr x) (reifyExpr y)
  Ir.KTimes x y -> KTimes (reifyExpr x) (reifyExpr y)
  Ir.KMinus x y -> KMinus (reifyExpr x) (reifyExpr y)
  Ir.KNegate x -> KNegate (reifyExpr x)
  Ir.KFracDiv x y -> KFracDiv (reifyExpr x) (reifyExpr y)
  Ir.KRem x y -> KRem (reifyExpr x) (reifyExpr y)
  Ir.KBitAnd x y -> KBitAnd (reifyExpr x) (reifyExpr y)
  Ir.KBitOr x y -> KBitOr (reifyExpr x) (reifyExpr y)
  Ir.KBitXor x y -> KBitXor (reifyExpr x) (reifyExpr y)
  Ir.KShl x y -> KShl (reifyExpr x) (reifyExpr y)
  Ir.KShr x y -> KShr (reifyExpr x) (reifyExpr y)
  Ir.KUShr x y -> KUShr (reifyExpr x) (reifyExpr y)
  Ir.KBig op x y -> KBig op (reifyExpr x) (reifyExpr y)
  Ir.KBigNeg x -> KBigNeg (reifyExpr x)
  Ir.KConcat x y -> KConcat (reifyExpr x) (reifyExpr y)
  Ir.KShow x -> KShow (reifyExpr x)
  Ir.KTypeOf x -> KTypeOf (reifyExpr x)
  Ir.KAnd x y -> KAnd (reifyExpr x) (reifyExpr y)
  Ir.KOr x y -> KOr (reifyExpr x) (reifyExpr y)
  Ir.KEq s x y -> KEq s (reifyExpr x) (reifyExpr y)
  Ir.KNEq s x y -> KNEq s (reifyExpr x) (reifyExpr y)
  Ir.KGTh x y -> KGTh (reifyExpr x) (reifyExpr y)
  Ir.KLTh x y -> KLTh (reifyExpr x) (reifyExpr y)
  Ir.KGTEq x y -> KGTEq (reifyExpr x) (reifyExpr y)
  Ir.KLTEq x y -> KLTEq (reifyExpr x) (reifyExpr y)

lowerStdMethodAt :: Int -> Method Stamp u -> (Int, Ir.IrMethod u)
lowerStdMethodAt t0 = \case
  MethMap arr f ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, arr') = lowerExprAt tUnder arr
      (t2, body') = lowerExprAt t1 (flattenExpr (f (Name tag)))
     in
      (t2, Ir.IrMethMap arr' tag body')
  MethFilter arr f ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, arr') = lowerExprAt tUnder arr
      (t2, body') = lowerExprAt t1 (flattenExpr (f (Name tag)))
     in
      (t2, Ir.IrMethFilter arr' tag body')
  MethReduce arr z f ->
    let
      tagA = t0
      tagB = t0 - optStep
      tUnder = t0 - 2 * optStep
      (t1, arr') = lowerExprAt tUnder arr
      (t2, z') = lowerExprAt t1 z
      (t3, body') =
        lowerExprAt t2 (flattenExpr (f (Name tagA) (Name tagB)))
     in
      (t3, Ir.IrMethReduce arr' z' tagA tagB body')
  MethReduceRight arr z f ->
    let
      tagA = t0
      tagB = t0 - optStep
      tUnder = t0 - 2 * optStep
      (t1, arr') = lowerExprAt tUnder arr
      (t2, z') = lowerExprAt t1 z
      (t3, body') =
        lowerExprAt t2 (flattenExpr (f (Name tagA) (Name tagB)))
     in
      (t3, Ir.IrMethReduceRight arr' z' tagA tagB body')
  MethToSorted arr f ->
    let
      tagA = t0
      tagB = t0 - optStep
      tUnder = t0 - 2 * optStep
      (t1, arr') = lowerExprAt tUnder arr
      (t2, body') =
        lowerExprAt t1 (flattenExpr (f (Name tagA) (Name tagB)))
     in
      (t2, Ir.IrMethToSorted arr' tagA tagB body')
  MethFrom n f ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, n') = lowerExprAt tUnder n
      (t2, body') = lowerExprAt t1 (flattenExpr (f (Name tag)))
     in
      (t2, Ir.IrMethFrom n' tag body')

reifyStdMethod :: Ir.IrMethod u -> Method Stamp u
reifyStdMethod = \case
  Ir.IrMethMap arr tag body ->
    MethMap (reifyExpr arr) (\s -> rebindExpr tag (reifyExpr body) s)
  Ir.IrMethFilter arr tag body ->
    MethFilter (reifyExpr arr) (\s -> rebindExpr tag (reifyExpr body) s)
  Ir.IrMethReduce arr z tagA tagB body ->
    MethReduce
      (reifyExpr arr)
      (reifyExpr z)
      (\a b -> rebindExpr2 tagA tagB (reifyExpr body) a b)
  Ir.IrMethReduceRight arr z tagA tagB body ->
    MethReduceRight
      (reifyExpr arr)
      (reifyExpr z)
      (\a b -> rebindExpr2 tagA tagB (reifyExpr body) a b)
  Ir.IrMethToSorted arr tagA tagB body ->
    MethToSorted
      (reifyExpr arr)
      (\a b -> rebindExpr2 tagA tagB (reifyExpr body) a b)
  Ir.IrMethFrom n tag body ->
    MethFrom (reifyExpr n) (\s -> rebindExpr tag (reifyExpr body) s)

lowerFieldLitAt :: Int -> FieldLit Stamp r -> (Int, Ir.IrFieldLit r)
lowerFieldLitAt t0 = \case
  FieldLit @k e ->
    let
      (t1, e') = lowerExprAt t0 e
     in
      (t1, Ir.IrFieldLit @k e')
  FieldLitEffect @k e ->
    let
      (t1, e') = lowerEffectAt t0 e
     in
      (t1, Ir.IrFieldLitEffect @k e')
  FieldLitExtra @k e ->
    let
      (t1, e') = lowerExprAt t0 e
     in
      (t1, Ir.IrFieldLitExtra @k e')
  FieldLitExtraEffect @k e ->
    let
      (t1, e') = lowerEffectAt t0 e
     in
      (t1, Ir.IrFieldLitExtraEffect @k e')

lowerFieldLitsAt ::
  Int -> [FieldLit Stamp r] -> (Int, [Ir.IrFieldLit r])
lowerFieldLitsAt t0 fs = goFieldLits t0 fs []
 where
  goFieldLits t [] acc = (t, reverse acc)
  goFieldLits t (fl : rest) acc =
    let
      (t1, fl') = lowerFieldLitAt t fl
     in
      goFieldLits t1 rest (fl' : acc)

lowerEffectsAt :: Int -> [Effect Stamp u] -> (Int, [Ir.IrEffect u])
lowerEffectsAt t0 es = goEffects t0 es []
 where
  goEffects t [] acc = (t, reverse acc)
  goEffects t (e : rest) acc =
    let
      (t1, e') = lowerEffectAt t e
     in
      goEffects t1 rest (e' : acc)

lowerEffectArmsAt ::
  Int -> [(Text, Effect Stamp u)] -> (Int, [(Text, Ir.IrEffect u)])
lowerEffectArmsAt t0 arms = goArms t0 arms []
 where
  goArms t [] acc = (t, reverse acc)
  goArms t ((k, e) : rest) acc =
    let
      (t1, e') = lowerEffectAt t e
     in
      goArms t1 rest ((k, e') : acc)

reifyFieldLitExtra ::
  forall u k r. (KnownSymbol k, Typeable u) => Ir.IrExpr u -> FieldLit Stamp r
reifyFieldLitExtra e = FieldLitExtra @k (reifyExpr e)

reifyFieldLitExtraEffect ::
  forall u k r. (KnownSymbol k, Typeable u) => Ir.IrEffect u -> FieldLit Stamp r
reifyFieldLitExtraEffect e = FieldLitExtraEffect @k (reifyEffect e)

reifyFieldLit :: forall r. Ir.IrFieldLit r -> FieldLit Stamp r
reifyFieldLit fl =
  unsafeCoerce $
    case fl of
      Ir.IrFieldLit @k e ->
        FieldLit @k (reifyExpr (unsafeCoerce e))
      Ir.IrFieldLitEffect @k e ->
        FieldLitEffect @k (reifyEffect (unsafeCoerce e))
      Ir.IrFieldLitExtra @k (e :: Ir.IrExpr u) ->
        reifyFieldLitExtra @u @k @r e
      Ir.IrFieldLitExtraEffect @k (e :: Ir.IrEffect u) ->
        reifyFieldLitExtraEffect @u @k @r e

irFnTags :: Ir.IrFnBody us r -> [Int]
irFnTags = \case
  Ir.IrJfNil _ -> []
  Ir.IrJfCons t k -> t : irFnTags k

irFnBodyExpr :: Ir.IrFnBody us r -> Ir.IrExpr r
irFnBodyExpr = \case
  Ir.IrJfNil e -> e
  Ir.IrJfCons _ k -> irFnBodyExpr k

lowerFnBody :: FnBody Stamp us r -> Ir.IrFnBody us r
lowerFnBody body = snd (lowerFnBodyAt (-2) body)

lowerFnBodyAt :: Int -> FnBody Stamp us r -> (Int, Ir.IrFnBody us r)
lowerFnBodyAt t0 body =
  let
    (tags, tEnd) = allocFnTags t0 body
   in
    (tEnd, lowerFnBodyTags tags body)

lowerFnBodyTags :: [Int] -> FnBody Stamp us r -> Ir.IrFnBody us r
lowerFnBodyTags tags = \case
  JfNil e -> Ir.IrJfNil (lowerExpr e)
  JfCons k ->
    case tags of
      t : ts -> Ir.IrJfCons t (lowerFnBodyTags ts (k (Name t)))
      _ -> error "JShark.lowerFnBodyTags: arity mismatch"

reifyFnBody :: Ir.IrFnBody us r -> FnBody Stamp us r
reifyFnBody ir =
  rebindFn (irFnTags ir) (reifyExpr (irFnBodyExpr ir))

lowerExpr :: Expr Stamp u -> Ir.IrExpr u
lowerExpr e = snd (lowerExprAt (-2) e)

lowerExprAt :: Int -> Expr Stamp u -> (Int, Ir.IrExpr u)
lowerExprAt t0 = \case
  Literal v -> (t0, Ir.IrLiteral v)
  Var (Stamp i) -> (t0, Ir.IrVar i)
  Var (Embed e) ->
    let
      (t1, e') = lowerExprAt t0 (flattenExpr e)
     in
      (t1, e')
  Var (EmbedEff e) ->
    let
      (t1, e') = lowerEffectAt t0 (flattenEff e)
     in
      (t1, Ir.IrEmbedEff e')
  Let x g ->
    let
      tag = t0
      tUnder = t0 - optStep
      (_, x') = lowerExprAt tUnder x
      (t2, body') = lowerExprAt tUnder (flattenExpr (g (Name tag)))
     in
      (t2, Ir.IrLet tag x' body')
  LetRec r b ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, r') = lowerExprAt tUnder (flattenExpr (r (Name tag)))
      (t2, b') = lowerExprAt t1 (flattenExpr (b (Name tag)))
     in
      (t2, Ir.IrLetRec tag r' b')
  Lambda g ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, body') = lowerExprAt tUnder (flattenExpr (g (Name tag)))
     in
      (t1, Ir.IrLambda tag body')
  Apply f x ->
    let
      (t1, f') = lowerExprAt t0 f
      (t2, x') = lowerExprAt t1 x
     in
      (t2, Ir.IrApply f' x')
  If c t e ->
    let
      (t1, c') = lowerExprAt t0 c
      (t2, t') = lowerExprAt t1 t
      (t3, e') = lowerExprAt t2 e
     in
      (t3, Ir.IrIf c' t' e')
  OptionCase o n s ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, o') = lowerExprAt tUnder o
      (t2, n') = lowerExprAt t1 n
      (t3, s') = lowerExprAt t2 (flattenExpr (s (Name tag)))
     in
      (t3, Ir.IrOptionCase o' n' tag s')
  ResultOk x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.IrResultOk x')
  ResultErr x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.IrResultErr x')
  ResultCase o er ok ->
    let
      tagE = t0
      t1 = t0 - optStep
      tagO = t1
      tUnder = t1 - optStep
      (t2, o') = lowerExprAt tUnder o
      (t3, er') = lowerExprAt t2 (flattenExpr (er (Name tagE)))
      (t4, ok') = lowerExprAt t3 (flattenExpr (ok (Name tagO)))
     in
      (t4, Ir.IrResultCase o' tagE er' tagO ok')
  Index arr idx ->
    let
      (t1, arr') = lowerExprAt t0 arr
      (t2, idx') = lowerExprAt t1 idx
     in
      (t2, Ir.IrIndex arr' idx')
  U8Index buf idx ->
    let
      (t1, buf') = lowerExprAt t0 buf
      (t2, idx') = lowerExprAt t1 idx
     in
      (t2, Ir.IrU8Index buf' idx')
  Error msg ->
    let
      (t1, msg') = lowerExprAt t0 msg
     in
      (t1, Ir.IrError msg')
  Std (Fixed op args) ->
    let
      (t1, args') = lowerFixedArgsAt t0 args
     in
      (t1, Ir.IrFixed op args')
  Std (Kernel k) ->
    let
      (t1, k') = lowerKernelKAt t0 k
     in
      (t1, Ir.IrKernelK k')
  Std (Method m) ->
    let
      (t1, m') = lowerStdMethodAt t0 m
     in
      (t1, Ir.IrMethod m')
  FnLit body ->
    let
      (t1, body') = lowerFnBodyAt t0 body
     in
      (t1, Ir.IrFnLit body')
  UnsafeNullable x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.IrUnsafeNullable x')
  FrozenLit fs ->
    let
      (t1, fs') = lowerFieldLitsAt t0 fs
     in
      (t1, Ir.IrFrozenLit fs')
  GetField @k o ->
    let
      (t1, o') = lowerExprAt t0 o
     in
      (t1, Ir.IrGetField @k o')
  Hvm2Kernel name _ ->
    (t0, Ir.IrHvm2Ref name)

reifyExpr :: Ir.IrExpr u -> Expr Stamp u
reifyExpr = \case
  Ir.IrLiteral v -> Literal v
  Ir.IrVar i -> Var (Name i)
  Ir.IrEmbedEff e -> Var (EmbedEff (reifyEffect e))
  Ir.IrLet tag x body ->
    Let (reifyExpr x) (\s -> rebindExpr tag (reifyExpr body) s)
  Ir.IrLetRec tag r b ->
    LetRec
      (\s -> rebindExpr tag (reifyExpr r) s)
      (\s -> rebindExpr tag (reifyExpr b) s)
  Ir.IrLambda tag body ->
    Lambda (\s -> rebindExpr tag (reifyExpr body) s)
  Ir.IrApply f x -> Apply (reifyExpr f) (reifyExpr x)
  Ir.IrIf c t e -> If (reifyExpr c) (reifyExpr t) (reifyExpr e)
  Ir.IrOptionCase o n tag s ->
    OptionCase (reifyExpr o) (reifyExpr n) (\x -> rebindExpr tag (reifyExpr s) x)
  Ir.IrResultOk x -> ResultOk (reifyExpr x)
  Ir.IrResultErr x -> ResultErr (reifyExpr x)
  Ir.IrResultCase o tagE er tagO ok ->
    ResultCase
      (reifyExpr o)
      (\x -> rebindExpr tagE (reifyExpr er) x)
      (\x -> rebindExpr tagO (reifyExpr ok) x)
  Ir.IrIndex arr idx -> Index (reifyExpr arr) (reifyExpr idx)
  Ir.IrU8Index buf idx -> U8Index (reifyExpr buf) (reifyExpr idx)
  Ir.IrError msg -> Error (reifyExpr msg)
  Ir.IrFixed op args -> Std (Fixed op (reifyFixedArgs args))
  Ir.IrKernelK k -> Std (Kernel (reifyKernelK k))
  Ir.IrMethod m -> Std (Method (reifyStdMethod m))
  Ir.IrFnLit body -> FnLit (reifyFnBody body)
  Ir.IrUnsafeNullable x -> UnsafeNullable (reifyExpr x)
  Ir.IrFrozenLit fs -> FrozenLit (map reifyFieldLit fs)
  Ir.IrGetField @k o -> GetField @k (reifyExpr o)
  Ir.IrHvm2Ref name ->
    error ("JShark.reifyExpr: IrHvm2Ref " <> T.unpack name)

lowerEffect :: Effect Stamp u -> Ir.IrEffect u
lowerEffect e = snd (lowerEffectAt (-2) e)

lowerEffectAt :: Int -> Effect Stamp u -> (Int, Ir.IrEffect u)
lowerEffectAt t0 = \case
  Lift x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.IrLift x')
  FFI n args ->
    let
      (t1, args') = lowerArgsAt t0 args
     in
      (t1, Ir.IrFFI n args')
  UnsafeObject o -> (t0, Ir.IrUnsafeObject o)
  UnsafeObjectGet x s ->
    let
      (t1, x') = lowerEffectAt t0 x
     in
      (t1, Ir.IrUnsafeObjectGet x' s)
  UnsafeObjectAssign x y ->
    let
      (t1, x') = lowerEffectAt t0 x
      (t2, y') = lowerEffectAt t1 y
     in
      (t2, Ir.IrUnsafeObjectAssign x' y')
  CallMethod x n args ->
    let
      (t1, x') = lowerEffectAt t0 x
      (t2, args') = lowerArgsAt t1 args
     in
      (t2, Ir.IrCallMethod x' n args')
  Bind x f ->
    let
      tag = t0
      tUnder = t0 - optStep
      (_, x') = lowerEffectAt tUnder x
      (t2, body') = lowerEffectAt tUnder (flattenEff (f (Name tag)))
     in
      (t2, Ir.IrBind tag x' body')
  ThenE x y ->
    let
      (t1, x') = lowerEffectAt t0 x
      (t2, y') = lowerEffectAt t1 y
     in
      (t2, Ir.IrThenE x' y')
  BindRec rhs body ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, r') = lowerEffectAt tUnder (flattenEff (rhs (Name tag)))
      (t2, b') = lowerEffectAt t1 (flattenEff (body (Name tag)))
     in
      (t2, Ir.IrBindRec tag r' b')
  LambdaE f ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, body') = lowerEffectAt tUnder (flattenEff (f (Name tag)))
     in
      (t1, Ir.IrLambdaE tag body')
  ApplyE f x ->
    let
      (t1, f') = lowerEffectAt t0 f
      (t2, x') = lowerEffectAt t1 x
     in
      (t2, Ir.IrApplyE f' x')
  IfE c t e ->
    let
      (t1, c') = lowerEffectAt t0 c
      (t2, t') = lowerEffectAt t1 t
      (t3, e') = lowerEffectAt t2 e
     in
      (t3, Ir.IrIfE c' t' e')
  While c b ->
    let
      (t1, c') = lowerEffectAt t0 c
      (t2, b') = lowerEffectAt t1 b
     in
      (t2, Ir.IrWhile c' b')
  ForRange s e f ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, s') = lowerExprAt tUnder s
      (t2, e') = lowerExprAt t1 e
      (t3, body') = lowerEffectAt t2 (flattenEff (f (Name tag)))
     in
      (t3, Ir.IrForRange s' e' tag body')
  U8Set b i v ->
    let
      (t1, b') = lowerExprAt t0 b
      (t2, i') = lowerExprAt t1 i
      (t3, v') = lowerExprAt t2 v
     in
      (t3, Ir.IrU8Set b' i' v')
  U8Fill b v ->
    let
      (t1, b') = lowerExprAt t0 b
      (t2, v') = lowerExprAt t1 v
     in
      (t2, Ir.IrU8Fill b' v')
  OptionCaseE o n s ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, o') = lowerExprAt tUnder o
      (t2, n') = lowerEffectAt t1 n
      (t3, s') = lowerEffectAt t2 (flattenEff (s (Name tag)))
     in
      (t3, Ir.IrOptionCaseE o' n' tag s')
  ResultCaseE o er ok ->
    let
      tagE = t0
      t1 = t0 - optStep
      tagO = t1
      tUnder = t1 - optStep
      (t2, o') = lowerExprAt tUnder o
      (t3, er') = lowerEffectAt t2 (flattenEff (er (Name tagE)))
      (t4, ok') = lowerEffectAt t3 (flattenEff (ok (Name tagO)))
     in
      (t4, Ir.IrResultCaseE o' tagE er' tagO ok')
  StringCaseE s arms d ->
    let
      (t1, s') = lowerExprAt t0 s
      (t2, arms') = lowerEffectArmsAt t1 arms
      (t3, d') = lowerEffectAt t2 d
     in
      (t3, Ir.IrStringCaseE s' arms' d')
  Throw x ->
    let
      (t1, x') = lowerExprAt t0 x
     in
      (t1, Ir.IrThrow x')
  Try a k ->
    let
      tag = t0
      tUnder = t0 - optStep
      (t1, a') = lowerEffectAt tUnder a
      (t2, k') = lowerEffectAt t1 (flattenEff (k (Name tag)))
     in
      (t2, Ir.IrTry a' tag k')
  ObjectLit fs ->
    let
      (t1, fs') = lowerFieldLitsAt t0 fs
     in
      (t1, Ir.IrObjectLit fs')
  DeleteProp o k ->
    let
      (t1, o') = lowerEffectAt t0 o
      (t2, k') = lowerExprAt t1 k
     in
      (t2, Ir.IrDeleteProp o' k')
  ArrayLit es ->
    let
      (t1, es') = lowerEffectsAt t0 es
     in
      (t1, Ir.IrArrayLit es')

reifyEffect :: Ir.IrEffect u -> Effect Stamp u
reifyEffect = \case
  Ir.IrLift x -> Lift (reifyExpr x)
  Ir.IrFFI n args -> FFI n (mapRec reifyArg args)
  Ir.IrUnsafeObject o -> UnsafeObject o
  Ir.IrUnsafeObjectGet x s -> UnsafeObjectGet (reifyEffect x) s
  Ir.IrUnsafeObjectAssign x y -> UnsafeObjectAssign (reifyEffect x) (reifyEffect y)
  Ir.IrCallMethod x n args -> CallMethod (reifyEffect x) n (mapRec reifyArg args)
  Ir.IrBind tag x body ->
    Bind (reifyEffect x) (\s -> rebindEff tag (reifyEffect body) s)
  Ir.IrThenE x y -> ThenE (reifyEffect x) (reifyEffect y)
  Ir.IrBindRec tag r b ->
    BindRec
      (\s -> rebindEff tag (reifyEffect r) s)
      (\s -> rebindEff tag (reifyEffect b) s)
  Ir.IrLambdaE tag body ->
    LambdaE (\s -> rebindEff tag (reifyEffect body) s)
  Ir.IrApplyE f x -> ApplyE (reifyEffect f) (reifyEffect x)
  Ir.IrIfE c t e -> IfE (reifyEffect c) (reifyEffect t) (reifyEffect e)
  Ir.IrWhile c b -> While (reifyEffect c) (reifyEffect b)
  Ir.IrForRange s e tag body ->
    ForRange (reifyExpr s) (reifyExpr e) (\i -> rebindEff tag (reifyEffect body) i)
  Ir.IrU8Set b i v -> U8Set (reifyExpr b) (reifyExpr i) (reifyExpr v)
  Ir.IrU8Fill b v -> U8Fill (reifyExpr b) (reifyExpr v)
  Ir.IrOptionCaseE o n tag s ->
    OptionCaseE
      (reifyExpr o)
      (reifyEffect n)
      (\x -> rebindEff tag (reifyEffect s) x)
  Ir.IrResultCaseE o tagE er tagO ok ->
    ResultCaseE
      (reifyExpr o)
      (\x -> rebindEff tagE (reifyEffect er) x)
      (\x -> rebindEff tagO (reifyEffect ok) x)
  Ir.IrStringCaseE s arms d ->
    StringCaseE (reifyExpr s) (map (fmap reifyEffect) arms) (reifyEffect d)
  Ir.IrThrow x -> Throw (reifyExpr x)
  Ir.IrTry a tag k ->
    Try (reifyEffect a) (\s -> rebindEff tag (reifyEffect k) s)
  Ir.IrObjectLit fs -> ObjectLit (map reifyFieldLit fs)
  Ir.IrDeleteProp o k -> DeleteProp (reifyEffect o) (reifyExpr k)
  Ir.IrArrayLit es -> ArrayLit (map reifyEffect es)

reifyArg :: Ir.IrArg u -> Arg Stamp u
reifyArg = \case
  Ir.IrArgExpr e -> ArgExpr (reifyExpr e)
  Ir.IrArgEffect e -> ArgEffect (reifyEffect e)

-- | Constant-fold and drop dead pure bindings. Applied automatically by
-- codegen. This is the End-algebra: a closed term is instantiated at
-- 'Stamp' for the name supply (Kmett: take the end, then interpret).
-- instantiating the 'ClosedExpr' once ('NOINLINE') before this walk.
optimize :: ClosedExpr u -> Expr Stamp u
optimize (e :: ClosedExpr u) =
  let
    (_, final, _) = optExpr (-2) (e :: Expr Stamp u)
   in
    flattenExpr final
{-# NOINLINE optimize #-}

-- | Legacy PHOAS round-trip: optimize via IR, then 'reifyEffect' back to
-- 'Effect Stamp'. Large programs use the flat pipeline instead; kept for
-- callers that still need an optimized 'Effect' tree.
optimizeEffectIr :: Effect Stamp u -> Effect Stamp u
optimizeEffectIr e =
  let
    (_, ir) = lowerEffectAt (-2) (flattenEff e)
    (_, irOpt, _) = Ir.optIrEffect (-2) ir
   in
    flattenEff (reifyEffect irOpt)
{-# NOINLINE optimizeEffectIr #-}

optimizeEffectTree :: ClosedEffect u -> Effect Stamp u
optimizeEffectTree (e :: ClosedEffect u) =
  if closedEffectNodes e >= optIrLargeThreshold
    then optimizeEffectIr (e :: Effect Stamp u)
    else
      let
        (_, final, _) = optEffect (-2) (e :: Effect Stamp u)
       in
        flattenEff final

optimizeEffect :: ClosedEffect u -> Effect Stamp u
optimizeEffect e = optimizeEffectTree e
{-# NOINLINE optimizeEffect #-}

irEffectFromClosed :: ClosedEffect u -> Ir.IrEffect u
irEffectFromClosed (e :: ClosedEffect u) =
  let
    (_, ir) = lowerEffectAt (-2) (flattenEff e)
    (_, irOpt, _) = Ir.optIrEffect (-2) ir
   in
    irOpt
{-# NOINLINE irEffectFromClosed #-}

irExprFromClosed :: ClosedExpr u -> Ir.IrExpr u
irExprFromClosed (e :: ClosedExpr u) =
  let
    (_, ir) = lowerExprAt (-2) (flattenExpr (e :: Expr Stamp u))
    (_, irOpt, _) = Ir.optIrExpr (-2) ir
   in
    irOpt
{-# NOINLINE irExprFromClosed #-}

irOptimizedExprFromClosed :: ClosedExpr u -> Ir.IrExpr u
irOptimizedExprFromClosed (e :: ClosedExpr u) =
  let
    (_, ir) = lowerExprAt (-2) (flattenExpr (optimize e))
    (_, irOpt, _) = Ir.optIrExpr (-2) ir
   in
    irOpt
{-# NOINLINE irOptimizedExprFromClosed #-}

irOptimizedEffectFromClosed :: ClosedEffect u -> Ir.IrEffect u
irOptimizedEffectFromClosed (e :: ClosedEffect u) =
  let
    (_, ir) = lowerEffectAt (-2) (flattenEff (optimizeEffect e))
    (_, irOpt, _) = Ir.optIrEffect (-2) ir
   in
    irOpt
{-# NOINLINE irOptimizedEffectFromClosed #-}

collectHvm2Kernels :: Expr f u -> [Hvm2KernelEntry]
collectHvm2Kernels expr = collectAny (unsafeCoerce expr :: Expr Stamp u)
 where
  collectAny :: Expr Stamp v -> [Hvm2KernelEntry]
  collectAny = \case
    Hvm2Kernel name k -> [Hvm2KernelEntry name k]
    Literal _ -> []
    Var _ -> []
    Let x g -> collectAny x <> collectAny (g nestedDummy)
    LetRec r b ->
      collectAny (r nestedDummy) <> collectAny (b nestedDummy)
    Lambda g -> collectAny (g nestedDummy)
    Apply f x -> collectAny f <> collectAny x
    If c t eF -> collectAny c <> collectAny t <> collectAny eF
    OptionCase o n s ->
      collectAny o <> collectAny n <> collectAny (s nestedDummy)
    ResultOk x -> collectAny x
    ResultErr x -> collectAny x
    ResultCase o er ok ->
      collectAny o
        <> collectAny (er nestedDummy)
        <> collectAny (ok nestedDummy)
    Index x i -> collectAny x <> collectAny i
    U8Index x i -> collectAny x <> collectAny i
    Error x -> collectAny x
    Std s -> collectStdHvm2 s
    FnLit body -> collectFnBodyHvm2 body
    UnsafeNullable x -> collectAny x
    FrozenLit fs -> concatMap collectFieldLitHvm2 fs
    GetField o -> collectAny o
  collectStdHvm2 = \case
    Fixed _ args -> collectFixedArgsHvm2 args
    Method m -> collectMethodHvm2 m
    Kernel k -> collectKernelHvm2 k
  collectFixedArgsHvm2 = \case
    ArgsU x -> collectAny x
    ArgsB x y -> collectAny x <> collectAny y
    ArgsT x y z -> collectAny x <> collectAny y <> collectAny z
  collectMethodHvm2 = \case
    MethMap x f -> collectAny x <> collectAny (f nestedDummy)
    MethFilter x f -> collectAny x <> collectAny (f nestedDummy)
    MethReduce x z _ -> collectAny x <> collectAny z
    MethReduceRight x z _ -> collectAny x <> collectAny z
    MethToSorted x _ -> collectAny x
    MethFrom n f -> collectAny n <> collectAny (f nestedDummy)
  collectKernelHvm2 = \case
    KPlus x y -> collectAny x <> collectAny y
    KTimes x y -> collectAny x <> collectAny y
    KMinus x y -> collectAny x <> collectAny y
    KNegate x -> collectAny x
    KFracDiv x y -> collectAny x <> collectAny y
    KRem x y -> collectAny x <> collectAny y
    KBitAnd x y -> collectAny x <> collectAny y
    KBitOr x y -> collectAny x <> collectAny y
    KBitXor x y -> collectAny x <> collectAny y
    KShl x y -> collectAny x <> collectAny y
    KShr x y -> collectAny x <> collectAny y
    KUShr x y -> collectAny x <> collectAny y
    KBig _ x y -> collectAny x <> collectAny y
    KBigNeg x -> collectAny x
    KConcat x y -> collectAny x <> collectAny y
    KShow x -> collectAny x
    KTypeOf x -> collectAny x
    KAnd x y -> collectAny x <> collectAny y
    KOr x y -> collectAny x <> collectAny y
    KEq _ x y -> collectAny x <> collectAny y
    KNEq _ x y -> collectAny x <> collectAny y
    KGTh x y -> collectAny x <> collectAny y
    KLTh x y -> collectAny x <> collectAny y
    KGTEq x y -> collectAny x <> collectAny y
    KLTEq x y -> collectAny x <> collectAny y
  collectFnBodyHvm2 :: FnBody Stamp us r -> [Hvm2KernelEntry]
  collectFnBodyHvm2 = \case
    JfNil e -> collectAny e
    JfCons k -> collectFnBodyHvm2 (k nestedDummy)
  collectFieldLitHvm2 = \case
    FieldLit e -> collectAny e
    FieldLitEffect e -> collectEffectAny e
    FieldLitExtra e -> collectAny e
    FieldLitExtraEffect e -> collectEffectAny e
  collectEffectAny :: Effect Stamp v -> [Hvm2KernelEntry]
  collectEffectAny = \case
    Lift x -> collectAny x
    FFI _ args -> collectRecArgs args
    Bind x f -> collectEffectAny x <> collectEffectAny (f nestedDummy)
    ThenE x y -> collectEffectAny x <> collectEffectAny y
    BindRec r b ->
      collectEffectAny (r nestedDummy) <> collectEffectAny (b nestedDummy)
    LambdaE f -> collectEffectAny (f nestedDummy)
    ApplyE f x -> collectEffectAny f <> collectEffectAny x
    IfE c u v -> collectEffectAny c <> collectEffectAny u <> collectEffectAny v
    While c b -> collectEffectAny c <> collectEffectAny b
    ForRange s e b ->
      collectAny s <> collectAny e <> collectEffectAny (b nestedDummy)
    U8Set b i v -> collectAny b <> collectAny i <> collectAny v
    U8Fill b v -> collectAny b <> collectAny v
    OptionCaseE o n s ->
      collectAny o <> collectEffectAny n <> collectEffectAny (s nestedDummy)
    ResultCaseE o er ok ->
      collectAny o
        <> collectEffectAny (er nestedDummy)
        <> collectEffectAny (ok nestedDummy)
    StringCaseE o arms d ->
      collectAny o
        <> concatMap (collectEffectAny . snd) arms
        <> collectEffectAny d
    Throw x -> collectAny x
    Try a k -> collectEffectAny a <> collectEffectAny (k nestedDummy)
    ObjectLit fs -> concatMap collectFieldLitHvm2 fs
    DeleteProp o k -> collectEffectAny o <> collectAny k
    ArrayLit es -> concatMap collectEffectAny es
    UnsafeObject {} -> []
    UnsafeObjectGet x _ -> collectEffectAny x
    UnsafeObjectAssign x y -> collectEffectAny x <> collectEffectAny y
    CallMethod x _ args -> collectEffectAny x <> collectRecArgs args
  collectRecArgs :: Rec (Arg Stamp) us -> [Hvm2KernelEntry]
  collectRecArgs = \case
    RecNil -> []
    RecCons a rest -> collectArgAny a <> collectRecArgs rest
  collectArgAny :: Arg Stamp v -> [Hvm2KernelEntry]
  collectArgAny = \case
    ArgExpr e -> collectAny (unsafeCoerce e :: Expr Stamp v)
    ArgEffect e -> collectEffectAny (unsafeCoerce e :: Effect Stamp v)

hvm2ExportRef :: Text -> JS
hvm2ExportRef name =
  let
    key = dquotes (jsString (escapeJsString (T.unpack name)))
    err =
      dquotes (jsString (escapeJsString ("HVM2 kernel not loaded: " ++ T.unpack name)))
  in
    "((function(){var f=globalThis.__jsharkHvm2?.exports?.["
      <> key
      <> "];if(typeof f===\"function\")return f;return function(){throw new Error("
      <> err
      <> ")};})())"

optimizedExprSize :: ClosedExpr u -> Int
optimizedExprSize (e :: ClosedExpr u) =
  let
    (_, ir) = lowerExprAt (-2) (flattenExpr (e :: Expr Stamp u))
    (_, _, md) = Ir.optIrExpr (-2) ir
   in
    Ir.irMetaSize md

optimizedEffectSize :: ClosedEffect u -> Int
optimizedEffectSize (e :: ClosedEffect u) =
  let
    (_, ir) = lowerEffectAt (-2) (flattenEff (e :: Effect Stamp u))
    (_, _, md) = Ir.optIrEffect (-2) ir
   in
    Ir.irMetaSize md

-- | Tags step by two, keeping the optimizer on the even negatives.
-- Codegen's 'allocTag' owns the odd ones, so neither can name a binder the
-- other is counting.
optStep :: Int
optStep = 2

optUnder ::
  Int -> (Stamp u -> Expr Stamp v) -> (Int, Int, Expr Stamp v, Metadata)
optUnder t0 f =
  let
    tag = t0
    (t1, body, md) = optExpr (t0 - optStep) (f (Stamp tag))
   in
    (t1, tag, body, md)

optUnderE ::
  Int -> (Stamp u -> Effect Stamp v) -> (Int, Int, Effect Stamp v, Metadata)
optUnderE t0 f =
  let
    tag = t0
    (t1, body, md) = optEffect (t0 - optStep) (f (Stamp tag))
   in
    (t1, tag, body, md)

optUnder2 ::
  Int
  -> (Stamp a -> Stamp b -> Expr Stamp v)
  -> (Int, Int, Int, Expr Stamp v, Metadata)
optUnder2 t0 f =
  let
    tA = t0
    tB = t0 - optStep
    (t1, body, md) = optExpr (t0 - 2 * optStep) (f (Stamp tA) (Stamp tB))
   in
    (t1, tA, tB, body, md)

isCheapValue :: Value u -> Bool
isCheapValue = \case
  ValueNumber {} -> True
  ValueBigInt {} -> True
  ValueString {} -> True
  ValueBool {} -> True
  ValueUnit -> True
  ValueOption Nothing -> True
  ValueOption (Just v) -> isCheapValue v
  ValueResult (Left v) -> isCheapValue v
  ValueResult (Right v) -> isCheapValue v
  ValueRegex {} -> False
  ValueUint8Array {} -> False
  ValueArray {} -> False
  ValueFunction {} -> False
  ValueFrozen {} -> False

isCheap :: Expr Stamp u -> Bool
isCheap = cheapExpr

isCheapFieldLit :: FieldLit Stamp r -> Bool
isCheapFieldLit = \case
  FieldLit e -> isCheap e
  FieldLitExtra e -> isCheap e
  FieldLitEffect {} -> False
  FieldLitExtraEffect {} -> False

isCheapEffect :: Effect Stamp u -> Bool
isCheapEffect = cheapEffect

class PhoasDummy f where
  phoasDummy :: f u
  isPureExpr_ :: Expr f u -> Bool
  isPureEffect_ :: Effect f u -> Bool

instance PhoasDummy Stamp where
  phoasDummy = nestedDummy
  isPureExpr_ = pureExpr
  isPureEffect_ = pureEffect

instance PhoasDummy Value where
  phoasDummy = error "JShark.phoasDummy: Value binder"
  isPureExpr_ _ = True
  isPureEffect_ _ = True

isPureExpr :: PhoasDummy f => Expr f u -> Bool
isPureExpr = isPureExpr_

isPureEffectStamp :: PhoasDummy f => Effect f u -> Bool
isPureEffectStamp = isPureEffect_

isPureEffect :: Effect Stamp u -> Bool
isPureEffect = pureEffect

optArgs :: Int -> Rec (Arg Stamp) us -> (Int, Rec (Arg Stamp) us, Metadata)
optArgs t0 RecNil = (t0, RecNil, mempty)
optArgs t0 (RecCons x xs) =
  let
    (t1, x', mdX) = optArg t0 x
    (t2, xs', mdXS) = optArgs t1 xs
   in
    (t2, RecCons x' xs', mdX <> mdXS)

optArg :: Int -> Arg Stamp u -> (Int, Arg Stamp u, Metadata)
optArg t (ArgExpr e) =
  let
    (t', e', md) = optExpr t e
   in
    (t', ArgExpr e', md)
optArg t (ArgEffect e) =
  let
    (t', e', md) = optEffect t e
   in
    (t', ArgEffect e', md)

foldNum1 ::
  (Double -> Double)
  -> (Expr Stamp 'Number -> Expr Stamp 'Number)
  -> Expr Stamp 'Number
  -> Expr Stamp 'Number
foldNum1 f k = \case
  Literal (ValueNumber a) -> Literal (ValueNumber (f a))
  x -> k x

foldNum2 ::
  (Double -> Double -> Double)
  -> (Expr Stamp 'Number -> Expr Stamp 'Number -> Expr Stamp 'Number)
  -> Expr Stamp 'Number
  -> Expr Stamp 'Number
  -> Expr Stamp 'Number
foldNum2 f k x y = case (x, y) of
  (Literal (ValueNumber a), Literal (ValueNumber b)) -> Literal (ValueNumber (f a b))
  _ -> k x y

foldConcat :: Expr Stamp 'String -> Expr Stamp 'String -> Expr Stamp 'String
foldConcat x y = case (x, y) of
  (Literal (ValueString a), Literal (ValueString b)) -> Literal (ValueString (a <> b))
  _ -> Concat x y

foldAnd :: Expr Stamp 'Bool -> Expr Stamp 'Bool -> Expr Stamp 'Bool
foldAnd x y = case (x, y) of
  (Literal (ValueBool False), _) -> Literal (ValueBool False)
  (Literal (ValueBool True), y') -> y'
  (_, Literal (ValueBool True)) -> x
  (x', Literal (ValueBool False)) | isPureExpr x' -> Literal (ValueBool False)
  _ -> And x y

foldOr :: Expr Stamp 'Bool -> Expr Stamp 'Bool -> Expr Stamp 'Bool
foldOr x y = case (x, y) of
  (Literal (ValueBool True), _) -> Literal (ValueBool True)
  (Literal (ValueBool False), y') -> y'
  (_, Literal (ValueBool False)) -> x
  (x', Literal (ValueBool True)) | isPureExpr x' -> Literal (ValueBool True)
  _ -> Or x y

foldCmp ::
  (Value u -> Value u -> Bool)
  -> (Value u -> Bool)
  -> (Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool)
  -> Expr Stamp u
  -> Expr Stamp u
  -> Expr Stamp 'Bool
foldCmp cmp ok k x y = case (x, y) of
  (Literal a, Literal b) | ok a && ok b -> Literal (ValueBool (cmp a b))
  _ -> k x y

foldEq :: Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool
foldEq = foldFrozenEq valueEq structuralEq

foldNEq :: Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool
foldNEq = foldFrozenEq (\a b -> not (valueEq a b)) structuralNEq

foldFrozenEq ::
  (forall a. Value a -> Value a -> Bool)
  -> (Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool)
  -> Expr Stamp u
  -> Expr Stamp u
  -> Expr Stamp 'Bool
foldFrozenEq cmp k x y = case (x, y) of
  (Literal a, Literal b)
    | eqFoldableValue a && eqFoldableValue b ->
        Literal (ValueBool (cmp a b))
  (FrozenLit as, FrozenLit bs)
    | Just as' <- peelFrozen as
    , Just bs' <- peelFrozen bs ->
        Literal (ValueBool (cmp (ValueFrozen as') (ValueFrozen bs')))
  _ -> k x y

peelFrozen :: [FieldLit Stamp r] -> Maybe [FieldLit Value r]
peelFrozen = traverse $ \case
  FieldLit @k e -> case e of
    Literal v -> Just (FieldLit @k (Literal v))
    _ -> Nothing
  FieldLitExtra @k e -> case e of
    Literal v -> Just (FieldLitExtra @k (Literal v))
    _ -> Nothing
  FieldLitEffect {} -> Nothing
  FieldLitExtraEffect {} -> Nothing

foldOrd ::
  Ordering
  -> (Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool)
  -> Expr Stamp u
  -> Expr Stamp u
  -> Expr Stamp 'Bool
foldOrd ord = foldCmp (\a b -> valueCompare a b == ord) isOrderableValue

foldOrdNeq ::
  Ordering
  -> (Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool)
  -> Expr Stamp u
  -> Expr Stamp u
  -> Expr Stamp 'Bool
foldOrdNeq ord = foldCmp (\a b -> valueCompare a b /= ord) isOrderableValue

foldShow :: Expr Stamp u -> Expr Stamp 'String
foldShow x = case x of
  Literal (ValueFunction _) -> Show x
  Literal v -> Literal (ValueString (jsShow v))
  _ -> Show x

foldTypeOf :: Expr Stamp u -> Expr Stamp 'String
foldTypeOf x = case x of
  Literal v -> Literal (ValueString (typeOfValue v))
  _ -> TypeOf x

foldIndex :: Expr Stamp ('Array u) -> Expr Stamp 'Number -> Expr Stamp u
foldIndex arr idx = case (arr, idx) of
  (Index {}, _) -> Index arr idx
  (Literal (ValueArray vs), Literal (ValueNumber d))
    | isFiniteDouble d
    , let
        i = truncate d :: Int
    , i >= 0 && i < length vs ->
        Literal (vs !! i)
  _ -> Index arr idx

foldFixedUnary ::
  FixedOp Number 'Unit 'Unit Number -> Expr Stamp 'Number -> Expr Stamp 'Number
foldFixedUnary n x = case x of
  Literal (ValueNumber a)
    | Just r <- Prim.exactMathUnary n a -> Literal (ValueNumber r)
  _ -> expr1 n x

foldFixedBinary ::
  FixedOp 'Number 'Number 'Unit 'Number
  -> Expr Stamp 'Number
  -> Expr Stamp 'Number
  -> Expr Stamp 'Number
foldFixedBinary n x y = case (x, y) of
  (Literal (ValueNumber a), Literal (ValueNumber b))
    | Just r <- Prim.exactMathBinary n a b -> Literal (ValueNumber r)
  _ -> expr2 n x y

foldArrLen :: Expr Stamp ('Array u) -> Expr Stamp 'Number
foldArrLen x = case x of
  Literal (ValueArray vs) ->
    Literal (ValueNumber (fromIntegral (Prelude.length vs)))
  _ -> expr1 FixArrLen x

foldToBigInt :: Expr Stamp 'Number -> Expr Stamp 'BigInt
foldToBigInt x = case x of
  Literal (ValueNumber d)
    | isFiniteDouble d
    , let
        n = truncate d
    , d == fromInteger n ->
        Literal (ValueBigInt n)
  _ -> expr1 FixToBigInt x

foldFromBigInt :: Expr Stamp 'BigInt -> Expr Stamp 'Number
foldFromBigInt x = case x of
  Literal (ValueBigInt n) -> Literal (ValueNumber (fromInteger n))
  _ -> expr1 FixFromBigInt x

foldParseBigInt :: Expr Stamp 'String -> Expr Stamp 'BigInt
foldParseBigInt x = case x of
  Literal (ValueString s)
    | Just n <- parseBigIntString (T.unpack s) ->
        Literal (ValueBigInt n)
  _ -> expr1 FixParseBigInt x

foldBig ::
  BigBinOp
  -> Expr Stamp 'BigInt
  -> Expr Stamp 'BigInt
  -> Expr Stamp 'BigInt
foldBig op x y = case (x, y) of
  (Literal (ValueBigInt a), Literal (ValueBigInt b))
    | Just r <- tryEvalBigBin op a b ->
        Literal (ValueBigInt r)
  _ -> Std (Kernel (KBig op x y))

foldBigNeg :: Expr Stamp 'BigInt -> Expr Stamp 'BigInt
foldBigNeg x = case x of
  Literal (ValueBigInt n) -> Literal (ValueBigInt (negate n))
  _ -> Std (Kernel (KBigNeg x))

optFixed ::
  Int
  -> FixedOp a b c u
  -> FixedArgs Stamp a b c
  -> (Int, Expr Stamp u, Metadata)
optFixed t0 op args = case (op, args) of
  (n, ArgsU x)
    | Just (MathUnary n') <- matchMathUnary n ->
        let
          (t1, x', mdX) = optExpr t0 x
          res = foldFixedUnary n' x'
          md = Metadata 1 True (isCheap res) <> mdX
         in
          (t1, res, md)
  (n, ArgsB x y)
    | Just (MathBinary n') <- matchMathBinary n ->
        let
          (t1, x', mdX) = optExpr t0 x
          (t2, y', mdY) = optExpr t1 y
          res = foldFixedBinary n' x' y'
          md = Metadata 1 True (isCheap res) <> mdX <> mdY
         in
          (t2, res, md)
  (FixArrLen, ArgsU x) ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldArrLen x'
      md = Metadata 1 True (isCheap res) <> mdX
     in
      (t1, res, md)
  (FixToBigInt, ArgsU x) ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldToBigInt x'
      md = Metadata 1 True (isCheap res) <> mdX
     in
      (t1, res, md)
  (FixFromBigInt, ArgsU x) ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldFromBigInt x'
      md = Metadata 1 True (isCheap res) <> mdX
     in
      (t1, res, md)
  (FixParseBigInt, ArgsU x) ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldParseBigInt x'
      md = Metadata 1 True (isCheap res) <> mdX
     in
      (t1, res, md)
  (n, ArgsU x) ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = expr1 n x'
      md = Metadata 1 (isPureFixed n) (isCheap res) <> mdX
     in
      (t1, res, md)
  (n, ArgsB x y) ->
    let
      (t1, x', mdX) = optExpr t0 x
      (t2, y', mdY) = optExpr t1 y
      res = expr2 n x' y'
      md = Metadata 1 (isPureFixed n) (isCheap res) <> mdX <> mdY
     in
      (t2, res, md)
  (n, ArgsT x y z) ->
    let
      (t1, x', mdX) = optExpr t0 x
      (t2, y', mdY) = optExpr t1 y
      (t3, z', mdZ) = optExpr t2 z
      res = expr3 n x' y' z'
      md = Metadata 1 True (isCheap res) <> mdX <> mdY <> mdZ
     in
      (t3, res, md)

optLet ::
  Int
  -> Expr Stamp u
  -> (Stamp u -> Expr Stamp v)
  -> (Int, Expr Stamp v, Metadata)
optLet t0 x f =
  let
    (t1, x', mdX) = optExpr t0 x
    (t2, tag, body, mdBody) = optUnder t1 f
   in
    elimLetFrom t2 x' mdX f tag body mdBody

-- Count uses on the already-optimized body. Large tails keep that
-- body (rename-only reopen). Small @f@ may still be applied once more
-- so nested lets / optionCase peel fold.
data ElimOps src body = ElimOps
  { elimCount :: Int -> body -> Metadata -> Int
  , elimPure :: Metadata -> Bool
  , elimCheap :: Metadata -> Bool
  , elimSize :: Metadata -> Int
  , elimRebuild :: body -> body
  , elimSplice :: Int -> (Int, body, Metadata)
  , elimDropUnused :: Metadata -> Bool
  , elimOccurs :: Int -> body -> Bool
  }

elimFrom ::
  ElimOps src body
  -> Int
  -> Metadata
  -> Int
  -> body
  -> Metadata
  -> (Int, body, Metadata)
elimFrom ops t mdX tag body mdBody =
  let
    uses = elimCount ops tag body mdBody
    kept = elimRebuild ops body
    inlined
      | elimSize ops mdBody > optSmall = (t, kept, mdBody)
      | otherwise = elimSplice ops t
   in
    case uses of
      0
        | elimPure ops mdX
        , elimDropUnused ops mdX
        , not (elimOccurs ops tag body) ->
            (t, body, mdBody)
      0 -> (t, kept, mdBody)
      1 -> inlined
      _ | elimCheap ops mdX -> inlined
      _ -> (t, kept, mdBody)

elimLetFrom ::
  Int
  -> Expr Stamp u
  -> Metadata
  -> (Stamp u -> Expr Stamp v)
  -> Int
  -> Expr Stamp v
  -> Metadata
  -> (Int, Expr Stamp v, Metadata)
elimLetFrom t x mdX f tag body mdBody =
  elimFrom
    ElimOps
      { elimCount = elimExprUses
      , elimPure = mdIsPure
      , elimCheap = mdIsCheap
      , elimSize = \_ -> nodeCountExpr body
      , elimRebuild = Let x . rebindExpr tag
      , elimSplice = \t' -> optExpr t' (inlineExpr f x)
      , elimDropUnused = const True
      , elimOccurs = occursVarInExpr
      }
    t
    mdX
    tag
    body
    mdBody

optBind ::
  Int
  -> Effect Stamp u
  -> (Stamp u -> Effect Stamp v)
  -> (Int, Effect Stamp v, Metadata)
optBind t0 x f =
  let
    (t1, x', mdX) = optEffect t0 x
    (t2, tag, body, mdBody) = optUnderE t1 f
   in
    elimBindFrom t2 x' mdX f tag body mdBody

elimBindFrom ::
  Int
  -> Effect Stamp u
  -> Metadata
  -> (Stamp u -> Effect Stamp v)
  -> Int
  -> Effect Stamp v
  -> Metadata
  -> (Int, Effect Stamp v, Metadata)
elimBindFrom t x mdX f tag body mdBody =
  elimFrom
    ElimOps
      { elimCount = elimEffUses
      , elimPure = const (isPureEffect x)
      , elimCheap = mdIsCheap
      , elimSize = \_ -> nodeCountEff body
      , elimRebuild = Bind x . rebindEff tag
      , elimSplice = \t' -> optEffect t' (inlineEff f x)
      , elimDropUnused = \_ -> not (isAliasBind x)
      , elimOccurs = occursVarInEff
      }
    t
    mdX
    tag
    body
    mdBody

optBin ::
  Int
  -> (Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool)
  -> Expr Stamp u
  -> Expr Stamp u
  -> (Int, Expr Stamp 'Bool, Metadata)
optBin t0 k x y =
  let
    (t1, x', mdX) = optExpr t0 x
    (t2, y', mdY) = optExpr t1 y
   in
    (t2, k x' y', Metadata 1 True False <> mdX <> mdY)

optBinNum ::
  Int
  -> (Double -> Double -> Double)
  -> (Expr Stamp 'Number -> Expr Stamp 'Number -> Expr Stamp 'Number)
  -> Expr Stamp 'Number
  -> Expr Stamp 'Number
  -> (Int, Expr Stamp 'Number, Metadata)
optBinNum t0 f k x y =
  let
    (t1, x', mdX) = optExpr t0 x
    (t2, y', mdY) = optExpr t1 y
    res = foldNum2 f k x' y'
   in
    (t2, res, Metadata 1 True (isCheap res) <> mdX <> mdY)

optUnNum ::
  Int
  -> (Double -> Double)
  -> (Expr Stamp 'Number -> Expr Stamp 'Number)
  -> Expr Stamp 'Number
  -> (Int, Expr Stamp 'Number, Metadata)
optUnNum t0 f k x =
  let
    (t1, x', mdX) = optExpr t0 x
    res = foldNum1 f k x'
   in
    (t1, res, Metadata 1 True (isCheap res) <> mdX)

optExpr :: Int -> Expr Stamp u -> (Int, Expr Stamp u, Metadata)
optExpr t0 = \case
  Literal v -> (t0, Literal v, Metadata 1 True (isCheapValue v))
  Var (Embed e) -> optExpr t0 (flattenExpr e)
  Var (EmbedEff (Lift e)) -> optExpr t0 (flattenExpr e)
  Var (EmbedEff e) ->
    let
      (t1, e', md) = optEffect t0 e
     in
      case e' of
        Lift x -> (t1, x, md)
        _ -> (t1, Var (EmbedEff e'), md)
  Var (Stamp i) -> (t0, Var (Stamp i), Metadata 1 True False)
  Let x f -> optLet t0 x f
  LetRec r b ->
    let
      tag = t0
      (t1, r', mdR) = optExpr (t0 - optStep) (r (Stamp tag))
      (t2, b', mdB) = optExpr t1 (b (Stamp tag))
      res = LetRec (keepExprCont t2 tag r' mdR r) (keepExprCont t2 tag b' mdB b)
      md = Metadata 1 True False <> mdR <> mdB
     in
      (t2, res, md)
  Lambda f ->
    let
      (t1, tag, body, mdBody) = optUnder t0 f
      res = Lambda (keepExprCont t1 tag body mdBody f)
      md = Metadata 1 True False <> mdBody
     in
      (t1, res, md)
  Apply f x ->
    let
      (t1, f', mdF) = optExpr t0 f
      (t2, x', mdX) = optExpr t1 x
     in
      case f' of
        Lambda g -> optLet t2 x' g
        _ -> (t2, Apply f' x', Metadata 1 True False <> mdF <> mdX)
  If c t e ->
    let
      (t1, c', mdC) = optExpr t0 c
     in
      case c' of
        Literal (ValueBool True) -> optExpr t1 t
        Literal (ValueBool False) -> optExpr t1 e
        _ ->
          let
            (t2, t', mdT) = optExpr t1 t
            (t3, e', mdE) = optExpr t2 e
            md = Metadata 1 True False <> mdC <> mdT <> mdE
           in
            (t3, If c' t' e', md)
  OptionCase o n s ->
    let
      (t1, o', mdO) = optExpr t0 o
     in
      case peelOption o' of
        Just Nothing -> optExpr t1 n
        Just (Just x) ->
          let
            (t2, tag, body, mdBody) = optUnder t1 s
           in
            elimLetFrom t2 x mdO s tag body mdBody
        Nothing ->
          let
            (t2, n', mdN) = optExpr t1 n
            (t3, tag, body, mdBody) = optUnder t2 s
            md = Metadata 1 True False <> mdO <> mdN <> mdBody
           in
            (t3, OptionCase o' n' (keepExprCont t3 tag body mdBody s), md)
  ResultOk x ->
    let
      (t1, x', mdX) = optExpr t0 x
     in
      (t1, ResultOk x', Metadata 1 True False <> mdX)
  ResultErr x ->
    let
      (t1, x', mdX) = optExpr t0 x
     in
      (t1, ResultErr x', Metadata 1 True False <> mdX)
  ResultCase o e s ->
    let
      (t1, o', mdO) = optExpr t0 o
     in
      case peelResult o' of
        Just (Left x) ->
          let
            (t2, tag, body, mdBody) = optUnder t1 e
           in
            elimLetFrom t2 x mdO e tag body mdBody
        Just (Right x) ->
          let
            (t2, tag, body, mdBody) = optUnder t1 s
           in
            elimLetFrom t2 x mdO s tag body mdBody
        Nothing ->
          let
            (t2, tE, e', mdE) = optUnder t1 e
            (t3, tS, s', mdS) = optUnder t2 s
            md = Metadata 1 True False <> mdO <> mdE <> mdS
           in
            ( t3
            , ResultCase o' (keepExprCont t3 tE e' mdE e) (keepExprCont t3 tS s' mdS s)
            , md
            )
  Index arr idx ->
    let
      (t1, arr', mdA) = optExpr t0 arr
      (t2, idx', mdI) = optExpr t1 idx
      res = foldIndex arr' idx'
      md = Metadata 1 True (isCheap res) <> mdA <> mdI
     in
      (t2, res, md)
  U8Index buf idx ->
    let
      (t1, buf', mdB) = optExpr t0 buf
      (t2, idx', mdI) = optExpr t1 idx
      md = Metadata 1 True False <> mdB <> mdI
     in
      (t2, U8Index buf' idx', md)
  Error x ->
    let
      (t1, x', mdX) = optExpr t0 x
     in
      (t1, Error x', Metadata 1 True False <> mdX)
  Std s -> optStd t0 s
  FnLit body ->
    let
      (t1, tags, expr', mdExpr, body0) = optUnderFn t0 body
      res = FnLit (keepFnCont tags expr' body0)
      md = Metadata 1 True False <> mdExpr
     in
      (t1, res, md)
  UnsafeNullable x ->
    let
      (t1, x', mdX) = optExpr t0 x
     in
      (t1, UnsafeNullable x', Metadata 1 True (isCheap x') <> mdX)
  FrozenLit fs ->
    let
      (t1, fs', mdFS) = mapAccumField t0 fs
     in
      (t1, FrozenLit fs', Metadata 1 (fieldsPure fs') False <> mdFS)
  GetField @k o ->
    let
      (t1, o', mdO) = optExpr t0 o
     in
      case foldGetField @k o' of
        Just e -> optExpr t1 e
        Nothing -> (t1, GetField @k o', Metadata 1 True False <> mdO)
  Hvm2Kernel name k ->
    (t0, Hvm2Kernel name k, Metadata 1 True False)

optMapped ::
  ( Expr Stamp ('Array u)
    -> (Stamp u -> Expr Stamp b)
    -> Expr Stamp c
  )
  -> Int
  -> Expr Stamp ('Array u)
  -> (Stamp u -> Expr Stamp b)
  -> (Int, Expr Stamp c, Metadata)
optMapped k t0 x f =
  let
    (t1, x', mdX) = optExpr t0 x
    (t2, tag, body, mdBody) = optUnder t1 f
    md = Metadata 1 True False <> mdX <> mdBody
   in
    (t2, k x' (keepExprCont t2 tag body mdBody f), md)

optReduced ::
  ( Expr Stamp ('Array u)
    -> Expr Stamp v
    -> (Stamp v -> Stamp u -> Expr Stamp v)
    -> Expr Stamp v
  )
  -> Int
  -> Expr Stamp ('Array u)
  -> Expr Stamp v
  -> (Stamp v -> Stamp u -> Expr Stamp v)
  -> (Int, Expr Stamp v, Metadata)
optReduced k t0 x z f =
  let
    (t1, x', mdX) = optExpr t0 x
    (t2, z', mdZ) = optExpr t1 z
    (t3, tA, tB, body, mdBody) = optUnder2 t2 f
    md = Metadata 1 True False <> mdX <> mdZ <> mdBody
   in
    (t3, k x' z' (keepExprCont2 t3 tA tB body mdBody f), md)

optToSorted ::
  ( Expr Stamp ('Array u)
    -> (Stamp u -> Stamp u -> Expr Stamp 'Number)
    -> Expr Stamp ('Array u)
  )
  -> Int
  -> Expr Stamp ('Array u)
  -> (Stamp u -> Stamp u -> Expr Stamp 'Number)
  -> (Int, Expr Stamp ('Array u), Metadata)
optToSorted k t0 x f =
  let
    (t1, x', mdX) = optExpr t0 x
    (t2, tA, tB, body, mdBody) = optUnder2 t1 f
    md = Metadata 1 True False <> mdX <> mdBody
   in
    (t2, k x' (keepExprCont2 t2 tA tB body mdBody f), md)

optStd :: Int -> Std Stamp u -> (Int, Expr Stamp u, Metadata)
optStd t0 = \case
  Fixed op args -> optFixed t0 op args
  Method m -> optMethod t0 m
  Kernel k -> optKernel t0 k

optKernel :: Int -> Kernel Stamp u -> (Int, Expr Stamp u, Metadata)
optKernel t0 = \case
  KPlus x y -> optBinNum t0 (+) Plus x y
  KTimes x y -> optBinNum t0 (*) Times x y
  KMinus x y -> optBinNum t0 (-) Minus x y
  KFracDiv x y -> optBinNum t0 (/) FracDiv x y
  KRem x y -> optBinNum t0 jsRem Rem x y
  KBitAnd x y -> optBinNum t0 (jsBit2 (.&.)) BitAnd x y
  KBitOr x y -> optBinNum t0 (jsBit2 (.|.)) BitOr x y
  KBitXor x y -> optBinNum t0 (jsBit2 xor) BitXor x y
  KShl x y -> optBinNum t0 jsShl Shl x y
  KShr x y -> optBinNum t0 jsShr Shr x y
  KUShr x y -> optBinNum t0 jsUShr UShr x y
  KNegate x -> optUnNum t0 negate Negate x
  KBig op x y ->
    let
      (t1, x', mdX) = optExpr t0 x
      (t2, y', mdY) = optExpr t1 y
      res = foldBig op x' y'
      md = Metadata 1 True (isCheap res) <> mdX <> mdY
     in
      (t2, res, md)
  KBigNeg x ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldBigNeg x'
      md = Metadata 1 True (isCheap res) <> mdX
     in
      (t1, res, md)
  KConcat x y ->
    let
      (t1, x', mdX) = optExpr t0 x
      (t2, y', mdY) = optExpr t1 y
      res = foldConcat x' y'
      md = Metadata 1 True (isCheap res) <> mdX <> mdY
     in
      (t2, res, md)
  KShow x ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldShow x'
      md = Metadata 1 True (isCheap res) <> mdX
     in
      (t1, res, md)
  KTypeOf x ->
    let
      (t1, x', mdX) = optExpr t0 x
      res = foldTypeOf x'
      md = Metadata 1 True (isCheap res) <> mdX
     in
      (t1, res, md)
  KAnd x y ->
    let
      (t1, x', mdX) = optExpr t0 x
     in
      case x' of
        Literal (ValueBool False) -> (t1, Literal (ValueBool False), Metadata 1 True True <> mdX)
        Literal (ValueBool True) -> optExpr t1 y
        _ ->
          let
            (t2, y', mdY) = optExpr t1 y
            res = foldAnd x' y'
            md = Metadata 1 True (isCheap res) <> mdX <> mdY
           in
            (t2, res, md)
  KOr x y ->
    let
      (t1, x', mdX) = optExpr t0 x
     in
      case x' of
        Literal (ValueBool True) -> (t1, Literal (ValueBool True), Metadata 1 True True <> mdX)
        Literal (ValueBool False) -> optExpr t1 y
        _ ->
          let
            (t2, y', mdY) = optExpr t1 y
            res = foldOr x' y'
            md = Metadata 1 True (isCheap res) <> mdX <> mdY
           in
            (t2, res, md)
  KEq structural x y ->
    optBin t0 (foldFrozenEq valueEq (\a b -> Std (Kernel (KEq structural a b)))) x y
  KNEq structural x y ->
    optBin
      t0
      ( foldFrozenEq
          (\a b -> not (valueEq a b))
          (\a b -> Std (Kernel (KNEq structural a b)))
      )
      x
      y
  KGTh x y -> optBin t0 (foldOrd GT GTh) x y
  KLTh x y -> optBin t0 (foldOrd LT LTh) x y
  KGTEq x y -> optBin t0 (foldOrdNeq LT GTEq) x y
  KLTEq x y -> optBin t0 (foldOrdNeq GT LTEq) x y

optMethod :: Int -> Method Stamp u -> (Int, Expr Stamp u, Metadata)
optMethod t0 = \case
  MethMap x f -> optMapped (\a g -> Std (Method (MethMap a g))) t0 x f
  MethFilter x f -> optMapped (\a g -> Std (Method (MethFilter a g))) t0 x f
  MethReduce x z f -> optReduced (\a b g -> Std (Method (MethReduce a b g))) t0 x z f
  MethReduceRight x z f -> optReduced (\a b g -> Std (Method (MethReduceRight a b g))) t0 x z f
  MethToSorted x f -> optToSorted (\a g -> Std (Method (MethToSorted a g))) t0 x f
  MethFrom n f ->
    let
      (t1, n', mdN) = optExpr t0 n
      (t2, tag, body, mdBody) = optUnder t1 f
      md = Metadata 1 True False <> mdN <> mdBody
     in
      (t2, Std (Method (MethFrom n' (keepExprCont t2 tag body mdBody f))), md)

optEffect :: Int -> Effect Stamp u -> (Int, Effect Stamp u, Metadata)
optEffect t0 = \case
  Lift x ->
    let
      (t1, x', mdX) = optExpr t0 x
     in
      case x' of
        Var (EmbedEff e) -> optEffect t1 e
        _ -> (t1, Lift x', Metadata 1 True (isCheap x') <> mdX)
  FFI n args ->
    let
      (t1, args', md) = optArgs t0 args
     in
      (t1, FFI n args', Metadata 1 False False <> md)
  UnsafeObject o -> (t0, UnsafeObject o, Metadata 1 False False)
  UnsafeObjectGet x s ->
    let
      (t1, x', mdX) = optEffect t0 x
     in
      (t1, UnsafeObjectGet x' s, Metadata 1 False False <> mdX)
  UnsafeObjectAssign x y ->
    let
      (t1, x', mdX) = optEffect t0 x
      (t2, y', mdY) = optEffect t1 y
     in
      (t2, UnsafeObjectAssign x' y', Metadata 1 False False <> mdX <> mdY)
  CallMethod x n args ->
    let
      (t1, x', mdX) = optEffect t0 x
      (t2, args', mdA) = optArgs t1 args
     in
      (t2, CallMethod x' n args', Metadata 1 False False <> mdX <> mdA)
  Bind x f -> optBind t0 x f
  ThenE x y ->
    let
      (t1, x', mdX) = optEffect t0 x
      (t2, y', mdY) = optEffect t1 y
     in
      (t2, ThenE x' y', Metadata 1 (mdIsPure mdX && mdIsPure mdY) False <> mdX <> mdY)
  BindRec r b ->
    let
      tag = t0
      (t1, r', mdR) = optEffect (t0 - optStep) (r (Stamp tag))
      (t2, b', mdB) = optEffect t1 (b (Stamp tag))
      res = BindRec (keepEffCont t2 tag r' mdR r) (keepEffCont t2 tag b' mdB b)
      md = Metadata 1 False False <> mdR <> mdB
     in
      (t2, res, md)
  LambdaE f ->
    let
      (t1, tag, body, mdBody) = optUnderE t0 f
      res = LambdaE (keepEffCont t1 tag body mdBody f)
      md = Metadata 1 True False <> mdBody
     in
      (t1, res, md)
  ApplyE f x ->
    let
      (t1, f', mdF) = optEffect t0 f
      (t2, x', mdX) = optEffect t1 x
     in
      case f' of
        LambdaE g -> optBind t2 x' g
        _ -> (t2, ApplyE f' x', Metadata 1 False False <> mdF <> mdX)
  IfE c t e ->
    let
      (t1, c', mdC) = optEffect t0 c
     in
      case peelBoolEffect c' of
        Just True -> optEffect t1 t
        Just False -> optEffect t1 e
        Nothing ->
          let
            (t2, t', mdT) = optEffect t1 t
            (t3, e', mdE) = optEffect t2 e
            md =
              Metadata 1 (mdIsPure mdC && mdIsPure mdT && mdIsPure mdE) False
                <> mdC
                <> mdT
                <> mdE
           in
            (t3, IfE c' t' e', md)
  While c b ->
    let
      (t1, c', mdC) = optEffect t0 c
     in
      case peelBoolEffect c' of
        Just False -> (t1, Lift (Literal ValueUnit), Metadata 1 True True <> mdC)
        _ ->
          let
            (t2, b', mdB) = optEffect t1 b
            md = Metadata 1 False False <> mdC <> mdB
           in
            (t2, While c' b', md)
  ForRange s e b ->
    let
      (t1, s', mdS) = optExpr t0 s
      (t2, e', mdE) = optExpr t1 e
      (t3, tag, body, mdBody) = optUnderE t2 b
      md = Metadata 1 False False <> mdS <> mdE <> mdBody
     in
      (t3, ForRange s' e' (keepEffCont t3 tag body mdBody b), md)
  U8Set b i v ->
    let
      (t1, b', mdB) = optExpr t0 b
      (t2, i', mdI) = optExpr t1 i
      (t3, v', mdV) = optExpr t2 v
      md = Metadata 1 False False <> mdB <> mdI <> mdV
     in
      (t3, U8Set b' i' v', md)
  U8Fill b v ->
    let
      (t1, b', mdB) = optExpr t0 b
      (t2, v', mdV) = optExpr t1 v
      md = Metadata 1 False False <> mdB <> mdV
     in
      (t2, U8Fill b' v', md)
  OptionCaseE o n s ->
    let
      (t1, o', mdO) = optExpr t0 o
     in
      case peelOption o' of
        Just Nothing -> optEffect t1 n
        Just (Just x) ->
          let
            (t2, tag, body, mdBody) = optUnderE t1 s
           in
            elimBindFrom t2 (Lift x) mdO s tag body mdBody
        Nothing ->
          let
            (t2, n', mdN) = optEffect t1 n
            (t3, tag, body, mdBody) = optUnderE t2 s
            md =
              Metadata 1 (mdIsPure mdO && mdIsPure mdN && mdIsPure mdBody) False
                <> mdO
                <> mdN
                <> mdBody
           in
            (t3, OptionCaseE o' n' (keepEffCont t3 tag body mdBody s), md)
  ResultCaseE o e s ->
    let
      (t1, o', mdO) = optExpr t0 o
     in
      case peelResult o' of
        Just (Left x) ->
          let
            (t2, tag, body, mdBody) = optUnderE t1 e
           in
            elimBindFrom t2 (Lift x) mdO e tag body mdBody
        Just (Right x) ->
          let
            (t2, tag, body, mdBody) = optUnderE t1 s
           in
            elimBindFrom t2 (Lift x) mdO s tag body mdBody
        Nothing ->
          let
            (t2, tE, e', mdE) = optUnderE t1 e
            (t3, tS, s', mdS) = optUnderE t2 s
            md =
              Metadata 1 (mdIsPure mdO && mdIsPure mdE && mdIsPure mdS) False
                <> mdO
                <> mdE
                <> mdS
           in
            ( t3
            , ResultCaseE o' (keepEffCont t3 tE e' mdE e) (keepEffCont t3 tS s' mdS s)
            , md
            )
  StringCaseE o arms d ->
    let
      (t1, o', mdO) = optExpr t0 o
     in
      case peelString o' of
        Just k -> optEffect t1 (fromMaybe d (lookup k arms))
        Nothing ->
          let
            (t2, arms', mdArms) = mapAccumArms t1 arms
            (t3, d', mdD) = optEffect t2 d
            md = Metadata 1 False False <> mdO <> mdArms <> mdD
           in
            (t3, StringCaseE o' arms' d', md)
  Throw x ->
    let
      (t1, x', mdX) = optExpr t0 x
     in
      (t1, Throw x', Metadata 1 False False <> mdX)
  Try a k ->
    let
      (t1, a', mdA) = optEffect t0 a
      (t2, tag, body, mdBody) = optUnderE t1 k
      md = Metadata 1 False False <> mdA <> mdBody
     in
      (t2, Try a' (keepEffCont t2 tag body mdBody k), md)
  ObjectLit fs ->
    let
      (t1, fs', mdFS) = mapAccumField t0 fs
     in
      (t1, ObjectLit fs', Metadata 1 False False <> mdFS)
  DeleteProp o k ->
    let
      (t1, o', mdO) = optEffect t0 o
      (t2, k', mdK) = optExpr t1 k
      md = Metadata 1 False False <> mdO <> mdK
     in
      (t2, DeleteProp o' k', md)
  ArrayLit es ->
    let
      (t1, es', mdEs) = mapAccumEffs t0 es
     in
      (t1, ArrayLit es', Metadata 1 False False <> mdEs)

mapAccumField ::
  forall r. Int -> [FieldLit Stamp r] -> (Int, [FieldLit Stamp r], Metadata)
mapAccumField t0 fs =
  let
    (t1, res) = mapAccumL step t0 fs
   in
    (t1, map fst res, mconcat (map snd res))
 where
  step :: Int -> FieldLit Stamp r -> (Int, (FieldLit Stamp r, Metadata))
  step t = \case
    FieldLit @k e ->
      let (t', e', md) = optExpr t e in (t', (FieldLit @k e', md))
    FieldLitEffect @k e ->
      let (t', e', md) = optEffect t e in (t', (FieldLitEffect @k e', md))
    FieldLitExtra @k e ->
      let (t', e', md) = optExpr t e in (t', (FieldLitExtra @k e', md))
    FieldLitExtraEffect @k e ->
      let (t', e', md) = optEffect t e in (t', (FieldLitExtraEffect @k e', md))

mapAccumEffs :: Int -> [Effect Stamp u] -> (Int, [Effect Stamp u], Metadata)
mapAccumEffs t0 es =
  let
    (t1, res) = mapAccumL step t0 es
   in
    (t1, map fst res, mconcat (map snd res))
 where
  step t e = let (t', e', md) = optEffect t e in (t', (e', md))

mapAccumArms ::
  Int -> [(Text, Effect Stamp u)] -> (Int, [(Text, Effect Stamp u)], Metadata)
mapAccumArms t0 arms =
  let
    (t1, res) = mapAccumL step t0 arms
   in
    (t1, map fst res, mconcat (map snd res))
 where
  step t (k, e) = let (t', e', md) = optEffect t e in (t', ((k, e'), md))

-- | Sequencing without a binder ('ThenE' / discarded bind).
seqEffectCode ::
  Env -> CG -> Effect Stamp u -> Effect Stamp v -> (CG, Code)
seqEffectCode env s0 x y =
  let
    (s1, MkCode xDecl xRef xFX) = effectfulAST' env s0 x
    (s2, MkCode yDecl yRef yFX) = effectfulAST' env s1 y
    -- Value-producing effects (ifE) put work in xDecl and leave a result
    -- ident in xRef (codeRefFX False). Assignments and calls keep the
    -- side effect in xRef (fxCode).
    stmt
      | isNothing xRef = fromMaybe mempty xDecl
      | not xFX && isJust xDecl = fromMaybe mempty xDecl
      | otherwise = asStmt xDecl xRef
   in
    (s2, MkCode (Just (stmt $$ fromMaybe mempty yDecl)) yRef yFX)

-- Bind of an Effect: when the continuation uses the binder once in a
-- strict position, splice the effect in place (so `x <- getEl; x.foo()`
-- becomes `getEl().foo()`); when never, keep it as a statement.
-- Apply `f` once at the optimizer tag; map that tag to the emitted ident
-- via `env` instead of `renameEff` (which copied the whole continuation).
bindEffectCode ::
  Env -> CG -> Effect Stamp u -> (Stamp u -> Effect Stamp v) -> (CG, Code)
bindEffectCode env s0 x f =
  let
    (sProbe, tagged, probeTag) = probeContEff s0 f
    (bTag, used) = bindProbeTag probeTag tagged
    (s1, MkCode xDecl xRef xFX) = effectfulAST' env sProbe x
    stmtX
      | isNothing xRef = fromMaybe mempty xDecl
      | not xFX && isJust xDecl = fromMaybe mempty xDecl
      | otherwise = asStmt xDecl xRef
    insertBinder env0 n =
      IM.insert bTag n $ if bTag == probeTag then env0 else IM.insert probeTag n env0
   in
    case xRef of
      Nothing ->
        if not used
          then
            let
              (s2, MkCode yDecl yRef yFX) = effectfulAST' env s1 tagged
             in
              (s2, MkCode (Just (stmtX $$ fromMaybe mempty yDecl)) yRef yFX)
          else
            let
              n = fromMaybe nestedDummyId (liveBinder x)
              (env', sBind, bindJS) =
                if n /= nestedDummyId
                  then (insertBinder env n, s1, mempty)
                  else
                    let
                      (nBind, s2) = allocIdent s1
                     in
                      (insertBinder env nBind, s2, mempty)
              (s3, MkCode yDecl yRef yFX) = effectfulAST' env' sBind tagged
             in
              ( s3
              , MkCode (Just (stmtX $$ bindJS $$ fromMaybe mempty yDecl)) yRef yFX
              )
      Just _ ->
        if not used
          then
            let
              (s2, MkCode yDecl yRef yFX) = effectfulAST' env s1 tagged
             in
              (s2, MkCode (Just (stmtX $$ fromMaybe mempty yDecl)) yRef yFX)
          else
            let
              (nBind, s2) = allocIdent s1
              env' = insertBinder env nBind
              (s3, MkCode yDecl yRef yFX) = effectfulAST' env' s2 tagged
             in
              ( s3
              , MkCode
                  ( Just
                      ( fromMaybe mempty xDecl
                          $$ constBind nBind (fromMaybe mempty xRef)
                          $$ fromMaybe mempty yDecl
                      )
                  )
                  yRef
                  yFX
              )

-- Flat codegen ('flatPureAST'' / 'flatEffectfulAST'') mirrors the PHOAS
-- emitters above; keep in sync — 'irParityTests' diff the two paths.
flatRenderLiteral ::
  Env -> CG -> Value u -> (CG, Code)
flatRenderLiteral env s0 = \case
  ValueNumber d -> (s0, Code mempty (jsDouble d))
  ValueBigInt n -> (s0, Code mempty (jsBigIntLit n))
  ValueArray xs ->
    let
      (s1, exprs) =
        mapAccumL (\s x -> flatRenderLiteral env s x) s0 xs
     in
      ( s1
      , Code
          (codesDecls exprs)
          (brackets (hcat (punctuate ", " (codesRefs exprs))))
      )
  ValueString s -> (s0, Code mempty (jsQuote s))
  ValueFunction _ -> error "JShark.flatPureAST: ValueFunction is eval-only"
  ValueUnit -> (s0, mempty)
  ValueOption (Just x) -> flatRenderLiteral env s0 x
  ValueOption Nothing -> (s0, Code mempty "null")
  ValueResult (Right x) -> renderResultLit True s0 x
  ValueResult (Left x) -> renderResultLit False s0 x
  ValueRegex s ->
    (s0, Code mempty ("new RegExp" <> parens (jsQuote s)))
  ValueUint8Array ba -> (s0, Code mempty (jsUint8ArrayLit ba))
  ValueBool True -> (s0, Code mempty "true")
  ValueBool False -> (s0, Code mempty "false")
  ValueFrozen {} -> error "JShark.flatPureAST: ValueFrozen is eval-only"

flatIsUnitExpr :: Flat.FlatProgram -> Flat.NodeId -> Bool
flatIsUnitExpr prog nid = case Flat.flatNode prog nid of
  Flat.FE_Literal li ->
    case Flat.flatLitValue prog li of
      ValueUnit -> True
      _ -> False
  Flat.FE_Var{} -> False
  _ -> False

flatIsUnitEffect :: Flat.FlatProgram -> Flat.NodeId -> Bool
flatIsUnitEffect prog nid = case Flat.flatNode prog nid of
  Flat.FX_Lift eid -> flatIsUnitExpr prog eid
  Flat.FX_While _ _ -> True
  Flat.FX_ForRange _ _ _ _ -> True
  Flat.FX_Throw _ -> True
  Flat.FX_ThenE x y ->
    flatIsUnitEffect prog x && flatIsUnitEffect prog y
  Flat.FX_Bind _ _ y -> flatIsUnitEffect prog y
  Flat.FX_BindRec _ _ y -> flatIsUnitEffect prog y
  Flat.FX_IfE _ t e -> flatIsUnitEffect prog t && flatIsUnitEffect prog e
  Flat.FX_OptionCaseE _ n _ s ->
    flatIsUnitEffect prog n && flatIsUnitEffect prog s
  Flat.FX_ResultCaseE _ _ er _ ok ->
    flatIsUnitEffect prog er && flatIsUnitEffect prog ok
  Flat.FX_StringCaseE _ ai d ->
    all (flatIsUnitEffect prog . snd) (Flat.flatStrCases prog ai)
      && flatIsUnitEffect prog d
  Flat.FX_Try a _ k -> flatIsUnitEffect prog a && flatIsUnitEffect prog k
  _ -> False

flatIsSimpleEffectNode :: Flat.FlatProgram -> Flat.NodeId -> Bool
flatIsSimpleEffectNode prog nid = case Flat.flatNode prog nid of
  Flat.FX_Lift eid -> flatIsSimpleNode prog eid
  Flat.FX_FFI{} -> True
  Flat.FX_CallMethod{} -> True
  Flat.FX_UnsafeObject{} -> True
  Flat.FX_UnsafeObjectGet{} -> True
  Flat.FX_ArrayLit es -> all (flatIsSimpleEffectNode prog) es
  _ -> False

flatIsSimpleNode :: Flat.FlatProgram -> Flat.NodeId -> Bool
flatIsSimpleNode prog nid = case Flat.flatNode prog nid of
  Flat.FE_Literal _ -> True
  Flat.FE_Var _ -> True
  Flat.FE_EmbedEff eid -> flatIsSimpleEffectNode prog eid
  Flat.FE_KShow _ -> True
  Flat.FE_KTypeOf _ -> True
  Flat.FE_KNegate _ -> True
  Flat.FE_KBigNeg _ -> True
  Flat.FE_KConcat{} -> False
  Flat.FE_KPlus{} -> False
  Flat.FE_KTimes{} -> False
  Flat.FE_KMinus{} -> False
  Flat.FE_KFracDiv{} -> False
  Flat.FE_KRem{} -> False
  Flat.FE_KBitAnd{} -> False
  Flat.FE_KBitOr{} -> False
  Flat.FE_KBitXor{} -> False
  Flat.FE_KShl{} -> False
  Flat.FE_KShr{} -> False
  Flat.FE_KUShr{} -> False
  Flat.FE_KBig{} -> False
  Flat.FE_KAnd{} -> False
  Flat.FE_KOr{} -> False
  Flat.FE_KEq{} -> False
  Flat.FE_KNEq{} -> False
  Flat.FE_KGTh{} -> False
  Flat.FE_KLTh{} -> False
  Flat.FE_KGTEq{} -> False
  Flat.FE_KLTEq{} -> False
  Flat.FE_Fixed{} -> True
  Flat.FE_MethMap{} -> False
  Flat.FE_MethFilter{} -> False
  Flat.FE_MethReduce{} -> False
  Flat.FE_MethReduceRight{} -> False
  Flat.FE_MethToSorted{} -> False
  Flat.FE_MethFrom{} -> False
  Flat.FE_FnLit{} -> True
  Flat.FE_Index{} -> True
  Flat.FE_U8Index{} -> True
  Flat.FE_Error{} -> False
  Flat.FE_UnsafeNullable x -> flatIsSimpleNode prog x
  Flat.FE_FrozenLit{} -> True
  Flat.FE_Hvm2Ref{} -> True
  Flat.FE_GetField{} -> True
  _ -> False

flatWrapOperand :: Flat.FlatProgram -> Flat.NodeId -> JS -> JS
flatWrapOperand prog nid d =
  if flatIsSimpleNode prog nid then d else parens d

flatRenderBin ::
  Env
  -> Text
  -> CG
  -> Flat.FlatProgram
  -> Flat.NodeId
  -> Flat.NodeId
  -> (CG, Code)
flatRenderBin env op s0 prog xId yId =
  let
    (s1, Code xDecl xRef) = flatPureAST' env s0 prog xId
    (s2, Code yDecl yRef) = flatPureAST' env s1 prog yId
   in
    ( s2
    , Code
        (xDecl $$ yDecl)
        ( flatWrapOperand prog xId xRef
            <+> jsText op
            <+> flatWrapOperand prog yId yRef
        )
    )

flatRenderArgList ::
  Env -> CG -> Flat.FlatProgram -> Int -> (CG, JS, JS)
flatRenderArgList env s0 prog ai =
  let
    args = Flat.flatArgGroup prog ai
    go s = \case
      [] -> (s, [])
      Flat.FlatArgExpr eid : rest ->
        let
          (s', c) = flatPureAST' env s prog eid
          (s'', cs') = go s' rest
         in
          (s'', c : cs')
      Flat.FlatArgEffect eid : rest ->
        let
          (s', c) = flatEffectfulAST' env s prog eid
          (s'', cs') = go s' rest
         in
          (s'', c : cs')
    (s1, cs) = go s0 args
   in
    (s1, codesDecls cs, hcat (punctuate ", " (codesRefs cs)))

flatRenderField ::
  Env -> Flat.FlatProgram -> CG -> Flat.FlatField -> (CG, (JS, JS))
flatRenderField env prog s = \case
  Flat.FlatField k eid ->
    let
      (s', Code d r) = flatPureAST' env s prog eid
     in
      (s', (d, (dquotes (jsText k) <> ":") <+> r))
  Flat.FlatFieldExtra k eid ->
    let
      (s', Code d r) = flatPureAST' env s prog eid
     in
      (s', (d, (dquotes (jsText k) <> ":") <+> r))
  Flat.FlatFieldEff k eid ->
    let
      (s', MkCode d r _) = flatEffectfulAST' env s prog eid
     in
      ( s'
      , ( fromMaybe mempty d
        , (dquotes (jsText k) <> ":") <+> fromMaybe mempty r
        )
      )
  Flat.FlatFieldExtraEff k eid ->
    let
      (s', MkCode d r _) = flatEffectfulAST' env s prog eid
     in
      ( s'
      , ( fromMaybe mempty d
        , (dquotes (jsText k) <> ":") <+> fromMaybe mempty r
        )
      )

flatRenderObjectLit ::
  Env -> CG -> Flat.FlatProgram -> Int -> (CG, Code)
flatRenderObjectLit env s0 prog gi =
  let
    fs = Flat.flatFieldGroup prog gi
    (s1, parts) = mapAccumL (flatRenderField env prog) s0 fs
    (declList, pairs) = unzip parts
   in
    (s1, Code (vcatNonEmpty declList) (braces (hcat (punctuate ", " pairs))))

flatRenderArrayLit ::
  Env -> CG -> Flat.FlatProgram -> [Flat.NodeId] -> (CG, Code)
flatRenderArrayLit env s0 prog es =
  let
    (s1, cs) = mapAccumL (\s e -> flatEffectfulAST' env s prog e) s0 es
   in
    ( s1
    , Code
        (codesDecls cs)
        (brackets (hcat (punctuate ", " (codesRefs cs))))
    )

flatRenderFixed ::
  Env -> CG -> Flat.FlatProgram -> Flat.FlatFixed -> (CG, Code)
flatRenderFixed env s0 prog = \case
  Flat.FlatFixedU op xId
    | Just name <- Prim.math1Name op ->
        let
          (s1, Code xDecl xRef) = flatPureAST' env s0 prog xId
         in
          (s1, Code xDecl ("Math." <> jsText name <> parens xRef))
  Flat.FlatFixedB op xId yId
    | Just name <- Prim.math2Name op ->
        let
          (s1, Code xDecl xRef) = flatPureAST' env s0 prog xId
          (s2, Code yDecl yRef) = flatPureAST' env s1 prog yId
         in
          ( s2
          , Code
              (xDecl $$ yDecl)
              ( "Math."
                  <> jsText name
                  <> parens (xRef <> ", " <> yRef)
              )
          )
  Flat.FlatFixedU op xId ->
    let
      (s1, Code rDecl rRef) = flatPureAST' env s0 prog xId
     in
      (s1, Code rDecl (Prim.fixedUnaryJS op (flatWrapOperand prog xId rRef)))
  Flat.FlatFixedB op xId yId ->
    let
      (s1, Code rDecl rRef) = flatPureAST' env s0 prog xId
      (s2, Code aDecl aRef) = flatPureAST' env s1 prog yId
     in
      ( s2
      , Code
          (rDecl $$ aDecl)
          (Prim.fixedBinaryJS op (flatWrapOperand prog xId rRef) aRef)
      )
  Flat.FlatFixedT op xId yId zId ->
    let
      (s1, Code rDecl rRef) = flatPureAST' env s0 prog xId
      (s2, Code aDecl aRef) = flatPureAST' env s1 prog yId
      (s3, Code bDecl bRef) = flatPureAST' env s2 prog zId
     in
      ( s3
      , Code
          (rDecl $$ aDecl $$ bDecl)
          ( Prim.fixedTernaryJS
              op
              (flatWrapOperand prog xId rRef)
              aRef
              bRef
          )
      )

flatRenderKernel ::
  Env -> CG -> Flat.FlatProgram -> Flat.FlatNode -> (CG, Code)
flatRenderKernel env s0 prog = \case
  Flat.FE_KConcat x y -> flatRenderBin env "+" s0 prog x y
  Flat.FE_KPlus x y -> flatRenderBin env "+" s0 prog x y
  Flat.FE_KMinus x y -> flatRenderBin env "-" s0 prog x y
  Flat.FE_KTimes x y -> flatRenderBin env "*" s0 prog x y
  Flat.FE_KFracDiv x y -> flatRenderBin env "/" s0 prog x y
  Flat.FE_KRem x y -> flatRenderBin env "%" s0 prog x y
  Flat.FE_KBitAnd x y -> flatRenderBin env "&" s0 prog x y
  Flat.FE_KBitOr x y -> flatRenderBin env "|" s0 prog x y
  Flat.FE_KBitXor x y -> flatRenderBin env "^" s0 prog x y
  Flat.FE_KShl x y -> flatRenderBin env "<<" s0 prog x y
  Flat.FE_KShr x y -> flatRenderBin env ">>" s0 prog x y
  Flat.FE_KUShr x y -> flatRenderBin env ">>>" s0 prog x y
  Flat.FE_KBig op x y -> flatRenderBin env (bigOpJS op) s0 prog x y
  Flat.FE_KBigNeg x ->
    let
      (s1, Code xDecl xRef) = flatPureAST' env s0 prog x
     in
      (s1, Code xDecl $ "-" <> parens xRef)
  Flat.FE_KShow x ->
    let
      (s1, Code xDecl xRef) = flatPureAST' env s0 prog x
     in
      (s1, Code xDecl $ "String" <> parens xRef)
  Flat.FE_KTypeOf x ->
    let
      (s1, Code xDecl xRef) = flatPureAST' env s0 prog x
     in
      (s1, Code xDecl $ "typeof" <+> xRef)
  Flat.FE_KNegate x ->
    let
      (s1, Code xDecl xRef) = flatPureAST' env s0 prog x
     in
      (s1, Code xDecl $ "-" <> parens xRef)
  Flat.FE_KAnd x y -> flatRenderBin env "&&" s0 prog x y
  Flat.FE_KOr x y -> flatRenderBin env "||" s0 prog x y
  Flat.FE_KEq structural x y
    | structural ->
        let
          s1 = useEqHelpers s0
          (s2, Code xDecl xRef) = flatPureAST' env s1 prog x
          (s3, Code yDecl yRef) = flatPureAST' env s2 prog y
         in
          (s3, Code (xDecl $$ yDecl) (jsValueEq (flatWrapOperand prog x xRef) (flatWrapOperand prog y yRef)))
    | otherwise ->
        flatRenderBin env "===" s0 prog x y
  Flat.FE_KNEq structural x y
    | structural ->
        let
          s1 = useEqHelpers s0
          (s2, Code xDecl xRef) = flatPureAST' env s1 prog x
          (s3, Code yDecl yRef) = flatPureAST' env s2 prog y
         in
          (s3, Code (xDecl $$ yDecl) (jsValueNEq (flatWrapOperand prog x xRef) (flatWrapOperand prog y yRef)))
    | otherwise ->
        flatRenderBin env "!==" s0 prog x y
  Flat.FE_KGTh x y -> flatRenderBin env ">" s0 prog x y
  Flat.FE_KLTh x y -> flatRenderBin env "<" s0 prog x y
  Flat.FE_KGTEq x y -> flatRenderBin env ">=" s0 prog x y
  Flat.FE_KLTEq x y -> flatRenderBin env "<=" s0 prog x y
  _ -> error "JShark.flatRenderKernel: unexpected node"

flatRenderCallbackMethod ::
  Env
  -> String
  -> CG
  -> Flat.FlatProgram
  -> Flat.NodeId
  -> Int
  -> Flat.NodeId
  -> (CG, Code)
flatRenderCallbackMethod env name s0 prog arrId tag bodyId =
  let
    (s1, Code rDecl rRef) = flatPureAST' env s0 prog arrId
    (nParam, s2) = allocIdent s1
    env' = IM.insert tag nParam env
    (s3, Code exDecl exRef) = flatPureAST' env' s2 prog bodyId
    call =
      flatWrapOperand prog arrId rRef
        <> "."
        <> jsString name
        <> parens (jsCallback [nJS nParam] exDecl exRef)
   in
    (s3, Code rDecl call)

flatRenderFold ::
  Env
  -> String
  -> CG
  -> Flat.FlatProgram
  -> Flat.NodeId
  -> Flat.NodeId
  -> Int
  -> Int
  -> Flat.NodeId
  -> (CG, Code)
flatRenderFold env method s0 prog arrId zId tagA tagB bodyId =
  let
    (s1, Code rDecl rRef) = flatPureAST' env s0 prog arrId
    (s2, Code zDecl zRef) = flatPureAST' env s1 prog zId
    (nAcc, s3) = allocIdent s2
    (nElem, s4) = allocIdent s3
    env' = IM.insert tagA nAcc $ IM.insert tagB nElem env
    (s5, Code exDecl exRef) = flatPureAST' env' s4 prog bodyId
    cb = jsCallback [nJS nAcc, nJS nElem] exDecl exRef
    call = flatWrapOperand prog arrId rRef <> jsString method <> parens (cb <> ", " <> zRef)
   in
    (s5, Code (rDecl $$ zDecl) call)

flatRenderMethod ::
  Env -> CG -> Flat.FlatProgram -> Flat.FlatNode -> (CG, Code)
flatRenderMethod env s0 prog = \case
  Flat.FE_MethMap arr tag body ->
    flatRenderCallbackMethod env "map" s0 prog arr tag body
  Flat.FE_MethFilter arr tag body ->
    flatRenderCallbackMethod env "filter" s0 prog arr tag body
  Flat.FE_MethReduce arr z tagA tagB body ->
    flatRenderFold env ".reduce" s0 prog arr z tagA tagB body
  Flat.FE_MethReduceRight arr z tagA tagB body ->
    flatRenderFold env ".reduceRight" s0 prog arr z tagA tagB body
  Flat.FE_MethToSorted arr tagA tagB body ->
    let
      (s1, Code rDecl rRef) = flatPureAST' env s0 prog arr
      (nA, s2) = allocIdent s1
      (nB, s3) = allocIdent s2
      env' = IM.insert tagA nA $ IM.insert tagB nB env
      (s4, Code exDecl exRef) = flatPureAST' env' s3 prog body
      cb = jsCallback [nJS nA, nJS nB] exDecl exRef
     in
      (s4, Code rDecl (flatWrapOperand prog arr rRef <> ".toSorted" <> parens cb))
  Flat.FE_MethFrom n tag body ->
    let
      (s1, Code nDecl nRef) = flatPureAST' env s0 prog n
      (nHole, s2) = allocIdent s1
      (nI, s3) = allocIdent s2
      env' = IM.insert tag nI env
      (s4, Code exDecl exRef) = flatPureAST' env' s3 prog body
      cb = jsCallback [nJS nHole, nJS nI] exDecl exRef
     in
      (s4, Code nDecl ("Array.from({length: " <> nRef <> "}, " <> cb <> ")"))
  _ -> error "JShark.flatRenderMethod: unexpected node"

flatRenderFnLit ::
  Env -> CG -> Flat.FlatProgram -> [Int] -> Flat.NodeId -> (CG, Code)
flatRenderFnLit env s0 prog tags bodyId =
  let
    (ids, s1) = allocNIdents s0 (length tags)
    env' = foldr (\(tag, n) -> IM.insert tag n) env (zip tags ids)
    (s2, Code d r) = flatPureAST' env' s1 prog bodyId
   in
    (s2, Code mempty (jsCallback (map nJS ids) d r))

flatRenderResultCase ::
  Env
  -> CG
  -> Flat.FlatProgram
  -> Flat.NodeId
  -> Int
  -> Flat.NodeId
  -> Int
  -> Flat.NodeId
  -> (CG, Code)
flatRenderResultCase env s0 prog resId tagE errId tagO okId =
  let
    (s1, MkCode rDecl rRef _) = flatPureAST' env s0 prog resId
    (nObj, s2) = allocIdent s1
    (nUnw, s3) = allocIdent s2
    obj = nName nObj
    prelude =
      fromMaybe mempty rDecl
        $$ constBind nObj (fromMaybe mempty rRef)
        $$ constBind nUnw (jsText obj <> ".value")
    envE = IM.insert tagE nUnw env
    envO = IM.insert tagO nUnw envE
    (s4, Code eDecl eRef) = flatPureAST' envE s3 prog errId
    (s5, Code oDecl oRef) = flatPureAST' envO s4 prog okId
   in
    ( s5
    , Code
        (prelude $$ eDecl $$ oDecl)
        (parens ((jsText obj <> ".ok") <+> "?" <+> oRef <+> ":" <+> eRef))
    )

flatSeqEffect ::
  Env
  -> CG
  -> Flat.FlatProgram
  -> Flat.NodeId
  -> Flat.NodeId
  -> (CG, Code)
flatSeqEffect env s0 prog xId yId =
  let
    (s1, MkCode xDecl xRef xFX) = flatEffectfulAST' env s0 prog xId
    (s2, MkCode yDecl yRef yFX) = flatEffectfulAST' env s1 prog yId
    stmt
      | isNothing xRef = fromMaybe mempty xDecl
      | not xFX && isJust xDecl = fromMaybe mempty xDecl
      | otherwise = asStmt xDecl xRef
   in
    (s2, MkCode (Just (stmt $$ fromMaybe mempty yDecl)) yRef yFX)

flatBindEffect ::
  Env
  -> CG
  -> Flat.FlatProgram
  -> Int
  -> Flat.NodeId
  -> Flat.NodeId
  -> (CG, Code)
flatBindEffect env s0 prog tag xId bodyId =
  let
    (s1, MkCode xDecl xRef xFX) = flatEffectfulAST' env s0 prog xId
    (nBind, s2) = allocIdent s1
    env' = IM.insert tag nBind env
    (s3, MkCode yDecl yRef yFX) = flatEffectfulAST' env' s2 prog bodyId
    stmtX
      | isNothing xRef = fromMaybe mempty xDecl
      | not xFX && isJust xDecl = fromMaybe mempty xDecl
      | otherwise = asStmt xDecl xRef
   in
    case xRef of
      Nothing ->
        (s3, MkCode (Just (stmtX $$ fromMaybe mempty yDecl)) yRef yFX)
      Just _ ->
        ( s3
        , MkCode
            ( Just
                ( fromMaybe mempty xDecl
                    $$ constBind nBind (fromMaybe mempty xRef)
                    $$ fromMaybe mempty yDecl
                )
            )
            yRef
            yFX
        )

flatRenderResultCaseE ::
  Env
  -> CG
  -> Flat.FlatProgram
  -> Flat.NodeId
  -> Int
  -> Flat.NodeId
  -> Int
  -> Flat.NodeId
  -> (CG, Code)
flatRenderResultCaseE env s0 prog resId tagE errId tagO okId =
  if flatIsUnitEffect prog errId && flatIsUnitEffect prog okId
    then
      let
        (s1, MkCode rDecl rRef _) = flatPureAST' env s0 prog resId
        (nObj, s2) = allocIdent s1
        (nUnw, s3) = allocIdent s2
        obj = nName nObj
        prelude =
          fromMaybe mempty rDecl
            $$ constBind nObj (fromMaybe mempty rRef)
            $$ constBind nUnw (jsText obj <> ".value")
        envE = IM.insert tagE nUnw env
        envO = IM.insert tagO nUnw envE
        (s4, MkCode eDecl eRef _) = flatEffectfulAST' envE s3 prog errId
        (s5, MkCode oDecl oRef _) = flatEffectfulAST' envO s4 prog okId
       in
        ( s5
        , Code
            (prelude $$ ifElseStmt (jsText obj <> ".ok") oDecl oRef eDecl eRef)
            mempty
        )
    else
      let
        (s1, MkCode rDecl rRef _) = flatPureAST' env s0 prog resId
        (nObj, s2) = allocIdent s1
        (nUnw, s3) = allocIdent s2
        obj = nName nObj
        prelude =
          fromMaybe mempty rDecl
            $$ constBind nObj (fromMaybe mempty rRef)
            $$ constBind nUnw (jsText obj <> ".value")
        (resultN, s4) = allocIdent s3
        resultVar = nName resultN
        envE = IM.insert tagE nUnw env
        envO = IM.insert tagO nUnw envE
        (s5, MkCode eDecl eRef _) = flatEffectfulAST' envE s4 prog errId
        (s6, MkCode oDecl oRef _) = flatEffectfulAST' envO s5 prog okId
        stmt =
          prelude
            $$ letResult resultVar
            $$ ifElseStmt
              (jsText obj <> ".ok")
              (Just (fromMaybe mempty oDecl $$ assignResult resultVar oRef))
              Nothing
              (Just (fromMaybe mempty eDecl $$ assignResult resultVar eRef))
              Nothing
       in
        (s6, Code stmt (jsText resultVar))

flatRenderStringCaseE ::
  Env
  -> CG
  -> Flat.FlatProgram
  -> Flat.NodeId
  -> Int
  -> Flat.NodeId
  -> (CG, Code)
flatRenderStringCaseE env s0 prog scrutId ai defId =
  let
    arms = Flat.flatStrCases prog ai
    unit =
      all (flatIsUnitEffect prog . snd) arms
        && flatIsUnitEffect prog defId
    (s1, Code oDecl oRef) = flatPureAST' env s0 prog scrutId
    (resultN, s2) =
      if unit then (0, s1) else allocIdent s1
    resultVar = nName resultN
    renderArm s e =
      let
        (s', MkCode mDecl mRef _) = flatEffectfulAST' env s prog e
        body =
          if unit
            then asStmt mDecl mRef
            else fromMaybe mempty mDecl $$ assignResult resultVar mRef
       in
        (s', body)
    (s3, caseJSs) =
      mapAccumL
        ( \s (k, e) ->
            let
              (s', body) = renderArm s e
              line =
                "case"
                  <+> (jsQuote k <> colon)
                  <+> blockBody (body <+> ("break" <> semi))
             in
              (s', line)
        )
        s2
        arms
    (s4, defBody) = renderArm s3 defId
    defJS = "default:" <+> blockBody defBody
    switchStmt = "switch" <+> parens oRef <+> blockBody (vcat (caseJSs ++ [defJS]))
    prelude =
      if unit then oDecl else oDecl $$ letResult resultVar
    ref = if unit then Nothing else Just (jsText resultVar)
   in
    (s4, MkCode (Just (prelude $$ switchStmt)) ref False)

flatPureAST' ::
  Env
  -> CG
  -> Flat.FlatProgram
  -> Flat.NodeId
  -> (CG, Code)
flatPureAST' !env !s0 prog nid =
  case Flat.flatNode prog nid of
    Flat.FE_Literal li ->
      flatRenderLiteral env s0 (Flat.flatLitValue prog li)
    Flat.FE_Var i ->
      (s0, Code mempty (varStampJS env (Name i)))
    Flat.FE_Let tag xId bodyId ->
      let
        (nBind, s1) = allocIdent s0
        (s2, MkCode xDecl xRef _) = flatPureAST' env s1 prog xId
        env' = IM.insert tag nBind env
        (s3, yCode) = flatPureAST' env' s2 prog bodyId
       in
        ( s3
        , keepRef
            ( fromMaybe mempty xDecl
                $$ constBind nBind (fromMaybe mempty xRef)
                $$ fromMaybe mempty (codeDecl yCode)
            )
            yCode
        )
    Flat.FE_LetRec tag rId bId ->
      let
        (nBind, s1) = allocIdent s0
        n = nJS nBind
        env' = IM.insert tag nBind env
        (s2, MkCode rDecl rRef _) = flatPureAST' env' s1 prog rId
        (s3, bCode) = flatPureAST' env' s2 prog bId
       in
        ( s3
        , keepRef (recBindStmt n rDecl rRef $$ fromMaybe mempty (codeDecl bCode)) bCode
        )
    Flat.FE_Lambda tag bodyId ->
      let
        (nParam, s1) = allocIdent s0
        env' = IM.insert tag nParam env
        (s2, MkCode exprXDecl exprXRef _) = flatPureAST' env' s1 prog bodyId
       in
        (s2, Code mempty (renderFunction nParam exprXDecl exprXRef))
    Flat.FE_Apply fId xId ->
      let
        (s1, Code fDecl fRef) = flatPureAST' env s0 prog fId
        (s2, Code xDecl xRef) = flatPureAST' env s1 prog xId
       in
        (s2, Code (fDecl $$ xDecl) (jsCall fRef xRef))
    Flat.FE_EmbedEff eId -> flatEffectfulAST' env s0 prog eId
    Flat.FE_If cId tId eId ->
      let
        (s1, Code cDecl cRef) = flatPureAST' env s0 prog cId
        (s2, Code tDecl tRef) = flatPureAST' env s1 prog tId
        (s3, Code eDecl eRef) = flatPureAST' env s2 prog eId
       in
        ( s3
        , Code
            (cDecl $$ tDecl $$ eDecl)
            (parens (cRef <+> "?" <+> tRef <+> ":" <+> eRef))
        )
    Flat.FE_OptionCase oId nId tag sId ->
      let
        (s1, Code optDecl optRef) = flatPureAST' env s0 prog oId
        (nBind, s2) = allocIdent s1
        optVar = nName nBind
        env' = IM.insert tag nBind env
        (s3, Code noneDecl noneRef) = flatPureAST' env s2 prog nId
        (s4, Code someDecl someRef) = flatPureAST' env' s3 prog sId
       in
        ( s4
        , Code
            (optDecl $$ constBind nBind optRef $$ noneDecl $$ someDecl)
            ( parens
                (jsText optVar <+> "===" <+> "null" <+> "?" <+> noneRef <+> ":" <+> someRef)
            )
        )
    Flat.FE_ResultOk xId ->
      let
        (s1, MkCode d r _) = flatPureAST' env s0 prog xId
       in
        (s1, MkCode d (Just (resultObject True r)) False)
    Flat.FE_ResultErr xId ->
      let
        (s1, MkCode d r _) = flatPureAST' env s0 prog xId
       in
        (s1, MkCode d (Just (resultObject False r)) False)
    Flat.FE_ResultCase resId tagE errId tagO okId ->
      flatRenderResultCase env s0 prog resId tagE errId tagO okId
    Flat.FE_Index arrId idxId ->
      let
        s1 = useHelperSrc "$checkedIndex" jsCheckedIndexSrc s0
        (s2, Code aDecl aRef) = flatPureAST' env s1 prog arrId
        (s3, Code iDecl iRef) = flatPureAST' env s2 prog idxId
       in
        (s3, Code (aDecl $$ iDecl) (jsCheckedIndex aRef iRef))
    Flat.FE_U8Index bufId idxId ->
      let
        (s1, Code bDecl bRef) = flatPureAST' env s0 prog bufId
        (s2, Code iDecl iRef) = flatPureAST' env s1 prog idxId
       in
        (s2, Code (bDecl $$ iDecl) (bRef <> brackets iRef))
    Flat.FE_Error msgId ->
      let
        (s1, Code d r) = flatPureAST' env s0 prog msgId
       in
        (s1, Code d ("(function(){throw new Error(" <> r <> ");}())"))
    Flat.FE_Fixed fixed -> flatRenderFixed env s0 prog fixed
    Flat.FE_FnLit tags bodyId -> flatRenderFnLit env s0 prog tags bodyId
    Flat.FE_UnsafeNullable xId -> flatPureAST' env s0 prog xId
    Flat.FE_FrozenLit gi -> flatRenderObjectLit env s0 prog gi
    Flat.FE_GetField ti oId ->
      let
        (s1, Code d r) = flatPureAST' env s0 prog oId
       in
        (s1, Code d (jsDotOrBracket r (Flat.flatText prog ti)))
    Flat.FE_Hvm2Ref ti ->
      (s0, Code mempty (hvm2ExportRef (Flat.flatText prog ti)))
    knode ->
      case knode of
        Flat.FE_KConcat{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KPlus{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KTimes{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KMinus{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KNegate{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KFracDiv{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KRem{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KBitAnd{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KBitOr{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KBitXor{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KShl{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KShr{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KUShr{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KBig{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KBigNeg{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KAnd{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KOr{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KEq{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KNEq{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KGTh{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KLTh{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KGTEq{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KLTEq{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KShow{} -> flatRenderKernel env s0 prog knode
        Flat.FE_KTypeOf{} -> flatRenderKernel env s0 prog knode
        Flat.FE_MethMap{} -> flatRenderMethod env s0 prog knode
        Flat.FE_MethFilter{} -> flatRenderMethod env s0 prog knode
        Flat.FE_MethReduce{} -> flatRenderMethod env s0 prog knode
        Flat.FE_MethReduceRight{} -> flatRenderMethod env s0 prog knode
        Flat.FE_MethToSorted{} -> flatRenderMethod env s0 prog knode
        Flat.FE_MethFrom{} -> flatRenderMethod env s0 prog knode
        _ -> error "JShark.flatPureAST': unexpected node"

flatEffectfulAST' ::
  Env
  -> CG
  -> Flat.FlatProgram
  -> Flat.NodeId
  -> (CG, Code)
flatEffectfulAST' !env !s0 prog nid =
  case Flat.flatNode prog nid of
    Flat.FX_Lift eId -> flatPureAST' env s0 prog eId
    Flat.FX_FFI fi ai ->
      let
        (s1, argDecl, argRefs) = flatRenderArgList env s0 prog ai
       in
        ( s1
        , fxCode
            argDecl
            (renderFFIInvoke (Flat.flatFFI prog fi) argRefs)
        )
    Flat.FX_UnsafeObject ti ->
      (s0, Code mempty (jsText (Flat.flatText prog ti)))
    Flat.FX_UnsafeObjectGet xId sId ->
      let
        (s1, Code xDecl xRef) = flatEffectfulAST' env s0 prog xId
       in
        (s1, Code xDecl $ jsDotOrBracket xRef (Flat.flatText prog sId))
    Flat.FX_UnsafeObjectAssign xId yId ->
      let
        (s1, Code xDecl xRef) = flatEffectfulAST' env s0 prog xId
        (s2, Code yDecl yRef) = flatEffectfulAST' env s1 prog yId
       in
        (s2, fxCode (xDecl $$ yDecl) $ xRef <> " = " <> yRef)
    Flat.FX_CallMethod recvId methodIdx ai ->
      let
        method = Flat.flatText prog methodIdx
        (s1, Code rDecl rRef) = flatEffectfulAST' env s0 prog recvId
        (s2, argDecl, argRefs) = flatRenderArgList env s1 prog ai
       in
        ( s2
        , fxCode
            (rDecl $$ argDecl)
            (rRef <> "." <> jsText method <> parens argRefs)
        )
    Flat.FX_Bind tag xId bodyId ->
      flatBindEffect env s0 prog tag xId bodyId
    Flat.FX_ThenE xId yId -> flatSeqEffect env s0 prog xId yId
    Flat.FX_BindRec tag rId bId ->
      let
        (nBind, s1) = allocIdent s0
        n = nJS nBind
        env' = IM.insert tag nBind env
        (s2, MkCode rDecl rRef _) = flatEffectfulAST' env' s1 prog rId
        (s3, MkCode bDecl bRef bFX) = flatEffectfulAST' env' s2 prog bId
       in
        ( s3
        , MkCode (Just (recBindStmt n rDecl rRef $$ fromMaybe mempty bDecl)) bRef bFX
        )
    Flat.FX_LambdaE tag bodyId ->
      let
        (nParam, s1) = allocIdent s0
        env' = IM.insert tag nParam env
        (s2, MkCode exprXDecl exprXRef _) = flatEffectfulAST' env' s1 prog bodyId
       in
        (s2, Code mempty (renderFunction nParam exprXDecl exprXRef))
    Flat.FX_ApplyE fId xId ->
      let
        (s1, Code fDecl fRef) = flatEffectfulAST' env s0 prog fId
        (s2, Code xDecl xRef) = flatEffectfulAST' env s1 prog xId
       in
        (s2, fxCode (fDecl $$ xDecl) (jsCall fRef xRef))
    Flat.FX_IfE cId tId eId ->
      emitBranching
        ( flatIsUnitEffect prog tId
            && flatIsUnitEffect prog eId
        )
        s0
        ( \s ->
            let
              (s1, Code cDecl cRef) = flatEffectfulAST' env s prog cId
             in
              (s1, cDecl, cRef)
        )
        ( \mRes cRef s ->
            let
              (s1, MkCode tDecl tRef _) = flatEffectfulAST' env s prog tId
              (s2, MkCode eDecl eRef _) = flatEffectfulAST' env s1 prog eId
             in
              (s2, ifAssignOrStmt mRes cRef tDecl tRef eDecl eRef)
        )
    Flat.FX_While cId bId ->
      let
        (s1, MkCode condDecl condRef _) = flatEffectfulAST' env s0 prog cId
        (s2, MkCode bodyDecl bodyRef _) = flatEffectfulAST' env s1 prog bId
        bodyStmt = asStmt bodyDecl bodyRef
        whileStmt =
          "while"
            <+> parens (fromMaybe mempty condRef)
            <+> blockBody bodyStmt
       in
        (s2, MkCode (Just (fromMaybe mempty condDecl $$ whileStmt)) Nothing False)
    Flat.FX_ForRange startId endId tag bodyId ->
      let
        (s1, MkCode startDecl startRef _) = flatPureAST' env s0 prog startId
        (s2, MkCode endDecl endRef _) = flatPureAST' env s1 prog endId
        (loopN, s3) = allocIdent s2
        loopVar = nJS loopN
        env' = IM.insert tag loopN env
        (s4, MkCode bodyDecl bodyRef _) = flatEffectfulAST' env' s3 prog bodyId
        bodyStmt = asStmt bodyDecl bodyRef
        forHead =
          "let"
            <+> loopVar
            <+> "="
            <+> fromMaybe mempty startRef
            <+> ";"
            <+> loopVar
            <+> "<"
            <+> fromMaybe mempty endRef
            <+> ";"
            <+> loopVar
            <+> "++"
        forStmt = "for" <+> parens forHead <+> blockBody bodyStmt
       in
        ( s4
        , MkCode
            ( Just
                ( fromMaybe mempty startDecl
                    $$ fromMaybe mempty endDecl
                    $$ forStmt
                )
            )
            Nothing
            False
        )
    Flat.FX_U8Set bufId idxId valId ->
      let
        (s1, Code bDecl bRef) = flatPureAST' env s0 prog bufId
        (s2, Code iDecl iRef) = flatPureAST' env s1 prog idxId
        (s3, Code vDecl vRef) = flatPureAST' env s2 prog valId
        stmt = (bRef <> brackets iRef) <+> "=" <+> vRef
       in
        (s3, Code (bDecl $$ iDecl $$ vDecl $$ (stmt <> semi)) mempty)
    Flat.FX_U8Fill bufId valId ->
      let
        (s1, Code bDecl bRef) = flatPureAST' env s0 prog bufId
        (s2, Code vDecl vRef) = flatPureAST' env s1 prog valId
        stmt = bRef <> ".fill" <> parens vRef
       in
        (s2, Code (bDecl $$ vDecl $$ (stmt <> semi)) mempty)
    Flat.FX_OptionCaseE oId nId tag sId ->
      emitBranching
        ( flatIsUnitEffect prog nId
            && flatIsUnitEffect prog sId
        )
        s0
        ( \s ->
            let
              (s1, Code oDecl oRef) = flatPureAST' env s prog oId
              (nBind, s2) = allocIdent s1
             in
              (s2, oDecl $$ constBind nBind oRef, nBind)
        )
        ( \mRes nBind s ->
            let
              env' = IM.insert tag nBind env
              (s1, MkCode nDecl nRef _) = flatEffectfulAST' env s prog nId
              (s2, MkCode sDecl sRef _) = flatEffectfulAST' env' s1 prog sId
              cond = nJS nBind <+> "===" <+> "null"
             in
              (s2, ifAssignOrStmt mRes cond nDecl nRef sDecl sRef)
        )
    Flat.FX_ResultCaseE resId tagE errId tagO okId ->
      flatRenderResultCaseE env s0 prog resId tagE errId tagO okId
    Flat.FX_StringCaseE scrutId ai defId ->
      flatRenderStringCaseE env s0 prog scrutId ai defId
    Flat.FX_Throw xId ->
      let
        (s1, Code xDecl xRef) = flatPureAST' env s0 prog xId
       in
        (s1, Code (xDecl $$ (("throw" <+> xRef) <> semi)) mempty)
    Flat.FX_Try aId tag kId ->
      emitBranching
        (flatIsUnitEffect prog aId && flatIsUnitEffect prog kId)
        s0
        (\s -> (s, mempty, ()))
        ( \mRes () s ->
            let
              (s1, MkCode aDecl aRef _) = flatEffectfulAST' env s prog aId
              (catchN, s2) = allocIdent s1
              env' = IM.insert tag catchN env
              (s3, MkCode bDecl bRef _) = flatEffectfulAST' env' s2 prog kId
             in
              (s3, tryCatchStmt mRes catchN aDecl aRef bDecl bRef)
        )
    Flat.FX_ObjectLit gi -> flatRenderObjectLit env s0 prog gi
    Flat.FX_DeleteProp oId kId ->
      let
        (s1, Code oDecl oRef) = flatEffectfulAST' env s0 prog oId
        (s2, Code kDecl kRef) = flatPureAST' env s1 prog kId
       in
        (s2, fxCode (oDecl $$ kDecl) (("delete" <+> oRef) <> brackets kRef))
    Flat.FX_ArrayLit es -> flatRenderArrayLit env s0 prog es
    _ -> error "JShark.flatEffectfulAST': unexpected node"

flatProgramNodeCount :: Flat.FlatProgram -> Int
flatProgramNodeCount p = V.length (Flat.fpNodes p)

flatEffectfulCodegen ::
  ClosedEffect u -> (CG, Code)
flatEffectfulCodegen (e :: ClosedEffect u) =
  let
    prog =
      FlatSoA.optimizeFlatProgram (Flat.packEffectProgram (irEffectFromClosed e))
    root = Flat.fpRootEffect prog
   in
    if root < 0 || root >= flatProgramNodeCount prog
      then error "JShark.flatEffectfulCodegen: invalid root node"
      else flatEffectfulAST' IM.empty startCG prog root

effectfulASTFromFlat :: ClosedEffect u -> JS
effectfulASTFromFlat e = uncurry renderWithHelpers (flatEffectfulCodegen e)

effectfulAST :: ClosedEffect u -> JS
effectfulAST e
  | closedEffectNodes e >= optIrLargeThreshold =
      effectfulASTFromFlat e
  | otherwise =
      uncurry
        renderWithHelpers
        (effectfulAST' IM.empty startCG (optimizeEffectTree e))

-- | Flat IR codegen after 'Ir.optIrEffect'. Below 'optIrLargeThreshold',
-- 'effectfulAST' still uses the PHOAS pipeline; this always uses flat
-- pack + SoA opts. Output matches 'effectfulAST' on 'irParityTests'.
effectfulASTIr :: ClosedEffect u -> JS
effectfulASTIr = effectfulASTFromFlat

-- | Conservative stmt-only test for unused-bind @ThenE@ merge. Never
-- materializes continuations (@f nestedDummy@).
isUnitBoundEffect :: Effect Stamp u -> Bool
isUnitBoundEffect = \case
  Lift (Literal ValueUnit) -> True
  Lift (Var (EmbedEff e)) -> isUnitBoundEffect e
  While {} -> True
  ForRange {} -> True
  Throw {} -> True
  ThenE _ b -> isUnitBoundEffect b
  _ -> False

-- | Stmt-only codegen for branching effects (no shared @let result@).
isUnitWitness :: Effect Stamp u -> Bool
isUnitWitness = \case
  Lift (Literal ValueUnit) -> True
  Lift (Var (EmbedEff e)) -> isUnitWitness e
  Lift _ -> False
  While {} -> True
  ForRange {} -> True
  Bind _ f -> isUnitWitness (f nestedDummy)
  ThenE _ y -> isUnitWitness y
  BindRec _ f -> isUnitWitness (f nestedDummy)
  IfE _ t e -> isUnitWitness t && isUnitWitness e
  OptionCaseE _ n s -> isUnitWitness n && isUnitWitness (s nestedDummy)
  ResultCaseE _ e s ->
    isUnitWitness (e nestedDummy) && isUnitWitness (s nestedDummy)
  StringCaseE _ arms d -> all (isUnitWitness . snd) arms && isUnitWitness d
  Throw {} -> True
  Try a k -> isUnitWitness a && isUnitWitness (k nestedDummy)
  _ -> False

-- | Turn a rendered effect into a statement. Unit values may still have a
-- non-empty ref (@el.x = v@, @foo()@); those become statements, not
-- @let n = …@.
asStmt :: Maybe (JS) -> Maybe (JS) -> JS
asStmt mDecl mRef = case mRef of
  Nothing -> fromMaybe mempty mDecl
  Just r -> fromMaybe mempty mDecl $$ (r <> semi)

ifElseStmt ::
  JS
  -> Maybe (JS)
  -> Maybe (JS)
  -> Maybe (JS)
  -> Maybe (JS)
  -> JS
ifElseStmt cRef tDecl tRef eDecl eRef
  | isNothing eDecl && isNothing eRef =
      "if" <+> parens cRef <+> blockBody (asStmt tDecl tRef)
  | otherwise =
      "if"
        <+> parens cRef
        <+> blockBody (asStmt tDecl tRef)
          $$ "else"
        <+> blockBody (asStmt eDecl eRef)

assignResult :: Text -> Maybe (JS) -> JS
assignResult resultVar mRef = case mRef of
  Nothing -> mempty
  Just r -> (jsText resultVar <+> "=" <+> r) <> semi

letResult :: Text -> JS
letResult resultVar = ("let" <+> jsText resultVar) <> semi

recBindStmt :: JS -> Maybe (JS) -> Maybe (JS) -> JS
recBindStmt n rDecl rRef =
  fromMaybe mempty rDecl
    $$ (("const" <+> n <+> "=" <+> fromMaybe mempty rRef) <> semi)

resultCasePrelude ::
  Env
  -> CG
  -> Expr Stamp ('Result e a)
  -> (CG, JS, Text, Int)
resultCasePrelude env s0 res =
  let
    (s1, MkCode rDecl rRef _) = pureAST' s0 env res
    (nObj, s2) = allocIdent s1
    (nUnw, s3) = allocIdent s2
    obj = nName nObj
    prelude =
      fromMaybe mempty rDecl
        $$ constBind nObj (fromMaybe mempty rRef)
        $$ constBind nUnw (jsText obj <> ".value")
   in
    (s3, prelude, obj, nUnw)

-- | Unit arms: prelude + stmt, empty ref. Value arms: prelude +
-- @let result@ + stmt, result ident.
emitBranching ::
  Bool
  -> CG
  -> (CG -> (CG, JS, extra))
  -> (Maybe Text -> extra -> CG -> (CG, JS))
  -> (CG, Code)
emitBranching unit s0 prelude k
  | unit =
      let
        (s1, pre, extra) = prelude s0
        (s2, stmt) = k Nothing extra s1
       in
        (s2, MkCode (Just (pre $$ stmt)) Nothing False)
  | otherwise =
      let
        (s1, pre, extra) = prelude s0
        (n, s2) = allocIdent s1
        rv = nName n
        (s3, stmt) = k (Just rv) extra s2
       in
        (s3, MkCode (Just (pre $$ letResult rv $$ stmt)) (Just (jsText rv)) False)

ifAssignOrStmt ::
  Maybe Text
  -> JS
  -> Maybe (JS)
  -> Maybe (JS)
  -> Maybe (JS)
  -> Maybe (JS)
  -> JS
ifAssignOrStmt Nothing c tD tR eD eR = ifElseStmt c tD tR eD eR
ifAssignOrStmt (Just rv) c tD tR eD eR =
  "if"
    <+> parens c
    <+> blockBody (fromMaybe mempty tD $$ assignResult rv tR)
      $$ "else"
    <+> blockBody (fromMaybe mempty eD $$ assignResult rv eR)

tryCatchStmt ::
  Maybe Text
  -> Int
  -> Maybe (JS)
  -> Maybe (JS)
  -> Maybe (JS)
  -> Maybe (JS)
  -> JS
tryCatchStmt mRes catchN aDecl aRef bDecl bRef =
  let
    catchHead = "catch" <+> parens (nJS catchN)
   in
    case mRes of
      Nothing ->
        "try"
          <+> blockBody (asStmt aDecl aRef)
            $$ (catchHead <+> blockBody (asStmt bDecl bRef))
      Just rv ->
        "try"
          <+> blockBody (fromMaybe mempty aDecl $$ assignResult rv aRef)
            $$ (catchHead <+> blockBody (fromMaybe mempty bDecl $$ assignResult rv bRef))

renderFunction :: Int -> Maybe (JS) -> Maybe (JS) -> JS
renderFunction nParam decl ref =
  "function"
    <+> parens (nJS nParam)
    <+> blockBody (fromMaybe mempty decl $$ ret)
 where
  -- Empty ref is Unit (event handlers, forEach of noOp). `return ()`
  -- is a SyntaxError; HughesPJ `parens` of empty is `()`.
  ret = case ref of
    Nothing -> "return"
    Just r -> "return" <+> parens r

-- | @function (n0, n1) { decls return ref }@ — callback style (bare return).
jsCallback :: [JS] -> JS -> JS -> JS
jsCallback params decl ref =
  "function"
    <+> parens (hcat (punctuate ", " params))
    <+> blockBody (decl $$ "return" <+> ref)

renderFFIForm :: FFIForm -> JS
renderFFIForm = \case
  FFICall s -> jsText s
  FFILambda s -> parens (jsText s)

-- | Multi-parameter arrow lambdas are invalid IIFEs as @(...=>{...})(a,b)@;
--   wrap the lambda in an extra pair of parens so the call applies cleanly.
--   Parenthesized arrows from 'classifyFFI' become 'FFICall'; only wrap twice
--   when the callee is not already a whole parenthesized expression.
renderFFIInvoke :: FFIForm -> JS -> JS
renderFFIInvoke fn argRefs = case fn of
  FFILambda s -> parens (jsText s) <> parens argRefs
  FFICall s ->
    let callee = jsText s
     in
      if "=>" `T.isInfixOf` s && not (isWholeParenthesized s)
        then parens callee <> parens argRefs
        else callee <> parens argRefs

-- | True when @t@ is @(… )@ with balanced outer parentheses only.
isWholeParenthesized :: Text -> Bool
isWholeParenthesized t =
  case T.uncons t of
    Nothing -> False
    Just ('(', rest) ->
      case T.unsnoc rest of
        Nothing -> False
        Just (inner, ')') -> parenBalanced inner (0 :: Int)
        Just _ -> False
    _ -> False
 where
  parenBalanced txt depth =
    case T.uncons txt of
      Nothing -> depth == 0
      Just ('(', rest) -> parenBalanced rest (depth + 1)
      Just (')', rest)
        | depth == 0 -> False
        | otherwise -> parenBalanced rest (depth - 1)
      Just (_, rest) -> parenBalanced rest depth

effectfulAST' :: forall v. Env -> CG -> Effect Stamp v -> (CG, Code)
effectfulAST' !env !s0 = \case
  Lift x -> pureAST' s0 env x
  FFI fn args ->
    let
      (s1, argDecl, argRefs) = renderArgList env s0 args
     in
      (s1, fxCode argDecl (renderFFIInvoke fn argRefs))
  IfE c t e ->
    -- Value-producing @if@: a shared result var is assigned in both
    -- arms. Do not use emptiness to pick a ternary — a Unit leftover
    -- ref is not a genuinely-empty Doc.
    emitBranching
      (isUnitWitness t && isUnitWitness e)
      s0
      ( \s ->
          let
            (s1, Code cDecl cRef) = effectfulAST' env s c
           in
            (s1, cDecl, cRef)
      )
      ( \mRes cRef s ->
          let
            (s1, MkCode tDecl tRef _) = effectfulAST' env s t
            (s2, MkCode eDecl eRef _) = effectfulAST' env s1 e
           in
            (s2, ifAssignOrStmt mRes cRef tDecl tRef eDecl eRef)
      )
  While cond body ->
    let
      (s1, MkCode condDecl condRef _) = effectfulAST' env s0 cond
      (s2, MkCode bodyDecl bodyRef _) = effectfulAST' env s1 body
      bodyStmt = asStmt bodyDecl bodyRef
      whileStmt = "while" <+> parens (fromMaybe mempty condRef) <+> blockBody bodyStmt
     in
      (s2, MkCode (Just (fromMaybe mempty condDecl $$ whileStmt)) Nothing False)
  ForRange start end body ->
    let
      (s1, MkCode startDecl startRef _) = pureAST' s0 env start
      (s2, MkCode endDecl endRef _) = pureAST' s1 env end
      (loopN, s3) = allocIdent s2
      loopVar = nJS loopN
      (s4, MkCode bodyDecl bodyRef _) = effectfulAST' env s3 (body (Name loopN))
      bodyStmt = asStmt bodyDecl bodyRef
      forHead =
        "let"
          <+> loopVar
          <+> "="
          <+> fromMaybe mempty startRef
          <+> ";"
          <+> loopVar
          <+> "<"
          <+> fromMaybe mempty endRef
          <+> ";"
          <+> loopVar
          <+> "++"
      forStmt = "for" <+> parens forHead <+> blockBody bodyStmt
     in
      ( s4
      , MkCode
          (Just (fromMaybe mempty startDecl $$ fromMaybe mempty endDecl $$ forStmt))
          Nothing
          False
      )
  U8Set buf idx val ->
    let
      (s1, Code bDecl bRef) = pureAST' s0 env buf
      (s2, Code iDecl iRef) = pureAST' s1 env idx
      (s3, Code vDecl vRef) = pureAST' s2 env val
      stmt = (bRef <> brackets iRef) <+> "=" <+> vRef
     in
      (s3, Code (bDecl $$ iDecl $$ vDecl $$ (stmt <> semi)) mempty)
  U8Fill buf val ->
    let
      (s1, Code bDecl bRef) = pureAST' s0 env buf
      (s2, Code vDecl vRef) = pureAST' s1 env val
      stmt = bRef <> ".fill" <> parens vRef
     in
      (s2, Code (bDecl $$ vDecl $$ (stmt <> semi)) mempty)
  OptionCaseE opt noneE someF ->
    let
      (sProbe, tagged, binderTag) = probeContEff s0 someF
     in
      emitBranching
        (isUnitWitness noneE && isUnitWitness (someF nestedDummy))
        sProbe
        ( \s ->
            let
              (s1, Code oDecl oRef) = pureAST' s env opt
              (nBind, s2) = allocIdent s1
             in
              (s2, oDecl $$ constBind nBind oRef, nBind)
        )
        ( \mRes nBind s ->
            let
              env' = IM.insert binderTag nBind env
              (s1, MkCode nDecl nRef _) = effectfulAST' env s noneE
              (s2, MkCode sDecl sRef _) = effectfulAST' env' s1 tagged
              cond = nJS nBind <+> "===" <+> "null"
             in
              (s2, ifAssignOrStmt mRes cond nDecl nRef sDecl sRef)
        )
  Try a k ->
    let
      (sProbe, tagged, binderTag) = probeContEff s0 k
     in
      emitBranching
        (isUnitWitness a && isUnitWitness (k nestedDummy))
        sProbe
        (\s -> (s, mempty, ()))
        ( \mRes () s ->
            let
              (s1, MkCode aDecl aRef _) = effectfulAST' env s a
              (catchN, s2) = allocIdent s1
              env' = IM.insert binderTag catchN env
              (s3, MkCode bDecl bRef _) = effectfulAST' env' s2 tagged
             in
              (s3, tryCatchStmt mRes catchN aDecl aRef bDecl bRef)
        )
  Bind x f -> bindEffectCode env s0 x f
  ThenE x y -> seqEffectCode env s0 x y
  BindRec r b ->
    let
      (nBind, s1) = allocIdent s0
      n = nJS nBind
      (s2, MkCode rDecl rRef _) = effectfulAST' env s1 (r (Name nBind))
      (s3, MkCode bDecl bRef bFX) = effectfulAST' env s2 (b (Name nBind))
     in
      ( s3
      , MkCode (Just (recBindStmt n rDecl rRef $$ fromMaybe mempty bDecl)) bRef bFX
      )
  Throw x ->
    let
      (s1, Code xDecl xRef) = pureAST' s0 env x
     in
      (s1, Code (xDecl $$ (("throw" <+> xRef) <> semi)) mempty)
  ObjectLit fs -> renderObjectLit env s0 fs
  ArrayLit es -> renderArrayLit env s0 es
  DeleteProp o k ->
    let
      (s1, Code oDecl oRef) = effectfulAST' env s0 o
      (s2, Code kDecl kRef) = pureAST' s1 env k
     in
      (s2, fxCode (oDecl $$ kDecl) (("delete" <+> oRef) <> brackets kRef))
  ResultCaseE res errF okF -> renderResultCaseE env s0 res errF okF
  StringCaseE scrut arms def -> renderStringCaseE env s0 scrut arms def
  UnsafeObject obj -> (s0, Code mempty (jsText obj))
  UnsafeObjectGet x string ->
    let
      (s1, Code x1Decl x1Ref) = effectfulAST' env s0 x
     in
      (s1, Code x1Decl $ jsDotOrBracket x1Ref string)
  UnsafeObjectAssign x y ->
    let
      (s1, Code x1Decl x1Ref) = effectfulAST' env s0 x
      (s2, Code y1Decl y1Ref) = effectfulAST' env s1 y
     in
      (s2, fxCode (x1Decl $$ y1Decl) $ x1Ref <> " = " <> y1Ref)
  CallMethod recv name args ->
    let
      (s1, Code rDecl rRef) = effectfulAST' env s0 recv
      (s2, argDecl, argRefs) = renderArgList env s1 args
     in
      (s2, fxCode (rDecl $$ argDecl) (rRef <> "." <> jsText name <> parens argRefs))
  LambdaE f -> emitEffectLambda env s0 f
  ApplyE fex ex ->
    let
      (s1, Code exprXDecl exprXRef) = effectfulAST' env s0 fex
      (s2, Code exprYDecl exprYRef) = effectfulAST' env s1 ex
     in
      (s2, fxCode (exprXDecl $$ exprYDecl) (jsCall exprXRef exprYRef))

letCode ::
  Env -> CG -> Expr Stamp u -> (Stamp u -> Expr Stamp v) -> (CG, Code)
letCode env s0 x g =
  let
    (sProbe, tagged, probeTag) = probeContExpr s0 g
    (binderTag, uses) = letProbeTag probeTag tagged
    (s1, MkCode xDecl xRef _) = pureAST' sProbe env x
   in
    case uses of
      0 ->
        let
          (s2, y) = pureAST' s1 env tagged
          stmt
            | isNothing xDecl && isJust xRef = fromMaybe mempty xRef <> semi
            | otherwise = fromMaybe mempty xDecl
         in
          (s2, keepRef (stmt $$ fromMaybe mempty (codeDecl y)) y)
      _ ->
        let
          (nBind, s2) = allocIdent s1
          env' = IM.insert binderTag nBind env
          (s3, y) = pureAST' s2 env' tagged
         in
          ( s3
          , keepRef
              ( fromMaybe mempty xDecl
                  $$ constBind nBind (fromMaybe mempty xRef)
                  $$ fromMaybe mempty (codeDecl y)
              )
              y
          )

pureAST :: ClosedExpr u -> JS
pureAST e = uncurry renderWithHelpers (pureAST' startCG IM.empty (optimize e))

pureAST' ::
  forall v.
  CG
  -> Env
  -> Expr Stamp v
  -> (CG, Code)
pureAST' !s0 env = \case
  Literal v -> case v of
    ValueNumber d -> (s0, Code mempty (jsDouble d))
    ValueBigInt n -> (s0, Code mempty (jsBigIntLit n))
    ValueArray xs ->
      let
        (s1, exprs) = mapAccumL (\s x -> pureAST' s env (Literal x)) s0 xs
       in
        ( s1
        , Code
            (codesDecls exprs)
            (brackets (hcat (punctuate ", " (codesRefs exprs))))
        )
    ValueString s -> (s0, Code mempty (jsQuote s))
    ValueFunction _ -> error "JShark.pureAST: ValueFunction is eval-only"
    ValueUnit -> (s0, mempty)
    ValueOption (Just x) -> pureAST' s0 env (Literal x)
    ValueOption Nothing -> (s0, Code mempty "null")
    ValueResult (Right x) -> renderResultLit True s0 x
    ValueResult (Left x) -> renderResultLit False s0 x
    ValueRegex s ->
      (s0, Code mempty ("new RegExp" <> parens (jsQuote s)))
    ValueUint8Array ba -> (s0, Code mempty (jsUint8ArrayLit ba))
    ValueBool True -> (s0, Code mempty "true")
    ValueBool False -> (s0, Code mempty "false")
    ValueFrozen {} -> error "JShark.pureAST: ValueFrozen is eval-only"
  Lambda f -> emitExprLambda env s0 f
  -- `const` when shared or used under a lambda/loop/short-circuit.
  Let x g -> letCode env s0 x g
  LetRec r b ->
    let
      (nBind, s1) = allocIdent s0
      n = nJS nBind
      (s2, MkCode rDecl rRef _) = pureAST' s1 env (r (Name nBind))
      (s3, bCode) = pureAST' s2 env (b (Name nBind))
     in
      ( s3
      , keepRef (recBindStmt n rDecl rRef $$ fromMaybe mempty (codeDecl bCode)) bCode
      )
  Apply fex ex ->
    let
      (s1, Code exprXDecl exprXRef) = pureAST' s0 env fex
      (s2, Code exprYDecl exprYRef) = pureAST' s1 env ex
     in
      (s2, Code (exprXDecl $$ exprYDecl) (jsCall exprXRef exprYRef))
  Var (Embed e) -> pureAST' s0 env (flattenExpr e)
  Var (EmbedEff e) -> effectfulAST' env s0 e
  Var s ->
    -- Tags and the unused-binder dummy are negative; map via `env`.
    (s0, Code mempty (varStampJS env s))
  If c t e ->
    let
      (s1, Code cDecl cRef) = pureAST' s0 env c
      (s2, Code tDecl tRef) = pureAST' s1 env t
      (s3, Code eDecl eRef) = pureAST' s2 env e
     in
      ( s3
      , Code
          (cDecl $$ tDecl $$ eDecl)
          (parens (cRef <+> "?" <+> tRef <+> ":" <+> eRef))
      )
  OptionCase opt none' someF ->
    case opt of
      Var (Embed e) -> pureAST' s0 env (OptionCase (flattenExpr e) none' someF)
      Var s ->
        let
          i = stampId s
          optVar = nName i
          (s2, Code noneDecl noneRef) = pureAST' s0 env none'
          (s3, Code someDecl someRef) = pureAST' s2 env (someF (Name i))
         in
          ( s3
          , Code
              (noneDecl $$ someDecl)
              ( parens
                  (jsText optVar <+> "===" <+> "null" <+> "?" <+> noneRef <+> ":" <+> someRef)
              )
          )
      _ ->
        let
          (s1, Code optDecl optRef) = pureAST' s0 env opt
          (nBind, s2) = allocIdent s1
          optVar = nName nBind
          (s3, Code noneDecl noneRef) = pureAST' s2 env none'
          (s4, Code someDecl someRef) = pureAST' s3 env (someF (Name nBind))
         in
          ( s4
          , Code
              (optDecl $$ constBind nBind optRef $$ noneDecl $$ someDecl)
              ( parens
                  (jsText optVar <+> "===" <+> "null" <+> "?" <+> noneRef <+> ":" <+> someRef)
              )
          )
  ResultOk x ->
    let
      (s1, MkCode d r _) = pureAST' s0 env x
     in
      (s1, MkCode d (Just (resultObject True r)) False)
  ResultErr x ->
    let
      (s1, MkCode d r _) = pureAST' s0 env x
     in
      (s1, MkCode d (Just (resultObject False r)) False)
  ResultCase res errF okF -> renderResultCase env s0 res errF okF
  Index arr idx ->
    let
      s1 = useHelperSrc "$checkedIndex" jsCheckedIndexSrc s0
      (s2, Code aDecl aRef) = pureAST' s1 env arr
      (s3, Code iDecl iRef) = pureAST' s2 env idx
     in
      (s3, Code (aDecl $$ iDecl) (jsCheckedIndex aRef iRef))
  U8Index buf idx ->
    let
      (s1, Code bDecl bRef) = pureAST' s0 env buf
      (s2, Code iDecl iRef) = pureAST' s1 env idx
     in
      (s2, Code (bDecl $$ iDecl) (bRef <> brackets iRef))
  Error msg ->
    let
      (s1, Code d r) = pureAST' s0 env msg
     in
      (s1, Code d ("(function(){throw new Error(" <> r <> ");}())"))
  Std s -> renderStd env s0 s
  FnLit body -> renderFn env s0 body
  UnsafeNullable x -> pureAST' s0 env x
  FrozenLit fs -> renderObjectLit env s0 fs
  GetField @k o ->
    let
      (s1, Code d r) = pureAST' s0 env o
     in
      (s1, Code d (jsDotOrBracket r (T.pack (symbolVal (Proxy @k)))))
  Hvm2Kernel name _ ->
    (s0, Code mempty (hvm2ExportRef name))

renderFixed ::
  Env
  -> CG
  -> FixedOp a b c u
  -> FixedArgs Stamp a b c
  -> (CG, Code)
renderFixed env s0 op args = case (op, args) of
  (n, ArgsU x)
    | Just name <- Prim.math1Name n ->
        let
          (s1, Code xDecl xRef) = pureAST' s0 env x
         in
          (s1, Code xDecl ("Math." <> jsText name <> parens xRef))
  (n, ArgsB x y)
    | Just name <- Prim.math2Name n ->
        let
          (s1, Code xDecl xRef) = pureAST' s0 env x
          (s2, Code yDecl yRef) = pureAST' s1 env y
         in
          ( s2
          , Code
              (xDecl $$ yDecl)
              ( "Math."
                  <> jsText name
                  <> parens (xRef <> ", " <> yRef)
              )
          )
  (n, ArgsU recv) ->
    let
      (s1, Code rDecl rRef) = pureAST' s0 env recv
     in
      (s1, Code rDecl (Prim.fixedUnaryJS n (wrapOperand recv rRef)))
  (n, ArgsB recv arg) ->
    let
      (s1, Code rDecl rRef) = pureAST' s0 env recv
      (s2, Code aDecl aRef) = pureAST' s1 env arg
     in
      (s2, Code (rDecl $$ aDecl) (Prim.fixedBinaryJS n (wrapOperand recv rRef) aRef))
  (n, ArgsT recv a b) ->
    let
      (s1, Code rDecl rRef) = pureAST' s0 env recv
      (s2, Code aDecl aRef) = pureAST' s1 env a
      (s3, Code bDecl bRef) = pureAST' s2 env b
     in
      ( s3
      , Code
          (rDecl $$ aDecl $$ bDecl)
          (Prim.fixedTernaryJS n (wrapOperand recv rRef) aRef bRef)
      )

resultPayloadRef :: Maybe (JS) -> JS
resultPayloadRef = fromMaybe "undefined"

resultObject :: Bool -> Maybe (JS) -> JS
resultObject isOk payload =
  let
    flag = if isOk then "true" else "false"
   in
    braces ((("ok:" <+> flag) <> ",") <+> ("value:" <+> resultPayloadRef payload))

renderResultLit :: Bool -> CG -> Value u -> (CG, Code)
renderResultLit isOk s0 x =
  let
    (s1, MkCode d r _) = pureAST' s0 IM.empty (Literal x)
   in
    (s1, MkCode d (Just (resultObject isOk r)) False)

renderArrayLit :: Env -> CG -> [Effect Stamp u] -> (CG, Code)
renderArrayLit env s0 es =
  let
    (s1, cs) = mapAccumL (\s e -> effectfulAST' env s e) s0 es
   in
    ( s1
    , Code
        (codesDecls cs)
        (brackets (hcat (punctuate ", " (codesRefs cs))))
    )

renderObjectLit :: Env -> CG -> [FieldLit Stamp r] -> (CG, Code)
renderObjectLit env s0 fs =
  let
    (s1, parts) =
      mapAccumL
        ( \s fl ->
            case fl of
              FieldLit e ->
                let
                  (s', Code d r) = pureAST' s env e
                 in
                  (s', (d, (dquotes (jsText (fieldKey fl)) <> ":") <+> r))
              FieldLitExtra e ->
                let
                  (s', Code d r) = pureAST' s env e
                 in
                  (s', (d, (dquotes (jsText (fieldKey fl)) <> ":") <+> r))
              FieldLitEffect e ->
                let
                  (s', MkCode d r _) = effectfulAST' env s e
                 in
                  ( s'
                  ,
                    ( fromMaybe mempty d
                    , (dquotes (jsText (fieldKey fl)) <> ":") <+> fromMaybe mempty r
                    )
                  )
              FieldLitExtraEffect e ->
                let
                  (s', MkCode d r _) = effectfulAST' env s e
                 in
                  ( s'
                  ,
                    ( fromMaybe mempty d
                    , (dquotes (jsText (fieldKey fl)) <> ":") <+> fromMaybe mempty r
                    )
                  )
        )
        s0
        fs
    (declList, pairs) = unzip parts
   in
    (s1, Code (vcatNonEmpty declList) (braces (hcat (punctuate ", " pairs))))

renderResultCase ::
  Env
  -> CG
  -> Expr Stamp ('Result e a)
  -> (Stamp e -> Expr Stamp v)
  -> (Stamp a -> Expr Stamp v)
  -> (CG, Code)
renderResultCase env s0 res errF okF =
  let
    (s1, prelude, obj, nUnw) = resultCasePrelude env s0 res
    (s2, Code eDecl eRef) = pureAST' s1 IM.empty (errF (Name nUnw))
    (s3, Code oDecl oRef) = pureAST' s2 IM.empty (okF (Name nUnw))
   in
    ( s3
    , Code
        (prelude $$ eDecl $$ oDecl)
        (parens ((jsText obj <> ".ok") <+> "?" <+> oRef <+> ":" <+> eRef))
    )

renderStringCaseE ::
  Env
  -> CG
  -> Expr Stamp 'String
  -> [(Text, Effect Stamp v)]
  -> Effect Stamp v
  -> (CG, Code)
renderStringCaseE env s0 scrut arms def =
  let
    unit = all (isUnitWitness . snd) arms && isUnitWitness def
    (s1, Code oDecl oRef) = pureAST' s0 env scrut
    (resultN, s2) =
      if unit then (0, s1) else allocIdent s1
    resultVar = nName resultN
    renderArm s e =
      let
        (s', MkCode mDecl mRef _) = effectfulAST' env s e
        body =
          if unit
            then asStmt mDecl mRef
            else fromMaybe mempty mDecl $$ assignResult resultVar mRef
       in
        (s', body)
    (s3, caseJSs) =
      mapAccumL
        ( \s (k, e) ->
            let
              (s', body) = renderArm s e
              line =
                "case" <+> (jsQuote k <> colon) <+> blockBody (body <+> ("break" <> semi))
             in
              (s', line)
        )
        s2
        arms
    (s4, defBody) = renderArm s3 def
    defJS = "default:" <+> blockBody defBody
    switchStmt = "switch" <+> parens oRef <+> blockBody (vcat (caseJSs ++ [defJS]))
    prelude =
      if unit then oDecl else oDecl $$ letResult resultVar
    ref = if unit then Nothing else Just (jsText resultVar)
   in
    (s4, MkCode (Just (prelude $$ switchStmt)) ref False)

renderResultCaseE ::
  Env
  -> CG
  -> Expr Stamp ('Result e a)
  -> (Stamp e -> Effect Stamp v)
  -> (Stamp a -> Effect Stamp v)
  -> (CG, Code)
renderResultCaseE env s0 res errF okF =
  let
    (s1, errTagged, tagE) = probeContEff s0 errF
    (s2, okTagged, tagO) = probeContEff s1 okF
   in
    if isUnitWitness (errF nestedDummy) && isUnitWitness (okF nestedDummy)
      then
        let
          (s3, prelude, obj, _) = resultCasePrelude env s2 res
          (s4, MkCode eDecl eRef _) = effectfulAST' env s3 errTagged
          (s5, MkCode oDecl oRef _) = effectfulAST' env s4 okTagged
         in
          ( s5
          , Code
              (prelude $$ ifElseStmt (jsText obj <> ".ok") oDecl oRef eDecl eRef)
              mempty
          )
      else
        let
          (s3, prelude, obj, nUnw) = resultCasePrelude env s2 res
          (resultN, s4) = allocIdent s3
          resultVar = nName resultN
          envE = IM.insert tagE nUnw env
          envO = IM.insert tagO nUnw envE
          (s5, MkCode eDecl eRef _) = effectfulAST' envE s4 errTagged
          (s6, MkCode oDecl oRef _) = effectfulAST' envO s5 okTagged
          stmt =
            prelude
              $$ letResult resultVar
              $$ ifElseStmt
                (jsText obj <> ".ok")
                (Just (fromMaybe mempty oDecl $$ assignResult resultVar oRef))
                Nothing
                (Just (fromMaybe mempty eDecl $$ assignResult resultVar eRef))
                Nothing
         in
          (s6, Code stmt (jsText resultVar))

emitExprLambda :: Env -> CG -> (Stamp u -> Expr Stamp v) -> (CG, Code)
emitExprLambda env = emitLambdaWith (\s e -> pureAST' s env e)

emitEffectLambda :: Env -> CG -> (Stamp u -> Effect Stamp v) -> (CG, Code)
emitEffectLambda env = emitLambdaWith (effectfulAST' env)

emitLambdaWith ::
  (CG -> t -> (CG, Code))
  -> CG
  -> (Stamp u -> t)
  -> (CG, Code)
emitLambdaWith walker s0 f =
  let
    (nParam, s1) = allocIdent s0
    (s2, MkCode exprXDecl exprXRef _) = walker s1 (f (Name nParam))
   in
    (s2, Code mempty (renderFunction nParam exprXDecl exprXRef))

renderBinaryFn ::
  Env
  -> CG
  -> (Stamp a -> Stamp b -> Expr Stamp c)
  -> (CG, JS)
renderBinaryFn env s0 f =
  let
    (s1, Code _ cb) = renderFn env s0 (JfCons $ \a -> JfCons $ \b -> JfNil (f a b))
   in
    (s1, cb)

renderStd :: Env -> CG -> Std Stamp u -> (CG, Code)
renderStd env s0 = \case
  Fixed op args -> renderFixed env s0 op args
  Method m -> renderMethod env s0 m
  Kernel k -> renderKernel env s0 k

renderKernel :: Env -> CG -> Kernel Stamp u -> (CG, Code)
renderKernel env s0 = \case
  KConcat x y -> renderBin env "+" s0 x y
  KPlus x y -> renderBin env "+" s0 x y
  KMinus x y -> renderBin env "-" s0 x y
  KTimes x y -> renderBin env "*" s0 x y
  KFracDiv x y -> renderBin env "/" s0 x y
  KRem x y -> renderBin env "%" s0 x y
  KBitAnd x y -> renderBin env "&" s0 x y
  KBitOr x y -> renderBin env "|" s0 x y
  KBitXor x y -> renderBin env "^" s0 x y
  KShl x y -> renderBin env "<<" s0 x y
  KShr x y -> renderBin env ">>" s0 x y
  KUShr x y -> renderBin env ">>>" s0 x y
  KBig op x y -> renderBin env (bigOpJS op) s0 x y
  KBigNeg x ->
    let
      (s1, Code x1Decl x1Ref) = pureAST' s0 env x
     in
      (s1, Code x1Decl $ "-" <> parens x1Ref)
  KShow x ->
    let
      (s1, Code x1Decl x1Ref) = pureAST' s0 env x
     in
      (s1, Code x1Decl $ "String" <> parens x1Ref)
  KTypeOf x ->
    let
      (s1, Code x1Decl x1Ref) = pureAST' s0 env x
      wrapped = case x of
        FrozenLit {} -> parens x1Ref
        _ -> x1Ref
     in
      (s1, Code x1Decl $ "typeof" <+> wrapped)
  KNegate x ->
    let
      (s1, Code x1Decl x1Ref) = pureAST' s0 env x
     in
      (s1, Code x1Decl $ "-" <> parens x1Ref)
  KAnd x y -> renderBin env "&&" s0 x y
  KOr x y -> renderBin env "||" s0 x y
  KEq structural x y
    | structural ->
        renderBinApp env jsValueEq (useEqHelpers s0) x y
    | otherwise ->
        renderBin env "===" s0 x y
  KNEq structural x y
    | structural ->
        renderBinApp env jsValueNEq (useEqHelpers s0) x y
    | otherwise ->
        renderBin env "!==" s0 x y
  KGTh x y -> renderBin env ">" s0 x y
  KLTh x y -> renderBin env "<" s0 x y
  KGTEq x y -> renderBin env ">=" s0 x y
  KLTEq x y -> renderBin env "<=" s0 x y

renderMethod :: Env -> CG -> Method Stamp u -> (CG, Code)
renderMethod env s0 = \case
  MethMap recv f -> renderCallbackMethod env "map" s0 recv f
  MethFilter recv f -> renderCallbackMethod env "filter" s0 recv f
  MethReduce recv z f -> renderFold env ".reduce" s0 recv z f
  MethReduceRight recv z f -> renderFold env ".reduceRight" s0 recv z f
  MethToSorted recv f ->
    let
      (s1, Code rDecl rRef) = pureAST' s0 env recv
      (s2, cb) = renderBinaryFn env s1 f
      call = wrapOperand recv rRef <> ".toSorted" <> parens cb
     in
      (s2, Code rDecl call)
  MethFrom n f ->
    let
      (s1, Code nDecl nRef) = pureAST' s0 env n
      (nHole, s2) = allocIdent s1
      (nI, s3) = allocIdent s2
      (s4, Code exDecl exRef) = pureAST' s3 env (f (Name nI))
      cb = jsCallback [nJS nHole, nJS nI] exDecl exRef
     in
      (s4, Code nDecl ("Array.from({length: " <> nRef <> "}, " <> cb <> ")"))

renderFold ::
  Env
  -> String
  -> CG
  -> Expr Stamp ('Array u)
  -> Expr Stamp v
  -> (Stamp v -> Stamp u -> Expr Stamp v)
  -> (CG, Code)
renderFold env method s0 recv z f =
  let
    (s1, Code rDecl rRef) = pureAST' s0 env recv
    (s2, Code zDecl zRef) = pureAST' s1 env z
    (s3, cb) = renderBinaryFn env s2 f
    call = wrapOperand recv rRef <> jsString method <> parens (cb <> ", " <> zRef)
   in
    (s3, Code (rDecl $$ zDecl) call)

renderCallbackMethod ::
  Env
  -> String
  -> CG
  -> Expr Stamp a
  -> (Stamp b -> Expr Stamp c)
  -> (CG, Code)
renderCallbackMethod env name s0 recv f =
  let
    (s1, Code rDecl rRef) = pureAST' s0 env recv
    (nParam, s2) = allocIdent s1
    (s3, Code exDecl exRef) = pureAST' s2 env (f (Name nParam))
    call =
      wrapOperand recv rRef
        <> "."
        <> jsString name
        <> parens (jsCallback [nJS nParam] exDecl exRef)
   in
    (s3, Code rDecl call)

renderBin :: Env -> Text -> CG -> Expr Stamp a -> Expr Stamp b -> (CG, Code)
renderBin env op s0 x y =
  renderBinApp
    env
    (\l r -> wrapOperand x l <+> jsText op <+> wrapOperand y r)
    s0
    x
    y

renderBinApp ::
  Env
  -> (JS -> JS -> JS)
  -> CG
  -> Expr Stamp a
  -> Expr Stamp b
  -> (CG, Code)
renderBinApp env join s0 x y =
  let
    (s1, Code xDecl xRef) = pureAST' s0 env x
    (s2, Code yDecl yRef) = pureAST' s1 env y
   in
    (s2, Code (xDecl $$ yDecl) (join xRef yRef))

argAST :: Env -> CG -> Arg Stamp u -> (CG, Code)
argAST env s (ArgExpr e) = pureAST' s env e
argAST env s (ArgEffect e) = effectfulAST' env s e

renderArgList ::
  Env -> CG -> Rec (Arg Stamp) us -> (CG, JS, JS)
renderArgList env s0 args =
  let
    (s1, cs) = recCodes (argAST env) s0 args
   in
    (s1, codesDecls cs, hcat (punctuate ", " (codesRefs cs)))
