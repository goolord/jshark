{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
      )
  , FnBody (..)
  , Value (..)
  , GroupBy
  , Arg (..)
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
  , evaluateCached
  -- Optimization
  , optimize
  , optimizeEffect
  -- Codegen
  , pureAST
  , effectfulAST
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
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int32)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (mapAccumL)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Monoid (All (..), Any (..), Sum (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable, eqT, type (:~:) (..))
import Data.Word (Word32)
import GHC.Exts (Int (..), indexWord8Array#, sizeofByteArray#)
import GHC.TypeLits (KnownSymbol, sameSymbol, symbolVal)
import GHC.Word (Word8 (..))
import JShark.Prim
  ( MathBinary (..)
  , MathUnary (..)
  , matchMathBinary
  , matchMathUnary
  )
import qualified JShark.Prim as Prim
import JShark.Rec
import JShark.Types
import Numeric (readInt, showFFloat, showHex)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName
  ( StableName
  , eqStableName
  , hashStableName
  , makeStableName
  )
import Text.PrettyPrint (Doc, ($$), (<+>))
import qualified Text.PrettyPrint as P
import Unsafe.Coerce (unsafeCoerce)

unNumber :: Value 'Number -> Double
unNumber (ValueNumber d) = d

unBool :: Value 'Bool -> Bool
unBool (ValueBool b) = b

unString :: Value 'String -> Text
unString (ValueString s) = s

unFunction :: Value ('Function u v) -> Value u -> Value v
unFunction (ValueFunction f) = f

valueEq :: Value u -> Value u -> Bool
valueEq (ValueNumber a) (ValueNumber b) = a == b
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

-- | Only numbers, strings, and booleans support ordering comparisons.
valueCompare :: Value u -> Value u -> Ordering
valueCompare (ValueNumber a) (ValueNumber b) = compare a b
valueCompare (ValueString a) (ValueString b) = compare a b
valueCompare (ValueBool a) (ValueBool b) = compare a b
valueCompare _ _ =
  error
    "evaluate: only numbers, strings, and booleans support ordering comparisons"

-- | Mimics JS's @String(x)@ coercion closely enough for the reference interpreter.
jsShow :: Value u -> Text
jsShow (ValueNumber d) = T.pack (jsShowNumber d)
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

jsQuote :: Text -> Doc
jsQuote s = P.doubleQuotes (P.text (escapeJsString (T.unpack s)))

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

jsUint8ArrayLit :: ByteArray -> Doc
jsUint8ArrayLit ba =
  let
    elems = uint8Elems ba
    n = length elems
   in
    if all (== 0) elems
      then "new Uint8Array" <> P.parens (P.int n)
      else
        "new Uint8Array"
          <> P.parens
            ( P.brackets
                (P.hcat (P.punctuate ", " (map (P.int . fromIntegral) elems)))
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

-- | Pure reference interpreter. Shared Haskell heap nodes are walked
-- once per occurrence (no memo table). Use 'evaluateCached' when host-level
-- sharing should be observed.
evaluate :: ClosedExpr u -> Value u
evaluate = evalValue

evalValue :: Expr Value v -> Value v
evalValue = runIdentity . evalAlg (Identity . evalValue) (\g v -> evalValue (g v))

-- | One algebra. 'evaluate' is Identity; 'evaluateCached' memos via
-- 'goOpen' / 'applyCached'.
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

-- Per-evaluation memo table keyed by 'StableName'. Recovers host-language
-- sharing (Haskell @let x = e in x + x@) so a shared 'Expr' node is only
-- interpreted once. Object-language 'Let' already preserves sharing on its
-- own; this cache is what makes the two coincide.
data CacheEntry where
  CacheEntry :: Typeable u => StableName (Expr Value u) -> Value u -> CacheEntry

type EvalCache = IORef (IntMap [CacheEntry])

-- | Like 'evaluate', but memoizes shared heap nodes via 'StableName'.
-- In 'IO' because observable sharing is inherently effectful.
-- No 'Typeable' on the result: 'goOpen' memos constructors whose
-- universe is a concrete 'Typeable' type ('Number', 'Bool', …).
evaluateCached :: ClosedExpr u -> IO (Value u)
evaluateCached e0 = do
  cache <- newIORef IM.empty
  goOpen cache e0

go :: forall v. Typeable v => EvalCache -> Expr Value v -> IO (Value v)
go cache e = do
  sn <- makeStableName $! e
  m <- readIORef cache
  case lookupCache sn (IM.lookup (hashStableName sn) m) of
    Just v -> pure v
    Nothing -> do
      v <- goNode cache e
      modifyIORef'
        cache
        (IM.insertWith (++) (hashStableName sn) [CacheEntry sn v])
      pure v

lookupCache ::
  forall v.
  Typeable v =>
  StableName (Expr Value v) -> Maybe [CacheEntry] -> Maybe (Value v)
lookupCache sn ments = ments >>= findHit
 where
  findHit [] = Nothing
  findHit (CacheEntry sn' val : rest)
    | eqStableName sn sn' =
        case castValue val of
          Just val' -> Just val'
          Nothing ->
            error "evaluateCached: StableName hit at a different universe"
    | otherwise = findHit rest

castValue :: forall u v. (Typeable u, Typeable v) => Value u -> Maybe (Value v)
castValue val = case eqT @u @v of
  Just Refl -> Just val
  Nothing -> Nothing

-- Named and NOINLINE so GHC cannot CSE applications at different 'v'.
applyCached :: EvalCache -> (Value u -> Expr Value v) -> Value u -> Value v
applyCached cache g v = unsafePerformIO (goOpen cache (g v))
{-# NOINLINE applyCached #-}

-- | Memoize a kernel node. Each 'Kernel' constructor fixes the result
-- universe, giving 'Typeable' evidence for 'go'.
goKernel :: EvalCache -> Expr Value v -> Kernel Value v -> IO (Value v)
goKernel cache e = \case
  KPlus {} -> go cache e
  KTimes {} -> go cache e
  KMinus {} -> go cache e
  KNegate {} -> go cache e
  KFracDiv {} -> go cache e
  KRem {} -> go cache e
  KBitAnd {} -> go cache e
  KBitOr {} -> go cache e
  KBitXor {} -> go cache e
  KShl {} -> go cache e
  KShr {} -> go cache e
  KUShr {} -> go cache e
  KConcat {} -> go cache e
  KShow {} -> go cache e
  KTypeOf {} -> go cache e
  KAnd {} -> go cache e
  KOr {} -> go cache e
  KEq {} -> go cache e
  KNEq {} -> go cache e
  KGTh {} -> go cache e
  KLTh {} -> go cache e
  KGTEq {} -> go cache e
  KLTEq {} -> go cache e

-- | Memoize constructors whose result universe is a concrete 'Typeable'
-- type. Polymorphic-result nodes cannot call 'go' (no 'Typeable'
-- evidence); they fall through to 'evalValue'.
goOpen :: EvalCache -> Expr Value v -> IO (Value v)
goOpen cache e = case e of
  Std (Kernel k) -> goKernel cache e k
  -- Memoize @Math@ / @parseInt@ only ('Typeable' @Number@ — see 'matchMathUnary').
  Std (Fixed FixParseInt _) -> go cache e
  Std (Fixed op _) ->
    case matchMathUnary op of
      Just (MathUnary _) -> go cache e
      Nothing ->
        case matchMathBinary op of
          Just (MathBinary _) -> go cache e
          Nothing -> pure (evalValue e)
  Literal ValueNumber {} -> go cache e
  Literal ValueString {} -> go cache e
  Literal ValueBool {} -> go cache e
  Literal ValueUnit -> go cache e
  Literal ValueRegex {} -> go cache e
  Literal ValueUint8Array {} -> go cache e
  FrozenLit fs -> ValueFrozen <$> traverse (evalFieldLit (goOpen cache)) fs
  GetField @k o -> do
    ov <- goOpen cache o
    withFrozenField @k ov (goOpen cache)
  _ -> pure (evalValue e)

goNode :: EvalCache -> Expr Value v -> IO (Value v)
goNode cache = evalAlg (goOpen cache) (applyCached cache)

printComputation :: Doc -> IO ()
printComputation computation = putStrLn (renderJSCompact computation)

renderJS :: Doc -> String
renderJS = P.renderStyle P.style

-- | Linear dump. HughesPJ 'PageMode' on a large inlined @bindRec@
-- body is superlinear (breakout sat in 'renderJS' for tens of seconds).
-- 'compileEffect' uses this, then 'prettyJS' for 'Readable'.
renderJSCompact :: Doc -> String
renderJSCompact = P.renderStyle P.style {P.mode = P.LeftMode}

-- | Runtime helpers emitted once per program as @const name = source@.
-- Recording the use in 'CG' keeps a second call site from repeating the
-- definition. A named @function@ declaration would become a global in a
-- classic script.
helperDecls :: CG -> Doc
helperDecls s =
  P.vcat
    [ ("const" <+> P.text name <+> "=" <+> P.text src) <> P.semi
    | (name, src) <- M.toAscList (cgHelpers s)
    ]

jsValueEq :: Doc -> Doc -> Doc
jsValueEq a b = "$valueEq" <> P.parens (a <> ("," <+> b))

jsValueNEq :: Doc -> Doc -> Doc
jsValueNEq a b = "!" <> P.parens (jsValueEq a b)

useEqHelpers :: CG -> CG
useEqHelpers s0 = foldr (uncurry useHelperSrc) s0 jsEqHelpers

-- | Integer slot + throw on a hole. Raw @a[i]@ would use the string key
-- (@a[1.9]@ is @undefined@) and invent @undefined@ at an arbitrary @u@.
jsCheckedIndex :: Doc -> Doc -> Doc
jsCheckedIndex arr idx =
  P.parens
    "function(a,i){var n=Math.trunc(i);if(!(n>=0&&n<a.length))throw new Error(\"jshark: index\");return a[n];}"
    <> P.parens (arr <> ("," <+> idx))

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
jsDotOrBracket :: Doc -> String -> Doc
jsDotOrBracket obj key
  | jsIdent key = obj <> "." <> P.text key
  | (seg, '.' : rest) <- break (== '.') key
  , jsIdent seg
  , not (null rest) =
      jsDotOrBracket (jsDotOrBracket obj seg) rest
  | otherwise = obj <> "[" <> P.doubleQuotes (P.text key) <> "]"

jsIdent :: String -> Bool
jsIdent [] = False
jsIdent (c : cs) = jsIdStart c && all jsIdPart cs
 where
  jsIdStart x = Char.isAscii x && (Char.isLetter x || x == '_' || x == '$')
  jsIdPart x = jsIdStart x || Char.isDigit x

data Code = MkCode
  { codeDecl :: Doc
  , codeRef :: Doc
  , codeRefFX :: Bool
  }

-- | Two-field sugar for a non-effectful leftover ref. Do not rematch
-- this pattern and rebuild — that drops 'codeRefFX'. Take 'MkCode'
-- apart and use 'keepRef' / 'fxCode'.
pattern Code :: Doc -> Doc -> Code
pattern Code d r <- MkCode d r _
 where
  Code d r = MkCode d r False

{-# COMPLETE Code #-}

fxCode :: Doc -> Doc -> Code
fxCode d r = MkCode d r True

-- | New decls, same ref and effectfulness as the source 'Code'.
keepRef :: Doc -> Code -> Code
keepRef d (MkCode _ r f) = MkCode d r f

instance Semigroup Code where
  MkCode a b f <> MkCode x y g = MkCode (a <> b) (x <> y) (f || g)

instance Monoid Code where
  mempty = MkCode mempty mempty False

renderCode :: Code -> Doc
renderCode (MkCode a b _) = a $$ b

-- | Wrap helpers + generated decls + result in an IIFE so a minifier treats
-- the result as live (plain expression statements get DCE'd).
renderIIFE :: CG -> Code -> Doc
renderIIFE s (MkCode decls ref _) =
  let
    stmts = helperDecls s $$ decls
    body = if P.isEmpty ref then stmts else stmts $$ (("return" <+> ref) <> P.semi)
   in
    "(() => {" $$ P.nest 2 body $$ "})()"

-- | Helper definitions ahead of a snippet's own declarations.
renderWithHelpers :: CG -> Code -> Doc
renderWithHelpers s code = helperDecls s $$ renderCode code

-- | Pure expression compiled to a self-contained JS program (IIFE).
pureProgram :: ClosedExpr u -> Doc
pureProgram e = uncurry renderIIFE (pureAST' startCG (optimize e))

-- | Effectful computation compiled to a self-contained JS program (IIFE).
effectfulProgram :: ClosedEffect u -> Doc
effectfulProgram e = uncurry renderIIFE (effectfulAST' startCG (optimizeEffect e))

partitionCode :: [Code] -> ([Doc], [Doc])
partitionCode = unzip . map (\(MkCode a b _) -> (a, b))

-- | 'ValueUnit' renders as nothing, since a unit statement emits nothing.
-- As an array element it still occupies a slot, so it has to print — a
-- dropped ref would shorten the literal.
arrayElemRef :: Doc -> Doc
arrayElemRef r = if P.isEmpty r then "undefined" else r

-- Codegen counters: `cgIdent` is the next emitted JS name (`n0`, `n1`, …);
-- `cgTag` is a decreasing negative id used only for use-counting/inlining
-- so nested Lets/Binds cannot collide (tags are never valid JS idents).
-- `cgHelpers` is the set of runtime functions the program has called.
--
-- `cgTag` walks the odd negatives and the optimizer's tags
-- ('optimizeEffect') the even ones, so the two numberings can never name
-- the same binder. Sharing the space made `countEffect` attribute a
-- binder's uses to a leftover optimizer tag, see zero, and drop the
-- `const` while its uses rendered empty.
data CG = CG
  { cgIdent :: {-# UNPACK #-} !Int
  , cgTag :: {-# UNPACK #-} !Int
  , cgHelpers :: !(M.Map String String)
  }

startCG :: CG
startCG = CG 0 (-3) M.empty

allocTag :: CG -> (Int, CG)
allocTag s = (cgTag s, s {cgTag = cgTag s - 2})

allocIdent :: CG -> (Int, CG)
allocIdent s = (cgIdent s, s {cgIdent = cgIdent s + 1})

useHelperSrc :: String -> String -> CG -> CG
useHelperSrc name src s = s {cgHelpers = M.insert name src (cgHelpers s)}

nestedDummyId :: Int
nestedDummyId = minBound

nestedDummy :: Stamp u
nestedDummy = Name nestedDummyId

-- | Codegen binder: @n0@, @n1@, …
nName :: Int -> String
nName n = 'n' : show n

nDoc :: Int -> Doc
nDoc n = P.text (nName n)

constBind :: Int -> Doc -> Doc
constBind n ref = ("const" <+> nDoc n <+> "=" <+> ref) <> P.semi

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

jsCall :: Doc -> Doc -> Doc
jsCall f a = P.parens f <> P.parens a

-- | Needs no parentheses as an operand: already a primary JS expression.
isSimple :: Expr Stamp u -> Bool
isSimple = \case
  Literal {} -> True
  Var (EmbedEff e) -> isSimpleEffect e
  Var {} -> True
  Std (Kernel (KShow {})) -> True
  Std (Kernel (KTypeOf {})) -> True
  Std (Kernel (KNegate {})) -> True
  Std (Kernel _) -> False
  Std {} -> True
  FnLit {} -> True
  Index {} -> True
  U8Index {} -> True
  Error {} -> False
  UnsafeNullable x -> isSimple x
  FrozenLit {} -> True
  GetField {} -> True
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

wrapOperand :: Expr Stamp u -> Doc -> Doc
wrapOperand e d = if isSimple e then d else P.parens d

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

-- Re-opt only small trees. A second walk of a @bindRec@ / do-chain
-- paint body is what hung todo-mvc and breakout.
optSmall :: Int
optSmall = 16

sizeExpr :: Expr Stamp u -> Int
sizeExpr e = case e of
  Var (Embed e') -> sizeExpr e'
  Var (EmbedEff e') -> sizeEffect e'
  _ -> 1 + getSum (foldExpr nestedDummy s s sf e)
 where
  s = Sum . sizeExpr
  sf = Sum . sizeEffect

sizeEffect :: Effect Stamp u -> Int
sizeEffect e = 1 + getSum (foldEff nestedDummy s sf sf e)
 where
  s = Sum . sizeExpr
  sf = Sum . sizeEffect

-- | First-order reopen: rename the tag allocated by 'optUnder'. Never
-- re-applies the original PHOAS @f@.
rebindExpr :: Int -> Expr Stamp v -> Stamp u -> Expr Stamp v
rebindExpr tag body s = renameExpr tag (stampId s) body

rebindEff :: Int -> Effect Stamp v -> Stamp u -> Effect Stamp v
rebindEff tag body s = renameEff tag (stampId s) body

rebindExpr2 :: Int -> Int -> Expr Stamp v -> Stamp a -> Stamp b -> Expr Stamp v
rebindExpr2 tA tB body a b =
  renameExpr tA (stampId a) (renameExpr tB (stampId b) body)

keepExprCont ::
  Int
  -> Int
  -> Expr Stamp v
  -> (Stamp u -> Expr Stamp v)
  -> Stamp u
  -> Expr Stamp v
keepExprCont t tag body f
  | sizeExpr body <= optSmall = reoptExpr t f
  | otherwise = rebindExpr tag body

keepEffCont ::
  Int
  -> Int
  -> Effect Stamp v
  -> (Stamp u -> Effect Stamp v)
  -> Stamp u
  -> Effect Stamp v
keepEffCont t tag body f
  | sizeEffect body <= optSmall = reoptEff t f
  | otherwise = rebindEff tag body

keepExprCont2 ::
  Int
  -> Int
  -> Int
  -> Expr Stamp v
  -> (Stamp a -> Stamp b -> Expr Stamp v)
  -> Stamp a
  -> Stamp b
  -> Expr Stamp v
keepExprCont2 t tA tB body f a b
  | sizeExpr body <= optSmall = reoptExpr2 t f a b
  | otherwise = rebindExpr2 tA tB body a b

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
  rebindGo (t : ts) e = unsafeCoerce (JfCons $ \s -> rebindGo ts (renameExpr t (stampId s) e))

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
  Int -> FnBody Stamp us v -> (Int, [Int], Expr Stamp v, FnBody Stamp us v)
optUnderFn t0 body =
  let
    (tags, tEnd) = allocFnTags t0 body
    expr = evalFnBody body tags
    (t1, expr') = optExpr tEnd expr
   in
    (t1, tags, expr', body)

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

renderFn :: forall us r. CG -> FnBody Stamp us r -> (CG, Code)
renderFn s0 body =
  let
    n = fnArity body
    (ids, s1) = allocNIdents s0 n
    (s2, Code d r) = pureAST' s1 (evalFnBody body ids)
   in
    (s2, Code mempty (jsCallback (map nDoc ids) d r))

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
flattenExpr :: Expr Stamp u -> Expr Stamp u
flattenExpr = \case
  Var (Embed e) -> flattenExpr e
  Var (EmbedEff (Lift e)) -> flattenExpr e
  Var (EmbedEff e) -> Var (EmbedEff (flattenEff e))
  e -> mapExpr flattenExpr flattenEff e

flattenEff :: Effect Stamp u -> Effect Stamp u
flattenEff = \case
  Lift (Var (EmbedEff e)) -> flattenEff e
  e -> mapEff flattenExpr flattenEff e

-- | Replace 'Stamp' @old@ with @new@. Phantom in the universe, so this
-- does not need a cast. Used after the one 'optUnder' apply of @f@.
renameExpr :: Int -> Int -> Expr Stamp u -> Expr Stamp u
renameExpr old new = \case
  Var (Embed e) -> renameExpr old new (flattenExpr e)
  Var (EmbedEff (Lift e)) -> renameExpr old new (flattenExpr e)
  Var (EmbedEff e) -> Var (EmbedEff (renameEff old new e))
  Var (Stamp t) | t == old -> Var (Stamp new)
  Var s -> Var s
  e -> mapExpr (renameExpr old new) (renameEff old new) e

renameEff :: Int -> Int -> Effect Stamp u -> Effect Stamp u
renameEff old new = \case
  Lift (Var (EmbedEff e)) -> renameEff old new (flattenEff e)
  e -> mapEff (renameExpr old new) (renameEff old new) e

inlineExpr :: (Stamp u -> Expr Stamp v) -> Expr Stamp u -> Expr Stamp v
inlineExpr f x = flattenExpr (f (Embed x))

inlineEff :: (Stamp u -> Effect Stamp v) -> Effect Stamp u -> Effect Stamp v
inlineEff f x = flattenEff (f (EmbedEff x))

-- | Re-apply a PHOAS continuation and optimize at the next free tag @t@
-- (never a reset @-2@ — that collides with 'Stamp's already in the tree).
reoptExpr :: Int -> (Stamp u -> Expr Stamp v) -> Stamp u -> Expr Stamp v
reoptExpr t f b = snd (optExpr t (flattenExpr (f b)))

reoptEff :: Int -> (Stamp u -> Effect Stamp v) -> Stamp u -> Effect Stamp v
reoptEff t f b = snd (optEffect t (flattenEff (f b)))

reoptExpr2 ::
  Int
  -> (Stamp u -> Stamp w -> Expr Stamp v)
  -> Stamp u
  -> Stamp w
  -> Expr Stamp v
reoptExpr2 t f a b = snd (optExpr t (flattenExpr (f a b)))

-- | Constant-fold and drop dead pure bindings. Applied automatically by
-- codegen. This is the End-algebra: a closed term is instantiated at
-- 'Stamp' for the name supply (Kmett: take the end, then interpret).
-- Host-language sharing is recovered by 'evaluateCached' and by
-- instantiating the 'ClosedExpr' once ('NOINLINE') before this walk.
optimize :: ClosedExpr u -> Expr Stamp u
optimize e = flattenExpr (snd (optExpr (-2) e))
{-# NOINLINE optimize #-}

optimizeEffect :: ClosedEffect u -> Effect Stamp u
optimizeEffect e = flattenEff (snd (optEffect (-2) e))
{-# NOINLINE optimizeEffect #-}

-- | Tags step by two, keeping the optimizer on the even negatives.
-- Codegen's 'allocTag' owns the odd ones, so neither can name a binder the
-- other is counting.
optStep :: Int
optStep = 2

optUnder :: Int -> (Stamp u -> Expr Stamp v) -> (Int, Int, Expr Stamp v)
optUnder t0 f =
  let
    tag = t0
    (t1, body) = optExpr (t0 - optStep) (f (Stamp tag))
   in
    (t1, tag, body)

optUnderE :: Int -> (Stamp u -> Effect Stamp v) -> (Int, Int, Effect Stamp v)
optUnderE t0 f =
  let
    tag = t0
    (t1, body) = optEffect (t0 - optStep) (f (Stamp tag))
   in
    (t1, tag, body)

optUnder2 ::
  Int -> (Stamp a -> Stamp b -> Expr Stamp v) -> (Int, Int, Int, Expr Stamp v)
optUnder2 t0 f =
  let
    tA = t0
    tB = t0 - optStep
    (t1, body) = optExpr (t0 - 2 * optStep) (f (Stamp tA) (Stamp tB))
   in
    (t1, tA, tB, body)

isCheapValue :: Value u -> Bool
isCheapValue = \case
  ValueNumber {} -> True
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
isCheap = \case
  Literal v -> isCheapValue v
  UnsafeNullable x -> isCheap x
  Var (EmbedEff _) -> False
  FrozenLit fs -> all isCheapFieldLit fs
  GetField o -> isCheap o
  FnLit _ -> False
  _ -> False

isCheapFieldLit :: FieldLit Stamp r -> Bool
isCheapFieldLit (FieldLit e) = isCheap e
isCheapFieldLit (FieldLitExtra e) = isCheap e
isCheapFieldLit (FieldLitEffect _) = False
isCheapFieldLit (FieldLitExtraEffect _) = False

isCheapEffect :: Effect Stamp u -> Bool
isCheapEffect = \case
  Lift x -> isCheap x
  -- Object literals are identity-sensitive (mutation / shared state).
  UnsafeObject {} -> False
  _ -> False

class PhoasDummy f where
  phoasDummy :: f u

instance PhoasDummy Stamp where
  phoasDummy = nestedDummy

instance PhoasDummy Value where
  phoasDummy = error "JShark.isPureExpr: Value binder"

isPureExpr :: PhoasDummy f => Expr f u -> Bool
isPureExpr e = case e of
  Std (Fixed op args) -> isPureFixedArgs op args
  _ -> getAll (foldExpr phoasDummy p p pe e)
 where
  isPureFixedArgs op args =
    Prim.isPureFixed op
      && case args of
        ArgsU x -> isPureExpr x
        ArgsB x y -> isPureExpr x && isPureExpr y
        ArgsT x y z -> isPureExpr x && isPureExpr y && isPureExpr z
  p = All . isPureExpr
  pe = All . isPureEffectStamp

isPureEffectStamp :: PhoasDummy f => Effect f u -> Bool
isPureEffectStamp e = case e of
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
  _ ->
    getAll
      ( foldEff
          phoasDummy
          (All . isPureExpr)
          (All . isPureEffectStamp)
          (All . isPureEffectStamp)
          e
      )

isPureEffect :: Effect Stamp u -> Bool
isPureEffect = isPureEffectStamp

optArgs :: Int -> Rec (Arg Stamp) us -> (Int, Rec (Arg Stamp) us)
optArgs = mapAccumRec optArg

optArg :: Int -> Arg Stamp u -> (Int, Arg Stamp u)
optArg t (ArgExpr e) = fmap ArgExpr (optExpr t e)
optArg t (ArgEffect e) = fmap ArgEffect (optEffect t e)

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

optFixed ::
  Int
  -> FixedOp a b c u
  -> FixedArgs Stamp a b c
  -> (Int, Expr Stamp u)
optFixed t0 op args = case (op, args) of
  (n, ArgsU x)
    | Just (MathUnary n') <- matchMathUnary n ->
        let
          (t1, x') = optExpr t0 x
         in
          (t1, foldFixedUnary n' x')
  (n, ArgsB x y)
    | Just (MathBinary n') <- matchMathBinary n ->
        let
          (t1, x') = optExpr t0 x
          (t2, y') = optExpr t1 y
         in
          (t2, foldFixedBinary n' x' y')
  (FixArrLen, ArgsU x) ->
    let
      (t1, x') = optExpr t0 x
     in
      (t1, foldArrLen x')
  (n, ArgsU x) ->
    let
      (t1, x') = optExpr t0 x
     in
      (t1, expr1 n x')
  (n, ArgsB x y) ->
    let
      (t1, x') = optExpr t0 x
      (t2, y') = optExpr t1 y
     in
      (t2, expr2 n x' y')
  (n, ArgsT x y z) ->
    let
      (t1, x') = optExpr t0 x
      (t2, y') = optExpr t1 y
      (t3, z') = optExpr t2 z
     in
      (t3, expr3 n x' y' z')

optLet ::
  Int -> Expr Stamp u -> (Stamp u -> Expr Stamp v) -> (Int, Expr Stamp v)
optLet t0 x f =
  let
    (t1, x') = optExpr t0 x
    (t2, tag, body) = optUnder t1 f
   in
    elimLetFrom t2 x' f tag body

-- Count uses on the already-optimized body. Large tails keep that
-- body (rename-only reopen). Small @f@ may still be applied once more
-- so nested lets / optionCase peel fold.
data ElimOps src body = ElimOps
  { elimCount :: Int -> body -> Int
  , elimPure :: src -> Bool
  , elimCheap :: src -> Bool
  , elimSize :: body -> Int
  , elimRebuild :: body -> body
  , elimSplice :: Int -> (Int, body)
  , elimDropUnused :: src -> Bool
  }

elimFrom :: ElimOps src body -> Int -> src -> Int -> body -> (Int, body)
elimFrom ops t x tag body =
  let
    uses = elimCount ops tag body
    kept = elimRebuild ops body
    inlined
      | elimSize ops body > optSmall = (t, kept)
      | otherwise = elimSplice ops t
   in
    case uses of
      0 | elimPure ops x, elimDropUnused ops x -> (t, body)
      0 -> (t, kept)
      1 -> inlined
      _ | elimCheap ops x -> inlined
      _ -> (t, kept)

elimLetFrom ::
  Int
  -> Expr Stamp u
  -> (Stamp u -> Expr Stamp v)
  -> Int
  -> Expr Stamp v
  -> (Int, Expr Stamp v)
elimLetFrom t x f tag body =
  -- uses==1 is always a single strict use: countExpr already
  -- treats lambda/loop/?:/&&-|| RHS positions as 2.
  elimFrom
    ElimOps
      { elimCount = countExpr
      , elimPure = isPureExpr
      , elimCheap = isCheap
      , elimSize = sizeExpr
      , elimRebuild = Let x . rebindExpr tag
      , elimSplice = \t' -> optExpr t' (inlineExpr f x)
      , elimDropUnused = const True
      }
    t
    x
    tag
    body

optBind ::
  Int -> Effect Stamp u -> (Stamp u -> Effect Stamp v) -> (Int, Effect Stamp v)
optBind t0 x f =
  let
    (t1, x') = optEffect t0 x
    (t2, tag, body) = optUnderE t1 f
   in
    elimBindFrom t2 x' f tag body

elimBindFrom ::
  Int
  -> Effect Stamp u
  -> (Stamp u -> Effect Stamp v)
  -> Int
  -> Effect Stamp v
  -> (Int, Effect Stamp v)
elimBindFrom t x f tag body =
  elimFrom
    ElimOps
      { elimCount = countEffect
      , elimPure = isPureEffect
      , elimCheap = isCheapEffect
      , elimSize = sizeEffect
      , elimRebuild = Bind x . rebindEff tag
      , elimSplice = \t' -> optEffect t' (inlineEff f x)
      , elimDropUnused = not . isAliasBind
      }
    t
    x
    tag
    body

optBin ::
  Int
  -> (Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool)
  -> Expr Stamp u
  -> Expr Stamp u
  -> (Int, Expr Stamp 'Bool)
optBin t0 k x y =
  let
    (t1, x') = optExpr t0 x
    (t2, y') = optExpr t1 y
   in
    (t2, k x' y')

optBinNum ::
  Int
  -> (Double -> Double -> Double)
  -> (Expr Stamp 'Number -> Expr Stamp 'Number -> Expr Stamp 'Number)
  -> Expr Stamp 'Number
  -> Expr Stamp 'Number
  -> (Int, Expr Stamp 'Number)
optBinNum t0 f k x y =
  let
    (t1, x') = optExpr t0 x
    (t2, y') = optExpr t1 y
   in
    (t2, foldNum2 f k x' y')

optUnNum ::
  Int
  -> (Double -> Double)
  -> (Expr Stamp 'Number -> Expr Stamp 'Number)
  -> Expr Stamp 'Number
  -> (Int, Expr Stamp 'Number)
optUnNum t0 f k x =
  let
    (t1, x') = optExpr t0 x
   in
    (t1, foldNum1 f k x')

optExpr :: Int -> Expr Stamp u -> (Int, Expr Stamp u)
optExpr t0 = \case
  Literal v -> (t0, Literal v)
  Var (Embed e) -> optExpr t0 (flattenExpr e)
  Var (EmbedEff (Lift e)) -> optExpr t0 (flattenExpr e)
  Var (EmbedEff e) ->
    let
      (t1, e') = optEffect t0 e
     in
      case e' of
        Lift x -> (t1, x)
        _ -> (t1, Var (EmbedEff e'))
  Var v -> (t0, Var v)
  Let x f -> optLet t0 x f
  LetRec r b ->
    let
      tag = t0
      (t1, r') = optExpr (t0 - optStep) (r (Stamp tag))
      (t2, b') = optExpr t1 (b (Stamp tag))
     in
      (t2, LetRec (keepExprCont t2 tag r' r) (keepExprCont t2 tag b' b))
  Lambda f ->
    let
      (t1, tag, body) = optUnder t0 f
     in
      (t1, Lambda (keepExprCont t1 tag body f))
  Apply f x ->
    let
      (t1, f') = optExpr t0 f
      (t2, x') = optExpr t1 x
     in
      case f' of
        Lambda g -> optLet t2 x' g
        _ -> (t2, Apply f' x')
  If c t e ->
    let
      (t1, c') = optExpr t0 c
     in
      case c' of
        Literal (ValueBool True) -> optExpr t1 t
        Literal (ValueBool False) -> optExpr t1 e
        _ ->
          let
            (t2, t') = optExpr t1 t
            (t3, e') = optExpr t2 e
           in
            (t3, If c' t' e')
  OptionCase o n s ->
    let
      (t1, o') = optExpr t0 o
     in
      case peelOption o' of
        Just Nothing -> optExpr t1 n
        Just (Just x) ->
          let
            (t2, tag, body) = optUnder t1 s
           in
            elimLetFrom t2 x s tag body
        Nothing ->
          let
            (t2, n') = optExpr t1 n
            (t3, tag, body) = optUnder t2 s
           in
            (t3, OptionCase o' n' (keepExprCont t3 tag body s))
  ResultOk x -> fmap ResultOk (optExpr t0 x)
  ResultErr x -> fmap ResultErr (optExpr t0 x)
  ResultCase o e s ->
    let
      (t1, o') = optExpr t0 o
     in
      case peelResult o' of
        Just (Left x) ->
          let
            (t2, tag, body) = optUnder t1 e
           in
            elimLetFrom t2 x e tag body
        Just (Right x) ->
          let
            (t2, tag, body) = optUnder t1 s
           in
            elimLetFrom t2 x s tag body
        Nothing ->
          let
            (t2, tE, e') = optUnder t1 e
            (t3, tS, s') = optUnder t2 s
           in
            (t3, ResultCase o' (keepExprCont t3 tE e' e) (keepExprCont t3 tS s' s))
  Index arr idx ->
    let
      (t1, arr') = optExpr t0 arr
      (t2, idx') = optExpr t1 idx
     in
      (t2, foldIndex arr' idx')
  U8Index buf idx ->
    let
      (t1, buf') = optExpr t0 buf
      (t2, idx') = optExpr t1 idx
     in
      (t2, U8Index buf' idx')
  Error x -> fmap Error (optExpr t0 x)
  Std s -> optStd t0 s
  FnLit body ->
    let
      (t1, tags, expr', body0) = optUnderFn t0 body
     in
      (t1, FnLit (keepFnCont tags expr' body0))
  UnsafeNullable x -> fmap UnsafeNullable (optExpr t0 x)
  FrozenLit fs ->
    let
      (t1, fs') = mapAccumField t0 fs
     in
      (t1, FrozenLit fs')
  GetField @k o ->
    let
      (t1, o') = optExpr t0 o
     in
      case foldGetField @k o' of
        Just e -> optExpr t1 e
        Nothing -> (t1, GetField @k o')

optMapped ::
  ( Expr Stamp ('Array u)
    -> (Stamp u -> Expr Stamp b)
    -> Expr Stamp c
  )
  -> Int
  -> Expr Stamp ('Array u)
  -> (Stamp u -> Expr Stamp b)
  -> (Int, Expr Stamp c)
optMapped k t0 x f =
  let
    (t1, x') = optExpr t0 x
    (t2, tag, body) = optUnder t1 f
   in
    (t2, k x' (keepExprCont t2 tag body f))

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
  -> (Int, Expr Stamp v)
optReduced k t0 x z f =
  let
    (t1, x') = optExpr t0 x
    (t2, z') = optExpr t1 z
    (t3, tA, tB, body) = optUnder2 t2 f
   in
    (t3, k x' z' (keepExprCont2 t3 tA tB body f))

optToSorted ::
  ( Expr Stamp ('Array u)
    -> (Stamp u -> Stamp u -> Expr Stamp 'Number)
    -> Expr Stamp ('Array u)
  )
  -> Int
  -> Expr Stamp ('Array u)
  -> (Stamp u -> Stamp u -> Expr Stamp 'Number)
  -> (Int, Expr Stamp ('Array u))
optToSorted k t0 x f =
  let
    (t1, x') = optExpr t0 x
    (t2, tA, tB, body) = optUnder2 t1 f
   in
    (t2, k x' (keepExprCont2 t2 tA tB body f))

optStd :: Int -> Std Stamp u -> (Int, Expr Stamp u)
optStd t0 = \case
  Fixed op args -> optFixed t0 op args
  Method m -> optMethod t0 m
  Kernel k -> optKernel t0 k

optKernel :: Int -> Kernel Stamp u -> (Int, Expr Stamp u)
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
  KConcat x y ->
    let
      (t1, x') = optExpr t0 x
      (t2, y') = optExpr t1 y
     in
      (t2, foldConcat x' y')
  KShow x ->
    let
      (t1, x') = optExpr t0 x
     in
      (t1, foldShow x')
  KTypeOf x ->
    let
      (t1, x') = optExpr t0 x
     in
      (t1, foldTypeOf x')
  KAnd x y ->
    let
      (t1, x') = optExpr t0 x
     in
      case x' of
        Literal (ValueBool False) -> (t1, Literal (ValueBool False))
        Literal (ValueBool True) -> optExpr t1 y
        _ ->
          let
            (t2, y') = optExpr t1 y
           in
            (t2, foldAnd x' y')
  KOr x y ->
    let
      (t1, x') = optExpr t0 x
     in
      case x' of
        Literal (ValueBool True) -> (t1, Literal (ValueBool True))
        Literal (ValueBool False) -> optExpr t1 y
        _ ->
          let
            (t2, y') = optExpr t1 y
           in
            (t2, foldOr x' y')
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

optMethod :: Int -> Method Stamp u -> (Int, Expr Stamp u)
optMethod t0 = \case
  MethMap x f -> optMapped (\a g -> Std (Method (MethMap a g))) t0 x f
  MethFilter x f -> optMapped (\a g -> Std (Method (MethFilter a g))) t0 x f
  MethReduce x z f -> optReduced (\a b g -> Std (Method (MethReduce a b g))) t0 x z f
  MethReduceRight x z f -> optReduced (\a b g -> Std (Method (MethReduceRight a b g))) t0 x z f
  MethToSorted x f -> optToSorted (\a g -> Std (Method (MethToSorted a g))) t0 x f
  MethFrom n f ->
    let
      (t1, n') = optExpr t0 n
      (t2, tag, body) = optUnder t1 f
     in
      (t2, Std (Method (MethFrom n' (keepExprCont t2 tag body f))))

optEffect :: Int -> Effect Stamp u -> (Int, Effect Stamp u)
optEffect t0 = \case
  Lift x ->
    let
      (t1, x') = optExpr t0 x
     in
      case x' of
        Var (EmbedEff e) -> optEffect t1 e
        _ -> (t1, Lift x')
  FFI n args -> fmap (FFI n) (optArgs t0 args)
  UnsafeObject o -> (t0, UnsafeObject o)
  UnsafeObjectGet x s ->
    let
      (t1, x') = optEffect t0 x
     in
      (t1, UnsafeObjectGet x' s)
  UnsafeObjectAssign x y ->
    let
      (t1, x') = optEffect t0 x
      (t2, y') = optEffect t1 y
     in
      (t2, UnsafeObjectAssign x' y')
  CallMethod x n args ->
    let
      (t1, x') = optEffect t0 x
      (t2, args') = optArgs t1 args
     in
      (t2, CallMethod x' n args')
  Bind x f -> optBind t0 x f
  BindRec r b ->
    let
      tag = t0
      (t1, r') = optEffect (t0 - optStep) (r (Stamp tag))
      (t2, b') = optEffect t1 (b (Stamp tag))
     in
      (t2, BindRec (keepEffCont t2 tag r' r) (keepEffCont t2 tag b' b))
  LambdaE f ->
    let
      (t1, tag, body) = optUnderE t0 f
     in
      (t1, LambdaE (keepEffCont t1 tag body f))
  ApplyE f x ->
    let
      (t1, f') = optEffect t0 f
      (t2, x') = optEffect t1 x
     in
      case f' of
        LambdaE g -> optBind t2 x' g
        _ -> (t2, ApplyE f' x')
  IfE c t e ->
    let
      (t1, c') = optEffect t0 c
     in
      case peelBoolEffect c' of
        Just True -> optEffect t1 t
        Just False -> optEffect t1 e
        Nothing ->
          let
            (t2, t') = optEffect t1 t
            (t3, e') = optEffect t2 e
           in
            (t3, IfE c' t' e')
  While c b ->
    let
      (t1, c') = optEffect t0 c
     in
      case peelBoolEffect c' of
        Just False -> (t1, Lift (Literal ValueUnit))
        _ ->
          let
            (t2, b') = optEffect t1 b
           in
            (t2, While c' b')
  ForRange s e b ->
    let
      (t1, s') = optExpr t0 s
      (t2, e') = optExpr t1 e
      (t3, tag, body) = optUnderE t2 b
     in
      (t3, ForRange s' e' (keepEffCont t3 tag body b))
  U8Set b i v ->
    let
      (t1, b') = optExpr t0 b
      (t2, i') = optExpr t1 i
      (t3, v') = optExpr t2 v
     in
      (t3, U8Set b' i' v')
  U8Fill b v ->
    let
      (t1, b') = optExpr t0 b
      (t2, v') = optExpr t1 v
     in
      (t2, U8Fill b' v')
  OptionCaseE o n s ->
    let
      (t1, o') = optExpr t0 o
     in
      case peelOption o' of
        Just Nothing -> optEffect t1 n
        Just (Just x) ->
          let
            (t2, tag, body) = optUnderE t1 s
           in
            elimBindFrom t2 (Lift x) s tag body
        Nothing ->
          let
            (t2, n') = optEffect t1 n
            (t3, tag, body) = optUnderE t2 s
           in
            (t3, OptionCaseE o' n' (keepEffCont t3 tag body s))
  ResultCaseE o e s ->
    let
      (t1, o') = optExpr t0 o
     in
      case peelResult o' of
        Just (Left x) ->
          let
            (t2, tag, body) = optUnderE t1 e
           in
            elimBindFrom t2 (Lift x) e tag body
        Just (Right x) ->
          let
            (t2, tag, body) = optUnderE t1 s
           in
            elimBindFrom t2 (Lift x) s tag body
        Nothing ->
          let
            (t2, tE, e') = optUnderE t1 e
            (t3, tS, s') = optUnderE t2 s
           in
            (t3, ResultCaseE o' (keepEffCont t3 tE e' e) (keepEffCont t3 tS s' s))
  StringCaseE o arms d ->
    let
      (t1, o') = optExpr t0 o
     in
      case peelString o' of
        Just k -> optEffect t1 (fromMaybe d (lookup k arms))
        Nothing ->
          let
            (t2, arms') = mapAccumArms t1 arms
            (t3, d') = optEffect t2 d
           in
            (t3, StringCaseE o' arms' d')
  Throw x ->
    let
      (t1, x') = optExpr t0 x
     in
      (t1, Throw x')
  Try a k ->
    let
      (t1, a') = optEffect t0 a
      (t2, tag, body) = optUnderE t1 k
     in
      (t2, Try a' (keepEffCont t2 tag body k))
  ObjectLit fs ->
    let
      (t1, fs') = mapAccumField t0 fs
     in
      (t1, ObjectLit fs')
  DeleteProp o k ->
    let
      (t1, o') = optEffect t0 o
      (t2, k') = optExpr t1 k
     in
      (t2, DeleteProp o' k')
  ArrayLit es ->
    let
      (t1, es') = mapAccumEffs t0 es
     in
      (t1, ArrayLit es')

mapAccumField :: Int -> [FieldLit Stamp r] -> (Int, [FieldLit Stamp r])
mapAccumField = mapAccumL $ \t -> \case
  FieldLit @k e ->
    let
      (t1, e') = optExpr t e
     in
      (t1, FieldLit @k e')
  FieldLitEffect @k e ->
    let
      (t1, e') = optEffect t e
     in
      (t1, FieldLitEffect @k e')
  FieldLitExtra @k e ->
    let
      (t1, e') = optExpr t e
     in
      (t1, FieldLitExtra @k e')
  FieldLitExtraEffect @k e ->
    let
      (t1, e') = optEffect t e
     in
      (t1, FieldLitExtraEffect @k e')

mapAccumEffs :: Int -> [Effect Stamp u] -> (Int, [Effect Stamp u])
mapAccumEffs = mapAccumL optEffect

mapAccumArms ::
  Int -> [(Text, Effect Stamp u)] -> (Int, [(Text, Effect Stamp u)])
mapAccumArms = mapAccumL $ \t (k, e) ->
  let
    (t1, e') = optEffect t e
   in
    (t1, (k, e'))

-- Bind of an Effect: when the continuation uses the binder once in a
-- strict position, splice the effect in place (so `x <- getEl; x.foo()`
-- becomes `getEl().foo()`); when never, keep it as a statement.
bindEffectCode ::
  CG -> Effect Stamp u -> (Stamp u -> Effect Stamp v) -> (CG, Code)
bindEffectCode s0 x f =
  let
    (tag, sTag) = allocTag s0
    tagged = f (Stamp tag)
    uses = countEffect tag tagged
    bodyOf s e = effectfulAST' s (f (maybe nestedDummy Name (liveBinder e)))
   in
    case uses of
      0 ->
        let
          (s1, MkCode xDecl xRef xFX) = effectfulAST' sTag x
          (s2, MkCode yDecl yRef yFX) = bodyOf s1 x
          -- Value-producing effects (ifE) put work in xDecl and leave
          -- a result ident in xRef (codeRefFX False). Assignments and
          -- calls keep the side effect in xRef (fxCode).
          stmt
            | P.isEmpty xRef = xDecl
            | not xFX && not (P.isEmpty xDecl) = xDecl
            | otherwise = asStmt xDecl xRef
         in
          (s2, MkCode (stmt $$ yDecl) yRef yFX)
      _ ->
        let
          (s1, MkCode xDecl xRef _) = effectfulAST' sTag x
         in
          if P.isEmpty xRef
            then
              let
                (s2, MkCode yDecl yRef yFX) = bodyOf s1 x
               in
                (s2, MkCode (xDecl $$ yDecl) yRef yFX)
            else
              let
                (nBind, s2) = allocIdent s1
                (s3, MkCode yDecl yRef yFX) = effectfulAST' s2 (f (Name nBind))
               in
                (s3, MkCode (xDecl $$ constBind nBind xRef $$ yDecl) yRef yFX)

effectfulAST :: ClosedEffect u -> Doc
effectfulAST e =
  uncurry renderWithHelpers (effectfulAST' startCG (optimizeEffect e))

-- | Witness that forces @u ~ 'Unit@: @noOp@, 'While', 'Throw', or
-- 'Bind' into those. Polymorphic nodes ('UnsafeObjectAssign',
-- 'CallMethod', 'FFI') do not count — they inhabit any @u@. Two-arm
-- forms require *both* arms unit; otherwise a 'Throw' would drop the
-- other arm's value. Statement @if@ is 'IfE' after 'JShark.Api.discard'.
isUnitWitness :: Effect Stamp u -> Bool
isUnitWitness = \case
  Lift (Literal ValueUnit) -> True
  Lift (Var (EmbedEff e)) -> isUnitWitness e
  Lift _ -> False
  While {} -> True
  ForRange {} -> True
  Bind _ f -> isUnitWitness (f nestedDummy)
  BindRec _ f -> isUnitWitness (f nestedDummy)
  IfE _ t e -> isUnitWitness t && isUnitWitness e
  OptionCaseE _ n s -> isUnitWitness n && isUnitWitness (s nestedDummy)
  ResultCaseE _ e s -> isUnitWitness (e nestedDummy) && isUnitWitness (s nestedDummy)
  StringCaseE _ arms d -> all (isUnitWitness . snd) arms && isUnitWitness d
  Throw {} -> True
  Try a k -> isUnitWitness a && isUnitWitness (k nestedDummy)
  _ -> False

-- | Turn a rendered effect into a statement. Unit values may still have a
-- non-empty ref (@el.x = v@, @foo()@); those become statements, not
-- @let n = …@.
asStmt :: Doc -> Doc -> Doc
asStmt decl ref
  | P.isEmpty ref = decl
  | otherwise = decl $$ (ref <> P.semi)

bracesNest :: Doc -> Doc
bracesNest = P.braces . P.nest 2

ifElseStmt :: Doc -> Doc -> Doc -> Doc -> Doc -> Doc
ifElseStmt cRef tDecl tRef eDecl eRef
  | P.isEmpty eDecl && P.isEmpty eRef =
      "if" <+> P.parens cRef <+> bracesNest (asStmt tDecl tRef)
  | otherwise =
      "if" <+> P.parens cRef <+> bracesNest (asStmt tDecl tRef)
        $$ "else" <+> bracesNest (asStmt eDecl eRef)

assignResult :: String -> Doc -> Doc
assignResult resultVar ref
  | P.isEmpty ref = mempty
  | otherwise = (P.text resultVar <+> "=" <+> ref) <> P.semi

letResult :: String -> Doc
letResult resultVar = ("let" <+> P.text resultVar) <> P.semi

recBindStmt :: Doc -> Doc -> Doc -> Doc
recBindStmt n rDecl rRef =
  ("let" <+> n) <> P.semi $$ rDecl $$ (n <+> "=" <+> rRef) <> P.semi

resultCasePrelude ::
  CG
  -> Expr Stamp ('Result e a)
  -> (CG, Doc, String, Int)
resultCasePrelude s0 res =
  let
    (s1, Code rDecl rRef) = pureAST' s0 res
    (nObj, s2) = allocIdent s1
    (nUnw, s3) = allocIdent s2
    obj = nName nObj
    prelude = rDecl $$ constBind nObj rRef $$ constBind nUnw (P.text obj <> ".value")
   in
    (s3, prelude, obj, nUnw)

-- | Unit arms: prelude + stmt, empty ref. Value arms: prelude +
-- @let result@ + stmt, result ident.
emitBranching ::
  Bool
  -> CG
  -> (CG -> (CG, Doc, extra))
  -> (Maybe String -> extra -> CG -> (CG, Doc))
  -> (CG, Code)
emitBranching unit s0 prelude k
  | unit =
      let
        (s1, pre, extra) = prelude s0
        (s2, stmt) = k Nothing extra s1
       in
        (s2, Code (pre $$ stmt) mempty)
  | otherwise =
      let
        (s1, pre, extra) = prelude s0
        (n, s2) = allocIdent s1
        rv = nName n
        (s3, stmt) = k (Just rv) extra s2
       in
        (s3, Code (pre $$ letResult rv $$ stmt) (P.text rv))

ifAssignOrStmt :: Maybe String -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc
ifAssignOrStmt Nothing c tD tR eD eR = ifElseStmt c tD tR eD eR
ifAssignOrStmt (Just rv) c tD tR eD eR =
  "if" <+> P.parens c <+> bracesNest (tD $$ assignResult rv tR)
    $$ "else" <+> bracesNest (eD $$ assignResult rv eR)

tryCatchStmt :: Maybe String -> Int -> Doc -> Doc -> Doc -> Doc -> Doc
tryCatchStmt mRes catchN aDecl aRef bDecl bRef =
  let
    catchHead = "catch" <+> P.parens (nDoc catchN)
   in
    case mRes of
      Nothing ->
        "try" <+> bracesNest (asStmt aDecl aRef)
          $$ (catchHead <+> bracesNest (asStmt bDecl bRef))
      Just rv ->
        "try" <+> P.braces (P.nest 2 (aDecl $$ assignResult rv aRef))
          $$ (catchHead <+> P.braces (P.nest 2 (bDecl $$ assignResult rv bRef)))

renderFunction :: Int -> Doc -> Doc -> Doc
renderFunction nParam decl ref =
  "function"
    <+> P.parens (nDoc nParam)
    <+> P.braces (decl $$ ret)
 where
  -- Empty ref is Unit (event handlers, forEach of noOp). `return ()`
  -- is a SyntaxError; HughesPJ `parens` of empty is `()`.
  ret
    | P.isEmpty ref = "return"
    | otherwise = "return" <+> P.parens ref

-- | @function (n0, n1) { decls return ref }@ — callback style (bare return).
jsCallback :: [Doc] -> Doc -> Doc -> Doc
jsCallback params decl ref =
  "function"
    <+> P.parens (P.hcat (P.punctuate ", " params))
    <+> P.braces (decl $$ "return" <+> ref)

renderFFIForm :: FFIForm -> Doc
renderFFIForm = \case
  FFICall s -> P.text s
  FFILambda s -> P.parens (P.text s)

effectfulAST' :: forall v. CG -> Effect Stamp v -> (CG, Code)
effectfulAST' !s0 = \case
  Lift x -> pureAST' s0 x
  FFI fn args ->
    let
      (s1, argDecl, argRefs) = renderArgList argAST s0 args
     in
      (s1, fxCode argDecl (renderFFIForm fn <> P.parens argRefs))
  IfE c t e ->
    -- Value-producing @if@: a shared result var is assigned in both
    -- arms. Do not use emptiness to pick a ternary — a Unit leftover
    -- ref is not a genuinely-empty Doc.
    emitBranching
      (isUnitWitness t && isUnitWitness e)
      s0
      ( \s ->
          let
            (s1, Code cDecl cRef) = effectfulAST' s c
           in
            (s1, cDecl, cRef)
      )
      ( \mRes cRef s ->
          let
            (s1, Code tDecl tRef) = effectfulAST' s t
            (s2, Code eDecl eRef) = effectfulAST' s1 e
           in
            (s2, ifAssignOrStmt mRes cRef tDecl tRef eDecl eRef)
      )
  While cond body ->
    let
      (s1, Code condDecl condRef) = effectfulAST' s0 cond
      (s2, Code bodyDecl bodyRef) = effectfulAST' s1 body
      bodyStmt = if P.isEmpty bodyRef then bodyDecl else bodyDecl $$ (bodyRef <> P.semi)
      whileStmt = "while" <+> P.parens condRef <+> P.braces (P.nest 2 bodyStmt)
     in
      (s2, Code (condDecl $$ whileStmt) mempty)
  ForRange start end body ->
    let
      (s1, Code startDecl startRef) = pureAST' s0 start
      (s2, Code endDecl endRef) = pureAST' s1 end
      (loopN, s3) = allocIdent s2
      loopVar = nDoc loopN
      (s4, Code bodyDecl bodyRef) = effectfulAST' s3 (body (Name loopN))
      bodyStmt = if P.isEmpty bodyRef then bodyDecl else bodyDecl $$ (bodyRef <> P.semi)
      forHead =
        "let"
          <+> loopVar
          <+> "="
          <+> startRef
          <+> ";"
          <+> loopVar
          <+> "<"
          <+> endRef
          <+> ";"
          <+> loopVar
          <+> "++"
      forStmt = "for" <+> P.parens forHead <+> P.braces (P.nest 2 bodyStmt)
     in
      (s4, Code (startDecl $$ endDecl $$ forStmt) mempty)
  U8Set buf idx val ->
    let
      (s1, Code bDecl bRef) = pureAST' s0 buf
      (s2, Code iDecl iRef) = pureAST' s1 idx
      (s3, Code vDecl vRef) = pureAST' s2 val
      stmt = (bRef <> P.brackets iRef) <+> "=" <+> vRef
     in
      (s3, Code (bDecl $$ iDecl $$ vDecl $$ (stmt <> P.semi)) mempty)
  U8Fill buf val ->
    let
      (s1, Code bDecl bRef) = pureAST' s0 buf
      (s2, Code vDecl vRef) = pureAST' s1 val
      stmt = bRef <> ".fill" <> P.parens vRef
     in
      (s2, Code (bDecl $$ vDecl $$ (stmt <> P.semi)) mempty)
  OptionCaseE opt noneE someF ->
    emitBranching
      (isUnitWitness noneE && isUnitWitness (someF nestedDummy))
      s0
      ( \s ->
          let
            (s1, Code oDecl oRef) = pureAST' s opt
            (nBind, s2) = allocIdent s1
           in
            (s2, oDecl $$ constBind nBind oRef, nBind)
      )
      ( \mRes nBind s ->
          let
            (s1, Code nDecl nRef) = effectfulAST' s noneE
            (s2, Code sDecl sRef) = effectfulAST' s1 (someF (Name nBind))
            cond = nDoc nBind <+> "===" <+> "null"
           in
            (s2, ifAssignOrStmt mRes cond nDecl nRef sDecl sRef)
      )
  Try a k ->
    emitBranching
      (isUnitWitness a && isUnitWitness (k nestedDummy))
      s0
      (\s -> (s, mempty, ()))
      ( \mRes () s ->
          let
            (s1, Code aDecl aRef) = effectfulAST' s a
            (catchN, s2) = allocIdent s1
            (s3, Code bDecl bRef) = effectfulAST' s2 (k (Name catchN))
           in
            (s3, tryCatchStmt mRes catchN aDecl aRef bDecl bRef)
      )
  Bind x f -> bindEffectCode s0 x f
  BindRec r b ->
    let
      (nBind, s1) = allocIdent s0
      n = nDoc nBind
      (s2, MkCode rDecl rRef _) = effectfulAST' s1 (r (Name nBind))
      (s3, MkCode bDecl bRef bFX) = effectfulAST' s2 (b (Name nBind))
     in
      (s3, MkCode (recBindStmt n rDecl rRef $$ bDecl) bRef bFX)
  Throw x ->
    let
      (s1, Code xDecl xRef) = pureAST' s0 x
     in
      (s1, Code (xDecl $$ (("throw" <+> xRef) <> P.semi)) mempty)
  ObjectLit fs -> renderObjectLit s0 fs
  ArrayLit es -> renderArrayLit s0 es
  DeleteProp o k ->
    let
      (s1, Code oDecl oRef) = effectfulAST' s0 o
      (s2, Code kDecl kRef) = pureAST' s1 k
     in
      (s2, fxCode (oDecl $$ kDecl) (("delete" <+> oRef) <> P.brackets kRef))
  ResultCaseE res errF okF -> renderResultCaseE s0 res errF okF
  StringCaseE scrut arms def -> renderStringCaseE s0 scrut arms def
  UnsafeObject obj -> (s0, Code mempty $ P.text $ T.unpack obj)
  UnsafeObjectGet x string ->
    let
      (s1, Code x1Decl x1Ref) = effectfulAST' s0 x
     in
      (s1, Code x1Decl $ jsDotOrBracket x1Ref string)
  UnsafeObjectAssign x y ->
    let
      (s1, Code x1Decl x1Ref) = effectfulAST' s0 x
      (s2, Code y1Decl y1Ref) = effectfulAST' s1 y
     in
      (s2, fxCode (x1Decl $$ y1Decl) $ x1Ref <> " = " <> y1Ref)
  CallMethod recv name args ->
    let
      (s1, Code rDecl rRef) = effectfulAST' s0 recv
      (s2, argDecl, argRefs) = renderArgList argAST s1 args
     in
      (s2, fxCode (rDecl $$ argDecl) (rRef <> "." <> P.text name <> P.parens argRefs))
  LambdaE f -> emitEffectLambda s0 f
  ApplyE fex ex ->
    let
      (s1, Code exprXDecl exprXRef) = effectfulAST' s0 fex
      (s2, Code exprYDecl exprYRef) = effectfulAST' s1 ex
     in
      (s2, fxCode (exprXDecl $$ exprYDecl) (jsCall exprXRef exprYRef))

letCode :: CG -> Expr Stamp u -> (Stamp u -> Expr Stamp v) -> (CG, Code)
letCode s0 x g =
  let
    (tag, sTag) = allocTag s0
    tagged = g (Stamp tag)
    uses = countExpr tag tagged
   in
    case uses of
      0 ->
        let
          (s1, MkCode xDecl xRef _) = pureAST' sTag x
          (s2, y) = pureAST' s1 (g nestedDummy)
          stmt
            | P.isEmpty xDecl && not (P.isEmpty xRef) = xRef <> P.semi
            | otherwise = xDecl
         in
          (s2, keepRef (stmt $$ codeDecl y) y)
      _ ->
        let
          (s1, MkCode xDecl xRef _) = pureAST' sTag x
          (nBind, s2) = allocIdent s1
          (s3, y) = pureAST' s2 (g (Name nBind))
         in
          (s3, keepRef (xDecl $$ constBind nBind xRef $$ codeDecl y) y)

pureAST :: ClosedExpr u -> Doc
pureAST e = uncurry renderWithHelpers (pureAST' startCG (optimize e))

pureAST' ::
  forall v.
  CG
  -> Expr Stamp v
  -> (CG, Code)
pureAST' !s0 = \case
  Literal v -> case v of
    ValueNumber d -> (s0, Code mempty (P.text $ showFFloat Nothing d ""))
    ValueArray xs ->
      let
        (s1, exprs) = mapAccumL (\s x -> pureAST' s (Literal x)) s0 xs
        (exprDecls, exprRefs) = partitionCode exprs
       in
        ( s1
        , Code
            (P.vcat exprDecls)
            (P.brackets (P.hcat (P.punctuate ", " (map arrayElemRef exprRefs))))
        )
    ValueString s -> (s0, Code mempty (jsQuote s))
    ValueFunction _ -> error "JShark.pureAST: ValueFunction is eval-only"
    ValueUnit -> (s0, mempty)
    ValueOption (Just x) -> pureAST' s0 (Literal x)
    ValueOption Nothing -> (s0, Code mempty "null")
    ValueResult (Right x) -> renderResultLit True s0 x
    ValueResult (Left x) -> renderResultLit False s0 x
    ValueRegex s ->
      (s0, Code mempty ("new RegExp" <> P.parens (jsQuote s)))
    ValueUint8Array ba -> (s0, Code mempty (jsUint8ArrayLit ba))
    ValueBool True -> (s0, Code mempty "true")
    ValueBool False -> (s0, Code mempty "false")
    ValueFrozen {} -> error "JShark.pureAST: ValueFrozen is eval-only"
  Lambda f -> emitExprLambda s0 f
  -- `const` when shared or used under a lambda/loop/short-circuit.
  Let x g -> letCode s0 x g
  LetRec r b ->
    let
      (nBind, s1) = allocIdent s0
      n = nDoc nBind
      (s2, MkCode rDecl rRef _) = pureAST' s1 (r (Name nBind))
      (s3, bCode) = pureAST' s2 (b (Name nBind))
     in
      (s3, keepRef (recBindStmt n rDecl rRef $$ codeDecl bCode) bCode)
  Apply fex ex ->
    let
      (s1, Code exprXDecl exprXRef) = pureAST' s0 fex
      (s2, Code exprYDecl exprYRef) = pureAST' s1 ex
     in
      (s2, Code (exprXDecl $$ exprYDecl) (jsCall exprXRef exprYRef))
  Var (Embed e) -> pureAST' s0 (flattenExpr e)
  Var (EmbedEff e) -> effectfulAST' s0 e
  Var s
    -- Tags and the unused-binder dummy are negative; never emit them as JS.
    | stampId s < 0 -> (s0, Code mempty mempty)
    | otherwise -> (s0, Code mempty $ nDoc (stampId s))
  If c t e ->
    let
      (s1, Code cDecl cRef) = pureAST' s0 c
      (s2, Code tDecl tRef) = pureAST' s1 t
      (s3, Code eDecl eRef) = pureAST' s2 e
     in
      ( s3
      , Code
          (cDecl $$ tDecl $$ eDecl)
          (P.parens (cRef <+> "?" <+> tRef <+> ":" <+> eRef))
      )
  OptionCase opt none' someF ->
    case opt of
      Var (Embed e) -> pureAST' s0 (OptionCase (flattenExpr e) none' someF)
      Var s ->
        let
          i = stampId s
          optVar = nName i
          (s2, Code noneDecl noneRef) = pureAST' s0 none'
          (s3, Code someDecl someRef) = pureAST' s2 (someF (Name i))
         in
          ( s3
          , Code
              (noneDecl $$ someDecl)
              ( P.parens
                  (P.text optVar <+> "===" <+> "null" <+> "?" <+> noneRef <+> ":" <+> someRef)
              )
          )
      _ ->
        let
          (s1, Code optDecl optRef) = pureAST' s0 opt
          (nBind, s2) = allocIdent s1
          optVar = nName nBind
          (s3, Code noneDecl noneRef) = pureAST' s2 none'
          (s4, Code someDecl someRef) = pureAST' s3 (someF (Name nBind))
         in
          ( s4
          , Code
              (optDecl $$ constBind nBind optRef $$ noneDecl $$ someDecl)
              ( P.parens
                  (P.text optVar <+> "===" <+> "null" <+> "?" <+> noneRef <+> ":" <+> someRef)
              )
          )
  ResultOk x ->
    let
      (s1, Code d r) = pureAST' s0 x
     in
      (s1, Code d (resultObject True r))
  ResultErr x ->
    let
      (s1, Code d r) = pureAST' s0 x
     in
      (s1, Code d (resultObject False r))
  ResultCase res errF okF -> renderResultCase s0 res errF okF
  Index arr idx ->
    let
      (s1, Code aDecl aRef) = pureAST' s0 arr
      (s2, Code iDecl iRef) = pureAST' s1 idx
     in
      (s2, Code (aDecl $$ iDecl) (jsCheckedIndex aRef iRef))
  U8Index buf idx ->
    let
      (s1, Code bDecl bRef) = pureAST' s0 buf
      (s2, Code iDecl iRef) = pureAST' s1 idx
     in
      (s2, Code (bDecl $$ iDecl) (bRef <> P.brackets iRef))
  Error msg ->
    let
      (s1, Code d r) = pureAST' s0 msg
     in
      (s1, Code d ("(function(){throw new Error(" <> r <> ");}())"))
  Std s -> renderStd s0 s
  FnLit body -> renderFn s0 body
  UnsafeNullable x -> pureAST' s0 x
  FrozenLit fs -> renderObjectLit s0 fs
  GetField @k o ->
    let
      (s1, Code d r) = pureAST' s0 o
     in
      (s1, Code d (jsDotOrBracket r (symbolVal (Proxy @k))))

renderFixed ::
  CG
  -> FixedOp a b c u
  -> FixedArgs Stamp a b c
  -> (CG, Code)
renderFixed s0 op args = case (op, args) of
  (n, ArgsU x)
    | Just name <- Prim.math1Name n ->
        let
          (s1, Code xDecl xRef) = pureAST' s0 x
         in
          (s1, Code xDecl ("Math." <> P.text (T.unpack name) <> P.parens xRef))
  (n, ArgsB x y)
    | Just name <- Prim.math2Name n ->
        let
          (s1, Code xDecl xRef) = pureAST' s0 x
          (s2, Code yDecl yRef) = pureAST' s1 y
         in
          ( s2
          , Code
              (xDecl $$ yDecl)
              ( "Math."
                  <> P.text (T.unpack name)
                  <> P.parens (xRef <> ", " <> yRef)
              )
          )
  (n, ArgsU recv) ->
    let
      (s1, Code rDecl rRef) = pureAST' s0 recv
     in
      (s1, Code rDecl (Prim.fixedUnaryJS n (wrapOperand recv rRef)))
  (n, ArgsB recv arg) ->
    let
      (s1, Code rDecl rRef) = pureAST' s0 recv
      (s2, Code aDecl aRef) = pureAST' s1 arg
     in
      (s2, Code (rDecl $$ aDecl) (Prim.fixedBinaryJS n (wrapOperand recv rRef) aRef))
  (n, ArgsT recv a b) ->
    let
      (s1, Code rDecl rRef) = pureAST' s0 recv
      (s2, Code aDecl aRef) = pureAST' s1 a
      (s3, Code bDecl bRef) = pureAST' s2 b
     in
      ( s3
      , Code
          (rDecl $$ aDecl $$ bDecl)
          (Prim.fixedTernaryJS n (wrapOperand recv rRef) aRef bRef)
      )

resultPayloadRef :: Doc -> Doc
resultPayloadRef r
  | P.isEmpty r = "undefined"
  | otherwise = r

resultObject :: Bool -> Doc -> Doc
resultObject isOk payload =
  let
    flag = if isOk then "true" else "false"
   in
    P.braces ((("ok:" <+> flag) <> ",") <+> ("value:" <+> resultPayloadRef payload))

renderResultLit :: Bool -> CG -> Value u -> (CG, Code)
renderResultLit isOk s0 x =
  let
    (s1, Code d r) = pureAST' s0 (Literal x)
   in
    (s1, Code d (resultObject isOk r))

renderArrayLit :: CG -> [Effect Stamp u] -> (CG, Code)
renderArrayLit s0 es =
  let
    (s1, cs) = mapAccumL effectfulAST' s0 es
    (decls, refs) = partitionCode cs
   in
    (s1, Code (P.vcat decls) (P.brackets (P.hcat (P.punctuate ", " refs))))

renderObjectLit :: CG -> [FieldLit Stamp r] -> (CG, Code)
renderObjectLit s0 fs =
  let
    (s1, parts) =
      mapAccumL
        ( \s fl ->
            case fl of
              FieldLit e ->
                let
                  (s', Code d r) = pureAST' s e
                 in
                  (s', (d, (P.doubleQuotes (P.text (fieldKey fl)) <> ":") <+> r))
              FieldLitExtra e ->
                let
                  (s', Code d r) = pureAST' s e
                 in
                  (s', (d, (P.doubleQuotes (P.text (fieldKey fl)) <> ":") <+> r))
              FieldLitEffect e ->
                let
                  (s', MkCode d r _) = effectfulAST' s e
                 in
                  (s', (d, (P.doubleQuotes (P.text (fieldKey fl)) <> ":") <+> r))
              FieldLitExtraEffect e ->
                let
                  (s', MkCode d r _) = effectfulAST' s e
                 in
                  (s', (d, (P.doubleQuotes (P.text (fieldKey fl)) <> ":") <+> r))
        )
        s0
        fs
    (declList, pairs) = unzip parts
   in
    (s1, Code (P.vcat declList) (P.braces (P.hcat (P.punctuate ", " pairs))))

renderResultCase ::
  CG
  -> Expr Stamp ('Result e a)
  -> (Stamp e -> Expr Stamp v)
  -> (Stamp a -> Expr Stamp v)
  -> (CG, Code)
renderResultCase s0 res errF okF =
  let
    (s1, prelude, obj, nUnw) = resultCasePrelude s0 res
    (s2, Code eDecl eRef) = pureAST' s1 (errF (Name nUnw))
    (s3, Code oDecl oRef) = pureAST' s2 (okF (Name nUnw))
   in
    ( s3
    , Code
        (prelude $$ eDecl $$ oDecl)
        (P.parens ((P.text obj <> ".ok") <+> "?" <+> oRef <+> ":" <+> eRef))
    )

renderStringCaseE ::
  CG
  -> Expr Stamp 'String
  -> [(Text, Effect Stamp v)]
  -> Effect Stamp v
  -> (CG, Code)
renderStringCaseE s0 scrut arms def =
  let
    unit = all (isUnitWitness . snd) arms && isUnitWitness def
    (s1, Code oDecl oRef) = pureAST' s0 scrut
    (resultN, s2) =
      if unit then (0, s1) else allocIdent s1
    resultVar = nName resultN
    renderArm s e =
      let
        (s', Code d r) = effectfulAST' s e
        body = if unit then asStmt d r else d $$ assignResult resultVar r
       in
        (s', body)
    (s3, caseDocs) =
      mapAccumL
        ( \s (k, e) ->
            let
              (s', body) = renderArm s e
              line =
                "case" <+> (jsQuote k <> P.colon) <+> bracesNest (body <+> ("break" <> P.semi))
             in
              (s', line)
        )
        s2
        arms
    (s4, defBody) = renderArm s3 def
    defDoc = "default:" <+> bracesNest defBody
    switchStmt = "switch" <+> P.parens oRef <+> bracesNest (P.vcat (caseDocs ++ [defDoc]))
    prelude
      | unit = oDecl
      | otherwise = oDecl $$ letResult resultVar
    ref = if unit then mempty else P.text resultVar
   in
    (s4, Code (prelude $$ switchStmt) ref)

renderResultCaseE ::
  CG
  -> Expr Stamp ('Result e a)
  -> (Stamp e -> Effect Stamp v)
  -> (Stamp a -> Effect Stamp v)
  -> (CG, Code)
renderResultCaseE s0 res errF okF
  | isUnitWitness (errF nestedDummy) && isUnitWitness (okF nestedDummy) =
      let
        (s1, prelude, obj, nUnw) = resultCasePrelude s0 res
        (s2, Code eDecl eRef) = effectfulAST' s1 (errF (Name nUnw))
        (s3, Code oDecl oRef) = effectfulAST' s2 (okF (Name nUnw))
       in
        ( s3
        , Code (prelude $$ ifElseStmt (P.text obj <> ".ok") oDecl oRef eDecl eRef) mempty
        )
  | otherwise =
      let
        (s1, prelude, obj, nUnw) = resultCasePrelude s0 res
        (resultN, s2) = allocIdent s1
        resultVar = nName resultN
        (s3, Code eDecl eRef) = effectfulAST' s2 (errF (Name nUnw))
        (s4, Code oDecl oRef) = effectfulAST' s3 (okF (Name nUnw))
        stmt =
          prelude
            $$ letResult resultVar
            $$ ifElseStmt
              (P.text obj <> ".ok")
              (oDecl $$ assignResult resultVar oRef)
              mempty
              (eDecl $$ assignResult resultVar eRef)
              mempty
       in
        (s4, Code stmt (P.text resultVar))

emitExprLambda :: CG -> (Stamp u -> Expr Stamp v) -> (CG, Code)
emitExprLambda = emitLambdaWith pureAST'

emitEffectLambda :: CG -> (Stamp u -> Effect Stamp v) -> (CG, Code)
emitEffectLambda = emitLambdaWith effectfulAST'

emitLambdaWith ::
  (CG -> t -> (CG, Code))
  -> CG
  -> (Stamp u -> t)
  -> (CG, Code)
emitLambdaWith walker s0 f =
  let
    (nParam, s1) = allocIdent s0
    (s2, Code exprXDecl exprXRef) = walker s1 (f (Name nParam))
   in
    (s2, Code mempty (renderFunction nParam exprXDecl exprXRef))

renderBinaryFn ::
  CG
  -> (Stamp a -> Stamp b -> Expr Stamp c)
  -> (CG, Doc)
renderBinaryFn s0 f =
  let
    (s1, Code _ cb) = renderFn s0 (JfCons $ \a -> JfCons $ \b -> JfNil (f a b))
   in
    (s1, cb)

renderStd :: CG -> Std Stamp u -> (CG, Code)
renderStd s0 = \case
  Fixed op args -> renderFixed s0 op args
  Method m -> renderMethod s0 m
  Kernel k -> renderKernel s0 k

renderKernel :: CG -> Kernel Stamp u -> (CG, Code)
renderKernel s0 = \case
  KConcat x y -> renderBin "+" s0 x y
  KPlus x y -> renderBin "+" s0 x y
  KMinus x y -> renderBin "-" s0 x y
  KTimes x y -> renderBin "*" s0 x y
  KFracDiv x y -> renderBin "/" s0 x y
  KRem x y -> renderBin "%" s0 x y
  KBitAnd x y -> renderBin "&" s0 x y
  KBitOr x y -> renderBin "|" s0 x y
  KBitXor x y -> renderBin "^" s0 x y
  KShl x y -> renderBin "<<" s0 x y
  KShr x y -> renderBin ">>" s0 x y
  KUShr x y -> renderBin ">>>" s0 x y
  KShow x ->
    let
      (s1, Code x1Decl x1Ref) = pureAST' s0 x
     in
      (s1, Code x1Decl $ "String" <> P.parens x1Ref)
  KTypeOf x ->
    let
      (s1, Code x1Decl x1Ref) = pureAST' s0 x
      wrapped = case x of
        FrozenLit {} -> P.parens x1Ref
        _ -> x1Ref
     in
      (s1, Code x1Decl $ "typeof" <+> wrapped)
  KNegate x ->
    let
      (s1, Code x1Decl x1Ref) = pureAST' s0 x
     in
      (s1, Code x1Decl $ "-" <> P.parens x1Ref)
  KAnd x y -> renderBin "&&" s0 x y
  KOr x y -> renderBin "||" s0 x y
  KEq structural x y
    | structural ->
        renderBinApp jsValueEq (useEqHelpers s0) x y
    | otherwise ->
        renderBin "===" s0 x y
  KNEq structural x y
    | structural ->
        renderBinApp jsValueNEq (useEqHelpers s0) x y
    | otherwise ->
        renderBin "!==" s0 x y
  KGTh x y -> renderBin ">" s0 x y
  KLTh x y -> renderBin "<" s0 x y
  KGTEq x y -> renderBin ">=" s0 x y
  KLTEq x y -> renderBin "<=" s0 x y

renderMethod :: CG -> Method Stamp u -> (CG, Code)
renderMethod s0 = \case
  MethMap recv f -> renderCallbackMethod "map" s0 recv f
  MethFilter recv f -> renderCallbackMethod "filter" s0 recv f
  MethReduce recv z f -> renderFold ".reduce" s0 recv z f
  MethReduceRight recv z f -> renderFold ".reduceRight" s0 recv z f
  MethToSorted recv f ->
    let
      (s1, Code rDecl rRef) = pureAST' s0 recv
      (s2, cb) = renderBinaryFn s1 f
      call = wrapOperand recv rRef <> ".toSorted" <> P.parens cb
     in
      (s2, Code rDecl call)
  MethFrom n f ->
    let
      (s1, Code nDecl nRef) = pureAST' s0 n
      (nHole, s2) = allocIdent s1
      (nI, s3) = allocIdent s2
      (s4, Code exDecl exRef) = pureAST' s3 (f (Name nI))
      cb = jsCallback [nDoc nHole, nDoc nI] exDecl exRef
     in
      (s4, Code nDecl ("Array.from({length: " <> nRef <> "}, " <> cb <> ")"))

renderFold ::
  String
  -> CG
  -> Expr Stamp ('Array u)
  -> Expr Stamp v
  -> (Stamp v -> Stamp u -> Expr Stamp v)
  -> (CG, Code)
renderFold method s0 recv z f =
  let
    (s1, Code rDecl rRef) = pureAST' s0 recv
    (s2, Code zDecl zRef) = pureAST' s1 z
    (s3, cb) = renderBinaryFn s2 f
    call = wrapOperand recv rRef <> P.text method <> P.parens (cb <> ", " <> zRef)
   in
    (s3, Code (rDecl $$ zDecl) call)

renderCallbackMethod ::
  String
  -> CG
  -> Expr Stamp a
  -> (Stamp b -> Expr Stamp c)
  -> (CG, Code)
renderCallbackMethod name s0 recv f =
  let
    (s1, Code rDecl rRef) = pureAST' s0 recv
    (nParam, s2) = allocIdent s1
    (s3, Code exDecl exRef) = pureAST' s2 (f (Name nParam))
    call =
      wrapOperand recv rRef
        <> "."
        <> P.text name
        <> P.parens (jsCallback [nDoc nParam] exDecl exRef)
   in
    (s3, Code rDecl call)

renderBin :: String -> CG -> Expr Stamp a -> Expr Stamp b -> (CG, Code)
renderBin op s0 x y =
  renderBinApp
    (\l r -> wrapOperand x l <+> P.text op <+> wrapOperand y r)
    s0
    x
    y

renderBinApp ::
  (Doc -> Doc -> Doc) -> CG -> Expr Stamp a -> Expr Stamp b -> (CG, Code)
renderBinApp join s0 x y =
  let
    (s1, Code xDecl xRef) = pureAST' s0 x
    (s2, Code yDecl yRef) = pureAST' s1 y
   in
    (s2, Code (xDecl $$ yDecl) (join xRef yRef))

argAST :: CG -> Arg Stamp u -> (CG, Code)
argAST s (ArgExpr e) = pureAST' s e
argAST s (ArgEffect e) = effectfulAST' s e

renderArgList ::
  (forall x. CG -> f x -> (CG, Code)) -> CG -> Rec f us -> (CG, Doc, Doc)
renderArgList f s0 args =
  let
    (s1, cs) = recCodes f s0 args
    (decls, refs) = partitionCode cs
   in
    (s1, P.vcat decls, P.hcat (P.punctuate ", " refs))
