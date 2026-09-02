{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Pure reference interpreter for closed 'Expr' terms.
module JShark.Compiler.Evaluate
  ( evaluate
  , evaluateNumber
  , evaluateBigInt
  , evaluateCached
  , valueEq
  , isCheapValue
  , mapFixedArgs
  , foldFixed
  , isFiniteDouble
  , escapeJsString
  , jsQuote
  , jsBigIntLit
  , jsUint8ArrayLit
  , bigOpJS
  , uint8Elems
  , packUint8
  , tryEvalBigBin
  , isOrderableValue
  , eqFoldableValue
  , jsShow
  , typeOfValue
  , valueCompare
  , parseBigIntString
  )
where

import Control.Monad (foldM)
import Data.Array.Byte (ByteArray (..))
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Char (digitToInt, isSpace)
import qualified Data.Char as Char
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable, eqT, type (:~:) (Refl))
import GHC.Exts
  ( Int (..)
  , indexWord8Array#
  , newByteArray#
  , sizeofByteArray#
  , unsafeFreezeByteArray#
  , writeWord8Array#
  , (+#)
  )
import GHC.ST (ST (..), runST)
import GHC.TypeLits (KnownSymbol, sameSymbol)
import GHC.Word (Word8 (..))
import JShark.Api.Prim
  ( MathBinary (..)
  , MathUnary (..)
  , isFiniteDouble
  , matchMathBinary
  , matchMathUnary
  )
import qualified JShark.Api.Prim as Prim
import JShark.Api.Types
import JShark.Compiler.Emit
  ( JS
  , brackets
  , dquotes
  , hcat
  , jsDecimal
  , jsString
  , parens
  , punctuate
  )
import JShark.Compiler.JsNum (jsBit2, jsRem, jsShl, jsShr, jsUShr)
import Numeric (readInt, showHex)

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
evalFieldLit rec (FieldLitExtraEffect @k (Lift e)) =
  FieldLitExtra @k . Literal <$> rec e
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

packUint8 :: [Word8] -> ByteArray
packUint8 xs = runST go
 where
  !(I# n#) = length xs
  go :: ST s ByteArray
  go = ST $ \s0 ->
    case newByteArray# n# s0 of
      (# s1, mba #) ->
        case write 0# xs mba s1 of
          s2 -> case unsafeFreezeByteArray# mba s2 of
            (# s3, ba #) -> (# s3, ByteArray ba #)
  write _ [] _ s = s
  write i# (W8# w : rest) mba s =
    write (i# +# 1#) rest mba (writeWord8Array# mba i# w s)

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
  Lambda _ g -> pure (ValueFunction (apply g))
  Let _ x g -> rec x >>= rec . g
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
  (FixCall2, ArgsT f x y) -> do
    -- Same semantics as nested 'Apply' (curried 'ValueFunction').
    fv <- rec f
    xv <- rec x
    yv <- rec y
    pure (unFunction (unFunction fv xv) yv)
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
mapFixedArgs ge a = case a of
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
foldFixed _ se _ a = case a of
  ArgsU x -> se x
  ArgsB x y -> se x <> se y
  ArgsT x y z -> se x <> se y <> se z

lookupFrozenField ::
  forall k r f. KnownSymbol k => [FieldLit f r] -> Maybe (Expr f (Field r k))
lookupFrozenField = findLit . reverse
 where
  findLit [] = Nothing
  findLit (FieldLit @k' e : rest) =
    case sameSymbol (Proxy @k) (Proxy @k') of
      Just Refl -> Just e
      Nothing -> findLit rest
  findLit (_ : rest) = findLit rest

withFrozenField ::
  forall k r a.
  KnownSymbol k =>
  Value ('Object r)
  -> (Expr Value (Field r k) -> a)
  -> a
withFrozenField (ValueFrozen fs) k =
  case lookupFrozenField @k fs of
    Just e -> k e
    Nothing -> cannotEval "GetField of a frozen object with effectful fields"

-- | 'evaluate' in 'IO'. Same semantics as 'evaluate'.
evaluateCached :: ClosedExpr u -> IO (Value u)
evaluateCached e = pure (evaluate e)
