{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
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

-- | Pure reference interpreter for closed 'Expr' terms, plus the JS value
-- semantics the optimizer folds with.
module JShark.Compiler.Evaluate
  ( evaluate
  , tryEvaluate
  , EvalFailure (..)
  , evaluateNumber
  , evaluateBigInt
  , isCheapValue
  , jsShow
  , typeOfValue
  , keepLastByKey
  , tryEvalBigBin
  , parseBigIntString
  , uint8Elems
  , packUint8
  , isFiniteDouble
  , math1Fn
  )
where

import Control.Exception (Exception, throw, try)
import qualified Control.Exception as E (evaluate)
import Data.Array.Byte (ByteArray (..))
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Char (digitToInt, isSpace)
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
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
import JShark.Api.Types
import Numeric (readInt)

-- | Why the host interpreter could not produce a value.
data EvalFailure
  = -- | The term uses something the host denotation does not model
    -- (functions, effectful object fields, an op with no host rule).
    EvalUnsupported !Text
  | -- | A JS-like runtime failure: @throw@, an out-of-bounds checked
    -- index, @BigInt(1.5)@, a negative shift, invalid @BigInt@ text.
    EvalJsFailure !Text
  deriving (Show, Eq)

instance Exception EvalFailure

cannotEval :: String -> a
cannotEval = throw . EvalUnsupported . T.pack

jsFailure :: String -> a
jsFailure = throw . EvalJsFailure . T.pack

evaluateNumber :: ClosedExpr 'Number -> Double
evaluateNumber e = case evaluate e of ValueNumber d -> d

evaluateBigInt :: ClosedExpr 'BigInt -> Integer
evaluateBigInt e = case evaluate e of ValueBigInt n -> n

-- | Pure reference interpreter. Shared Haskell heap nodes are walked once
-- per occurrence (no memo table). Throws 'EvalFailure' on failure.
evaluate :: ClosedExpr u -> Value u
evaluate = eval

-- | 'evaluate' with failures reified as data. Forcing to WHNF catches the
-- failure; a value whose lazy interior needs a partial operation is only
-- forced as far as the caller demands.
tryEvaluate :: ClosedExpr u -> IO (Either EvalFailure (Value u))
tryEvaluate e = try (E.evaluate (evaluate e))

num :: Expr Value 'Number -> Double
num e = case eval e of ValueNumber d -> d

big :: Expr Value 'BigInt -> Integer
big e = case eval e of ValueBigInt n -> n

str :: Expr Value 'String -> Text
str e = case eval e of ValueString s -> s

arr :: Expr Value ('Array u) -> [Value u]
arr e = case eval e of ValueArray vs -> vs

bytes :: Expr Value u -> ByteArray
bytes e = case eval e of
  ValueUint8Array ba -> ba
  ValueUint8ClampedArray ba -> ba
  _ -> error "evaluate: expected a byte buffer"

call :: Value ('Function u v) -> Value u -> Value v
call (ValueFunction f) = f

eval :: Expr Value v -> Value v
eval = \case
  Literal v -> v
  Var x -> x
  Apply g x -> call (eval g) (eval x)
  Lambda _ g -> ValueFunction (eval . g)
  Let _ x g -> eval (g (eval x))
  LetRec r b -> let v = eval (r v) in eval (b v)
  If c t e -> case eval c of ValueBool b -> eval (if b then t else e)
  OptionCase o n s -> case eval o of
    ValueOption Nothing -> eval n
    ValueOption (Just x) -> eval (s x)
  ResultOk x -> ValueResult (Right (eval x))
  ResultErr x -> ValueResult (Left (eval x))
  ResultCase r e k -> case eval r of
    ValueResult (Left x) -> eval (e x)
    ValueResult (Right x) -> eval (k x)
  FnLit {} -> cannotEval "Fn (fn)"
  Index xs i ->
    let
      vs = arr xs
     in
      maybe (jsFailure "array index out of bounds") id (at vs (num i))
  U8Index b i ->
    maybe
      (jsFailure "uint8 index out of bounds")
      (ValueNumber . fromIntegral)
      (at (uint8Elems (bytes b)) (num i))
  Error m -> jsFailure (T.unpack (str m))
  Std (Fixed op args) -> evalFixed op args
  Std (Method m) -> evalMethod m
  Std (Kernel k) -> evalKernel k
  -- The runtime conversion is @v == null ? none : some v@; only 'ValueUnit'
  -- is absent on the host, so nested options round-trip.
  UnsafeNullable x -> case eval x of
    ValueUnit -> ValueOption Nothing
    v -> ValueOption (Just v)
  FrozenLit fs -> ValueFrozen (map evalField fs)
  GetField @k o -> case eval o of
    ValueFrozen fs ->
      maybe
        (cannotEval "GetField of a frozen object with effectful fields")
        eval
        (lookupField @k fs)
 where
  at vs d
    | isFiniteDouble d, i >= 0, i < length vs = Just (vs !! i)
    | otherwise = Nothing
   where
    i = truncate d :: Int

evalField :: FieldLit Value r -> FieldLit Value r
evalField = \case
  FieldLit @k e -> FieldLit @k (Literal (eval e))
  FieldLitEffect @k (Lift e) -> FieldLit @k (Literal (eval e))
  FieldLitExtra @k e -> FieldLitExtra @k (Literal (eval e))
  FieldLitExtraEffect @k (Lift e) -> FieldLitExtra @k (Literal (eval e))
  FieldLitEffect _ -> cannotEval "effectful object field (FieldLitEffect); not a pure Lift"
  FieldLitExtraEffect _ -> cannotEval "effectful object field (FieldLitExtraEffect); not a pure Lift"

lookupField ::
  forall k r f. KnownSymbol k => [FieldLit f r] -> Maybe (Expr f (Field r k))
lookupField = go . reverse
 where
  go [] = Nothing
  go (FieldLit @k' e : rest) = case sameSymbol (Proxy @k) (Proxy @k') of
    Just Refl -> Just e
    Nothing -> go rest
  go (_ : rest) = go rest

evalKernel :: Kernel Value u -> Value u
evalKernel = \case
  KNum op x y -> case (eval x, eval y) of
    (ValueNumber a, ValueNumber b) -> ValueNumber (numOpFn op a b)
    (ValueBigInt a, ValueBigInt b) -> ValueBigInt (evalBigBin op a b)
    _ -> cannotEval "arithmetic operand"
  KNegate x -> case eval x of
    ValueNumber a -> ValueNumber (negate a)
    ValueBigInt a -> ValueBigInt (negate a)
    _ -> cannotEval "negation operand"
  KConcat x y -> ValueString (str x <> str y)
  KShow x -> ValueString (jsShow (eval x))
  KTypeOf x -> ValueString (typeOfValue (eval x))
  KAnd x y -> case eval x of ValueBool True -> eval y; _ -> ValueBool False
  KOr x y -> case eval x of ValueBool True -> ValueBool True; _ -> eval y
  KEq _ x y -> ValueBool (valueEq (eval x) (eval y))
  KNEq _ x y -> ValueBool (not (valueEq (eval x) (eval y)))
  KCmp c x y -> ValueBool (cmpOpFn c (valueCompare (eval x) (eval y)))

evalMethod :: Method Value u -> Value u
evalMethod = \case
  MethMap xs f -> ValueArray (map (eval . f) (arr xs))
  MethFilter xs f -> ValueArray [v | v <- arr xs, ValueBool True <- [eval (f v)]]
  MethReduce xs z f -> foldl (\acc v -> eval (f acc v)) (eval z) (arr xs)
  MethReduceRight xs z f -> foldr (\v acc -> eval (f acc v)) (eval z) (arr xs)
  MethToSorted xs f -> ValueArray (mergeSort (\a b -> compare (num (f a b)) 0) (arr xs))
  MethFrom n f ->
    let
      d = num n
      len = if isFiniteDouble d && d > 0 then truncate d else 0 :: Int
     in
      ValueArray [eval (f (ValueNumber (fromIntegral i))) | i <- [0 .. len - 1]]

-- | Stable merge sort; a comparator reporting 'GT' takes the right element.
mergeSort :: (a -> a -> Ordering) -> [a] -> [a]
mergeSort cmp = go
 where
  go [] = []
  go [x] = [x]
  go xs = let (l, r) = splitAt (length xs `div` 2) xs in merge (go l) (go r)
  merge [] ys = ys
  merge xs [] = xs
  merge (x : xs) (y : ys) = case cmp x y of
    GT -> y : merge (x : xs) ys
    _ -> x : merge xs (y : ys)

evalFixed :: FixedOp a b c u -> FixedArgs Value a b c -> Value u
evalFixed op args = case (op, args) of
  (FixSome, ArgsU x) -> ValueOption (Just (eval x))
  (FixMath1 m, ArgsU x) -> ValueNumber (math1Fn m (num x))
  (FixMath2 m, ArgsB x y) -> ValueNumber (math2Fn m (num x) (num y))
  (FixArrLen, ArgsU xs) -> ValueNumber (fromIntegral (length (arr xs)))
  (FixU8Len, ArgsU b) -> ValueNumber (fromIntegral (length (uint8Elems (bytes b))))
  (FixParseInt, ArgsB s r) -> ValueNumber (jsParseInt (str s) (truncate (num r)))
  (FixToBigInt, ArgsU x) ->
    let
      d = num x
      n = truncate d
     in
      if isFiniteDouble d && d == fromInteger n
        then ValueBigInt n
        else
          jsFailure "Number cannot be converted to BigInt because it is not an integer"
  (FixFromBigInt, ArgsU x) -> ValueNumber (fromInteger (big x))
  (FixParseBigInt, ArgsU x) ->
    ValueBigInt
      ( maybe
          (jsFailure "invalid BigInt string")
          id
          (parseBigIntString (T.unpack (str x)))
      )
  (FixConcat, ArgsB x y) -> ValueArray (arr x ++ arr y)
  (FixCall2, ArgsT f x y) -> call (call (eval f) (eval x)) (eval y)
  (FixIncludes, ArgsB xs y) -> let v = eval y in ValueBool (any (valueEq v) (arr xs))
  (FixJoin, ArgsB xs sep) -> ValueString (T.intercalate (str sep) (map joinElem (arr xs)))
  (FixArrSlice, ArgsT xs a b) -> ValueArray (slice (arr xs) (num a) (num b))
  (FixGroupBy, ArgsB xs keyFn) ->
    let
      f = eval keyFn
      step (order, gs) x =
        let
          k = case call f x of ValueString s -> s
         in
          if Map.member k gs
            then (order, Map.adjust (++ [x]) k gs)
            else (order ++ [k], Map.insert k [x] gs)
      (keys, groups) = foldl' step ([], Map.empty) (arr xs)
     in
      ValueArray
        [ ValueFrozen
            [ FieldLit @"key" (Literal (ValueString k))
            , FieldLit @"items" (Literal (ValueArray (groups Map.! k)))
            ]
        | k <- keys
        ]
  -- String and regex ops are codegen-only.
  _ -> cannotEval "a fixed stdlib op"

math1Fn :: Math1 -> Double -> Double
math1Fn = \case
  Abs -> abs
  Sign -> signum
  Sin -> sin
  Cos -> cos
  Tan -> tan
  Asin -> asin
  Acos -> acos
  Atan -> atan
  Sinh -> sinh
  Cosh -> cosh
  Tanh -> tanh
  Asinh -> asinh
  Acosh -> acosh
  Atanh -> atanh
  Sqrt -> sqrt
  Cbrt -> \x -> signum x * (abs x ** (1 / 3))
  Exp -> exp
  Log -> log
  Log2 -> logBase 2
  Log10 -> logBase 10
  Floor -> integral floor
  Ceil -> integral ceiling
  -- JS rounds halves toward +Infinity (@Math.round(-2.5) === -2@), unlike
  -- Haskell's banker's 'round'.
  Round -> integral (floor . (+ 0.5))
  Trunc -> integral truncate
 where
  integral :: (Double -> Integer) -> Double -> Double
  integral f d = if isFiniteDouble d then fromIntegral (f d) else d

math2Fn :: Math2 -> Double -> Double -> Double
math2Fn = \case
  Pow -> (**)
  Atan2 -> atan2
  Max -> max
  Min -> min
  Hypot -> \x y -> sqrt (x * x + y * y)

isFiniteDouble :: Double -> Bool
isFiniteDouble d = not (isNaN d) && not (isInfinite d)

-- Values ----------------------------------------------------------------------

valueEq :: Value u -> Value u -> Bool
valueEq a b = case (a, b) of
  (ValueNumber x, ValueNumber y) -> x == y
  (ValueBigInt x, ValueBigInt y) -> x == y
  (ValueString x, ValueString y) -> x == y
  (ValueBool x, ValueBool y) -> x == y
  (ValueUnit, ValueUnit) -> True
  (ValueArray xs, ValueArray ys) -> length xs == length ys && and (zipWith valueEq xs ys)
  (ValueOption (Just x), ValueOption (Just y)) -> valueEq x y
  (ValueOption x, ValueOption y) -> null x && null y
  (ValueResult (Left x), ValueResult (Left y)) -> valueEq x y
  (ValueResult (Right x), ValueResult (Right y)) -> valueEq x y
  (ValueResult _, ValueResult _) -> False
  (ValueRegex x, ValueRegex y) -> x == y
  (ValueUint8Array x, ValueUint8Array y) -> x == y
  (ValueUint8ClampedArray x, ValueUint8ClampedArray y) -> x == y
  -- A frozen object is a record, not a handle: compare by value, last
  -- field of each name winning.
  (ValueFrozen xs, ValueFrozen ys) ->
    let
      xs' = keepLastByKey fieldKey xs
      ys' = keepLastByKey fieldKey ys
     in
      length xs' == length ys' && all (\x -> any (fieldEq x) ys') xs'
  (ValueFunction _, ValueFunction _) -> cannotEval "function equality"

fieldEq :: forall r. FieldLit Value r -> FieldLit Value r -> Bool
fieldEq x y = case (x, y) of
  (FieldLit @k a, FieldLit @k' b) -> declared @k @k' a b
  (FieldLitEffect @k (Lift a), FieldLitEffect @k' (Lift b)) -> declared @k @k' a b
  (FieldLitExtra @k a, FieldLitExtra @k' b) -> extra @k @k' a b
  (FieldLitExtraEffect @k (Lift a), FieldLitExtraEffect @k' (Lift b)) -> extra @k @k' a b
  _ -> False
 where
  declared ::
    forall k k'.
    (KnownSymbol k, KnownSymbol k') =>
    Expr Value (Field r k) -> Expr Value (Field r k') -> Bool
  declared a b = case sameSymbol (Proxy @k) (Proxy @k') of
    Just Refl -> forced a b
    Nothing -> False
  extra ::
    forall k k' u v.
    (KnownSymbol k, KnownSymbol k', Typeable u, Typeable v) =>
    Expr Value u -> Expr Value v -> Bool
  extra a b = case (sameSymbol (Proxy @k) (Proxy @k'), eqT @u @v) of
    (Just Refl, Just Refl) -> forced a b
    _ -> False
  forced :: Expr Value w -> Expr Value w -> Bool
  forced (Literal a) (Literal b) = valueEq a b
  forced _ _ = error "evaluate: frozen field was not forced"

-- | Only numbers, bigints, strings, and booleans support ordering comparisons.
valueCompare :: Value u -> Value u -> Ordering
valueCompare a b = case (a, b) of
  (ValueNumber x, ValueNumber y) -> compare x y
  (ValueBigInt x, ValueBigInt y) -> compare x y
  (ValueString x, ValueString y) -> compare x y
  (ValueBool x, ValueBool y) -> compare x y
  _ ->
    error
      "evaluate: only numbers, bigints, strings, and booleans support ordering comparisons"

-- | Duplicable without cost: scalars, and results carrying one.
isCheapValue :: Value u -> Bool
isCheapValue = \case
  ValueNumber {} -> True
  ValueBigInt {} -> True
  ValueString {} -> True
  ValueBool {} -> True
  ValueUnit -> True
  ValueResult r -> either isCheapValue isCheapValue r
  _ -> False

-- | Keep the last occurrence of each key, in first-occurrence order.
keepLastByKey :: Eq k => (a -> k) -> [a] -> [a]
keepLastByKey key = reverse . keep [] . reverse
 where
  keep acc [] = acc
  keep acc (x : xs)
    | key x `elem` map key acc = keep acc xs
    | otherwise = keep (x : acc) xs

-- | JS @String(x)@.
jsShow :: Value u -> Text
jsShow = \case
  ValueNumber d
    | not (isNaN d) && not (isInfinite d) && d == fromInteger (truncate d) ->
        T.pack (show (truncate d :: Integer))
    | otherwise -> T.pack (show d)
  ValueBigInt n -> T.pack (show n)
  ValueString s -> s
  ValueBool b -> if b then "true" else "false"
  ValueUnit -> "undefined"
  -- @Array.prototype.join@ prints @null@ and @undefined@ as empty strings.
  ValueArray xs -> T.intercalate "," (map joinElem xs)
  ValueRegex s -> s
  ValueUint8Array ba -> showBytes ba
  ValueUint8ClampedArray ba -> showBytes ba
  ValueFunction _ -> cannotEval "show of a function"
  _ -> "[object Object]"
 where
  showBytes = T.intercalate "," . map (T.pack . show) . uint8Elems

joinElem :: Value u -> Text
joinElem = \case
  ValueOption _ -> "[object Object]"
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
  _ -> "object"

-- | JS @Array.prototype.slice@: truncate, count negatives from the end, clamp.
slice :: [a] -> Double -> Double -> [a]
slice vs start end = take (max 0 (clamp end - k)) (drop k vs)
 where
  len = length vs
  k = clamp start
  clamp x
    | isNaN x = 0
    | isInfinite x = if x < 0 then 0 else len
    | otherwise = let n = truncate x in if n < 0 then max 0 (len + n) else min n len

jsParseInt :: Text -> Int -> Double
jsParseInt s r
  | r < 2 || r > 36 = 0 / 0
  | otherwise =
      let
        (neg, t) = sign (dropWhile isSpace (T.unpack s))
       in
        case readInt (fromIntegral r :: Integer) (digitBelow r) digitToInt t of
          (n, _) : _ -> fromInteger (if neg then negate n else n)
          [] -> 0 / 0

sign :: String -> (Bool, String)
sign = \case
  '-' : xs -> (True, xs)
  '+' : xs -> (False, xs)
  xs -> (False, xs)

-- | JS @BigInt(s)@: optional sign and @0x@ \/ @0b@ \/ @0o@ prefix.
parseBigIntString :: String -> Maybe Integer
parseBigIntString raw =
  let
    (neg, rest) = sign (dropWhile isSpace (reverse (dropWhile isSpace (reverse raw))))
    (base, digits) = case map Char.toLower (take 2 rest) of
      "0x" -> (16, drop 2 rest)
      "0b" -> (2, drop 2 rest)
      "0o" -> (8, drop 2 rest)
      _ -> (10, rest)
   in
    case readInt (fromIntegral base :: Integer) (digitBelow base) digitToInt digits of
      (n, []) : _ | not (null digits) -> Just (if neg then negate n else n)
      _ -> Nothing

digitBelow :: Int -> Char -> Bool
digitBelow base c
  | Char.isDigit c = Char.ord c - Char.ord '0' < base
  | Char.isAsciiLower c = Char.ord c - Char.ord 'a' + 10 < base
  | Char.isAsciiUpper c = Char.ord c - Char.ord 'A' + 10 < base
  | otherwise = False

evalBigBin :: NumOp -> Integer -> Integer -> Integer
evalBigBin op a b = case op of
  NPlus -> a + b
  NMinus -> a - b
  NTimes -> a * b
  NDiv -> quot a b
  NRem -> rem a b
  NBitAnd -> a .&. b
  NBitOr -> a .|. b
  NBitXor -> xor a b
  NShl -> if b < 0 then negShift else shiftL a (fromInteger b)
  NShr -> if b < 0 then negShift else shiftR a (fromInteger b)
  NUShr -> jsFailure "BigInts have no unsigned right shift"
 where
  negShift = jsFailure "BigInt shift count is negative"

-- | 'evalBigBin' where it cannot throw.
tryEvalBigBin :: NumOp -> Integer -> Integer -> Maybe Integer
tryEvalBigBin op a b
  | op `elem` [NDiv, NRem], b == 0 = Nothing
  | op `elem` [NShl, NShr], b < 0 = Nothing
  | NUShr <- op = Nothing
  | otherwise = Just (evalBigBin op a b)

uint8Elems :: ByteArray -> [Word8]
uint8Elems (ByteArray ba#) =
  [W8# (indexWord8Array# ba# i#) | I# i# <- [0 .. I# (sizeofByteArray# ba#) - 1]]

packUint8 :: [Word8] -> ByteArray
packUint8 xs = runST go
 where
  !(I# n#) = length xs
  go :: ST s ByteArray
  go = ST $ \s0 -> case newByteArray# n# s0 of
    (# s1, mba #) -> case write 0# xs mba s1 of
      s2 -> case unsafeFreezeByteArray# mba s2 of
        (# s3, ba #) -> (# s3, ByteArray ba #)
  write _ [] _ s = s
  write i# (W8# w : rest) mba s = write (i# +# 1#) rest mba (writeWord8Array# mba i# w s)
