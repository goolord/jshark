{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
      , ExprIndex
      , MathUnary
      , MathBinary
      , ExprUnary
      , ExprBinary
      , ExprTernary
      , ExprMap
      , ExprFilter
      , ExprReduce
      , ExprReduceRight
      , ExprGroupBy
      , ExprZipWith
      , UnsafeNullable
      , FrozenLit
      , GetField
      )
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
    , OptionCaseE
    , ResultCaseE
    , StringCaseE
    , Throw
    , Try
    , ObjectLit
    , DeleteProp
    , ArrayLit
    , ArraySort
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
  )
where

-- Indexed PHOAS (binders @f u@, closed terms @forall f@) in the style of
-- Chlipala / Kmett's parametric HOAS, with host-language sharing as in
-- https://www.reddit.com/r/haskell/comments/85een6/sharing_from_phoas_multiple_interpreters_from_free/dvxhlba
--
-- 'Expr' is the pure tree; 'Effect' is the impure tree. They join at FFI
-- through 'Arg', not by treating effects as expressions.

import Control.Monad (foldM)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Char (digitToInt, isSpace)
import qualified Data.Char as Char
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int32)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Monoid (All (..), Sum (..))
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable, eqT, type (:~:) (..))
import Data.Word (Word32)
import GHC.TypeLits (KnownSymbol, sameSymbol, symbolVal)
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

fieldLitEq :: FieldLit Value r -> FieldLit Value r -> Bool
fieldLitEq (FieldLit @k a) (FieldLit @k' b) =
  case sameSymbol (Proxy @k) (Proxy @k') of
    Nothing -> False
    Just Refl -> case (a, b) of
      (Literal x, Literal y) -> valueEq x y
      _ -> error "evaluate: frozen field was not forced"

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
  ValueFrozen {} -> "object"

jsShowNumber :: Double -> String
jsShowNumber d
  | isInt = show (truncate d :: Integer)
  | otherwise = show d
 where
  isInt = not (isNaN d) && not (isInfinite d) && d == fromInteger (truncate d)

isFiniteDouble :: Double -> Bool
isFiniteDouble d = not (isNaN d) && not (isInfinite d)

-- | Implements the unary @Math@ functions supported by 'MathUnary'.
mathUnaryFn :: MathFn1 -> Double -> Double
mathUnaryFn = \case
  MathAbs -> abs
  MathSign -> signum
  MathSin -> sin
  MathCos -> cos
  MathTan -> tan
  MathAsin -> asin
  MathAcos -> acos
  MathAtan -> atan
  MathSinh -> sinh
  MathCosh -> cosh
  MathTanh -> tanh
  MathAsinh -> asinh
  MathAcosh -> acosh
  MathAtanh -> atanh
  MathSqrt -> sqrt
  MathCbrt -> \x -> signum x * (abs x ** (1 / 3))
  MathExp -> exp
  MathLog -> log
  MathLog2 -> logBase 2
  MathLog10 -> logBase 10
  MathFloor -> fromIntegral . (floor :: Double -> Integer)
  MathCeil -> fromIntegral . (ceiling :: Double -> Integer)
  -- JS's Math.round rounds half-way values toward +Infinity (e.g.
  -- Math.round(2.5) === 3, Math.round(-2.5) === -2), unlike Haskell's
  -- 'round' (banker's rounding to even: round 2.5 == 2). floor(x + 0.5)
  -- matches JS's semantics.
  MathRound -> fromIntegral . (floor :: Double -> Integer) . (+ 0.5)
  MathTrunc -> fromIntegral . (truncate :: Double -> Integer)

-- | Implements the binary @Math@ functions supported by 'MathBinary'.
mathBinaryFn :: MathFn2 -> Double -> Double -> Double
mathBinaryFn = \case
  MathPow -> (**)
  MathAtan2 -> atan2
  MathMax -> max
  MathMin -> min
  MathHypot -> \x y -> sqrt (x * x + y * y)

-- | Fold @Math.*@ only when the Haskell result is known to match JS.
-- Transcendentals (@sin(1)@, @cbrt@, @pow@, …) stay in JS.
exactMathUnary :: MathFn1 -> Double -> Maybe Double
exactMathUnary n a = case n of
  MathAbs -> Just (abs a)
  MathSign | isFiniteDouble a -> Just (signum a)
  MathSin | a == 0 -> Just 0
  MathCos | a == 0 -> Just 1
  MathTan | a == 0 -> Just 0
  MathSinh | a == 0 -> Just 0
  MathCosh | a == 0 -> Just 1
  MathTanh | a == 0 -> Just 0
  MathAsinh | a == 0 -> Just 0
  MathAcosh | a == 1 -> Just 0
  MathAtanh | a == 0 -> Just 0
  MathSqrt
    | a >= 0
    , let
        r = sqrt a
    , r * r == a ->
        Just r
  MathFloor | isFiniteDouble a -> Just (fromIntegral (floor a :: Integer))
  MathCeil | isFiniteDouble a -> Just (fromIntegral (ceiling a :: Integer))
  MathRound | isFiniteDouble a -> Just (fromIntegral (floor (a + 0.5) :: Integer))
  MathTrunc | isFiniteDouble a -> Just (fromIntegral (truncate a :: Integer))
  _ -> Nothing

exactMathBinary :: MathFn2 -> Double -> Double -> Maybe Double
exactMathBinary n a b = case n of
  MathMax | isFiniteDouble a && isFiniteDouble b -> Just (max a b)
  MathMin | isFiniteDouble a && isFiniteDouble b -> Just (min a b)
  _ -> Nothing

cannotEval :: String -> a
cannotEval what = error ("evaluate: cannot evaluate " ++ what)

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

-- | Optimizer / codegen name. 'Stamp' is an untyped tag for use-counting.
-- 'Embed' is a typed hole filler: applying a PHOAS continuation to
-- 'Embed' @x@ inlines @x@ at the binder's own universe, so substitution
-- never needs 'unsafeCoerce' or 'eqT'.
data Stamp (u :: Universe) where
  Stamp :: Int -> Stamp u
  Embed :: Expr Stamp u -> Stamp u

-- | Codegen / dummy binder. Same as 'Stamp'; kept so call sites that
-- only need a name stay readable.
pattern Name :: Int -> Stamp u
pattern Name i = Stamp i

{-# COMPLETE Stamp, Embed #-}

stampId :: Stamp u -> Int
stampId (Stamp i) = i
stampId (Embed _) = error "JShark.stampId: Embed (flatten first)"

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
  Plus x y -> num2 (+) x y
  Times x y -> num2 (*) x y
  Minus x y -> num2 (-) x y
  Negate x -> num1 negate x
  FracDiv x y -> num2 (/) x y
  Rem x y -> num2 jsRem x y
  BitAnd x y -> num2 (jsBit2 (.&.)) x y
  BitOr x y -> num2 (jsBit2 (.|.)) x y
  BitXor x y -> num2 (jsBit2 xor) x y
  Shl x y -> num2 jsShl x y
  Shr x y -> num2 jsShr x y
  UShr x y -> num2 jsUShr x y
  Var x -> pure x
  Apply g x -> unFunction <$> rec g <*> rec x
  Lambda g -> pure (ValueFunction (apply g))
  Concat x y -> do
    a <- rec x
    b <- rec y
    pure (ValueString (unString a <> unString b))
  Show x -> ValueString . jsShow <$> rec x
  TypeOf x -> ValueString . typeOfValue <$> rec x
  And x y -> do
    a <- rec x
    if unBool a then rec y else pure (ValueBool False)
  Or x y -> do
    a <- rec x
    if unBool a then pure (ValueBool True) else rec y
  Eq x y -> ValueBool <$> (valueEq <$> rec x <*> rec y)
  NEq x y -> ValueBool . not <$> (valueEq <$> rec x <*> rec y)
  GTh x y -> ValueBool . (== GT) <$> (valueCompare <$> rec x <*> rec y)
  LTh x y -> ValueBool . (== LT) <$> (valueCompare <$> rec x <*> rec y)
  GTEq x y -> ValueBool . (/= LT) <$> (valueCompare <$> rec x <*> rec y)
  LTEq x y -> ValueBool . (/= GT) <$> (valueCompare <$> rec x <*> rec y)
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
  UnsafeEffectExpr _ -> cannotEval "an embedded Effect (UnsafeEffectExpr)"
  ExprUnary n x -> case n of
    StdArrLen -> do
      arr <- rec x
      case arr of
        ValueArray vs ->
          pure (ValueNumber (fromIntegral (Prelude.length vs)))
    _ -> cannotEval "a stdlib ExprUnary"
  ExprBinary n x y -> evalStdBinary n <$> rec x <*> rec y
  ExprTernary n xs a b -> case n of
    StdArrSlice -> do
      arr <- rec xs
      i <- rec a
      j <- rec b
      case arr of
        ValueArray vs ->
          pure (ValueArray (jsArraySlice vs (unNumber i) (unNumber j)))
    _ -> cannotEval "a stdlib ExprTernary"
  ExprMap xs f -> do
    arr <- rec xs
    case arr of
      ValueArray vs -> ValueArray <$> traverse (\v -> rec (f v)) vs
  ExprFilter xs f -> do
    arr <- rec xs
    case arr of
      ValueArray vs -> do
        keep <-
          traverse
            ( \v -> do
                b <- rec (f v)
                pure (unBool b, v)
            )
            vs
        pure (ValueArray [v | (True, v) <- keep])
  ExprGroupBy xs kf -> do
    arr <- rec xs
    case arr of
      ValueArray vs -> do
        keyed <-
          traverse
            ( \v -> do
                k <- rec (kf v)
                pure (unString k, v)
            )
            vs
        pure (ValueArray (map groupRow (groupByFirst keyed)))
  ExprZipWith xs ys f -> do
    a <- rec xs
    b <- rec ys
    case (a, b) of
      (ValueArray as, ValueArray bs) ->
        ValueArray <$> sequence (zipWith (\x y -> rec (f x y)) as bs)
  ExprReduce xs z f -> do
    arr <- rec xs
    z0 <- rec z
    case arr of
      ValueArray vs -> foldM (\acc v -> rec (f acc v)) z0 vs
  ExprReduceRight xs z f -> do
    arr <- rec xs
    z0 <- rec z
    case arr of
      ValueArray vs ->
        let
          stepRight [] = pure z0
          stepRight (v : rest) = stepRight rest >>= \acc -> rec (f acc v)
         in
          stepRight vs
  ExprIndex xs i -> do
    arr <- rec xs
    iv <- rec i
    case arr of
      ValueArray vs ->
        -- Ordinary JS @a[1.9]@ is the string key @\"1.9\"@ (@undefined@).
        -- We treat the index as an integer (trunc toward 0) and throw
        -- out of bounds: no holes, no @undefined@ at an arbitrary @u@.
        -- Codegen matches this; it does not emit raw @a[i]@.
        let
          d = unNumber iv
          idx = truncate d :: Int
         in
          if isFiniteDouble d && idx >= 0 && idx < length vs
            then pure (vs !! idx)
            else error "evaluate: array index out of bounds"
  MathUnary name x -> ValueNumber . mathUnaryFn name . unNumber <$> rec x
  MathBinary name x y ->
    ValueNumber
      <$> (mathBinaryFn name <$> (unNumber <$> rec x) <*> (unNumber <$> rec y))
  UnsafeNullable x -> ValueOption . Just <$> rec x
  FrozenLit fs -> ValueFrozen <$> traverse (evalFieldLit rec) fs
  GetField @k o -> do
    ov <- rec o
    withFrozenField @k ov rec
 where
  num1 f x = ValueNumber . f . unNumber <$> rec x
  num2 f x y = ValueNumber <$> (f <$> (unNumber <$> rec x) <*> (unNumber <$> rec y))

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

-- | Memoize constructors whose result universe is a concrete 'Typeable'
-- type. Polymorphic-result nodes cannot call 'go' (no 'Typeable'
-- evidence); they fall through to 'evalValue'.
goOpen :: EvalCache -> Expr Value v -> IO (Value v)
goOpen cache e = case e of
  Plus {} -> go cache e
  Times {} -> go cache e
  Minus {} -> go cache e
  Negate {} -> go cache e
  FracDiv {} -> go cache e
  Rem {} -> go cache e
  BitAnd {} -> go cache e
  BitOr {} -> go cache e
  BitXor {} -> go cache e
  Shl {} -> go cache e
  Shr {} -> go cache e
  UShr {} -> go cache e
  Concat {} -> go cache e
  Show {} -> go cache e
  TypeOf {} -> go cache e
  And {} -> go cache e
  Or {} -> go cache e
  Eq {} -> go cache e
  NEq {} -> go cache e
  GTh {} -> go cache e
  LTh {} -> go cache e
  GTEq {} -> go cache e
  LTEq {} -> go cache e
  MathUnary {} -> go cache e
  MathBinary {} -> go cache e
  ExprBinary StdParseInt _ _ -> go cache e
  Literal ValueNumber {} -> go cache e
  Literal ValueString {} -> go cache e
  Literal ValueBool {} -> go cache e
  Literal ValueUnit -> go cache e
  Literal ValueRegex {} -> go cache e
  FrozenLit fs -> ValueFrozen <$> traverse (evalFieldLit (goOpen cache)) fs
  GetField @k o -> do
    ov <- goOpen cache o
    withFrozenField @k ov (goOpen cache)
  _ -> pure (evalValue e)

goNode :: EvalCache -> Expr Value v -> IO (Value v)
goNode cache = evalAlg (goOpen cache) (applyCached cache)

evalStdBinary :: StdBinary a b c -> Value a -> Value b -> Value c
evalStdBinary n x y = case n of
  StdParseInt ->
    ValueNumber (jsParseInt (unString x) (truncate (unNumber y)))
  StdConcat ->
    case (x, y) of
      (ValueArray as, ValueArray bs) -> ValueArray (as ++ bs)
  StdIncludes ->
    case x of
      ValueArray vs -> ValueBool (any (valueEq y) vs)
  StdJoin ->
    case x of
      ValueArray vs ->
        ValueString (T.intercalate (unString y) (map jsJoinElem vs))
  StdTest -> cannotEval "RegExp.test"
  StdIndexOf -> cannotEval "String.indexOf"
  StdSplit -> cannotEval "String.split"

printComputation :: Doc -> IO ()
printComputation computation = putStrLn (renderJSCompact computation)

renderJS :: Doc -> String
renderJS = P.renderStyle P.style

-- | Linear dump. HughesPJ 'PageMode' on a large inlined @bindRec@
-- body is superlinear (breakout sat in 'renderJS' for tens of seconds).
-- 'compileEffect' uses this, then 'prettyJS' for 'Readable'.
renderJSCompact :: Doc -> String
renderJSCompact = P.renderStyle P.style {P.mode = P.LeftMode}

-- | Integer slot + throw on a hole. Raw @a[i]@ would use the string key
-- (@a[1.9]@ is @undefined@) and invent @undefined@ at an arbitrary @u@.
jsCheckedIndex :: Doc -> Doc -> Doc
jsCheckedIndex arr idx =
  P.parens
    "function(a,i){var n=Math.trunc(i);if(!(n>=0&&n<a.length))throw new Error(\"jshark: index\");return a[n];}"
    <> P.parens (arr <> ("," <+> idx))

-- | A runtime function emitted once per program, ahead of the code that
-- calls it. Recording the use in 'CG' keeps a second call site from
-- repeating the whole definition.
data Helper = HelperEq | HelperGroupBy | HelperZipWith
  deriving (Eq, Ord)

helperSource :: Helper -> Doc
helperSource = \case
  -- @===@ then structural arrays / plain objects (frozen records, 'Result').
  -- Identity-only @===@ would make two equal @[1]@ / @{x:1}@ bindings
  -- disagree with 'evaluate'.
  HelperEq ->
    "function $eq(a,b){if(a===b)return true;if(Array.isArray(a)&&Array.isArray(b)){if(a.length!==b.length)return false;for(var i=0;i<a.length;i++)if(!$eq(a[i],b[i]))return false;return true}if(a&&b&&a.constructor===Object&&b.constructor===Object){var ka=Object.keys(a);if(ka.length!==Object.keys(b).length)return false;for(var j=0;j<ka.length;j++){var k=ka[j];if(!Object.prototype.hasOwnProperty.call(b,k)||!$eq(a[k],b[k]))return false}return true}return false}"
  -- First-seen keys, arrays of items. Not a null-prototype @Object.groupBy@.
  HelperGroupBy ->
    "function $groupBy(a,f){var g=Object.create(null),ks=[],i,k;for(i=0;i<a.length;i++){k=f(a[i]);if(!Object.prototype.hasOwnProperty.call(g,k)){ks.push(k);g[k]=[]}g[k].push(a[i])}return ks.map(function(k){return {key:k,items:g[k]}})}"
  HelperZipWith ->
    "function $zipWith(a,b,f){var n=Math.min(a.length,b.length),o=[],i;for(i=0;i<n;i++)o.push(f(a[i],b[i]));return o}"

helperDecls :: CG -> Doc
helperDecls = P.vcat . map helperSource . Set.toAscList . cgHelpers

jsValueEq :: Doc -> Doc -> Doc
jsValueEq a b = "$eq" <> P.parens (a <> ("," <+> b))

jsValueNEq :: Doc -> Doc -> Doc
jsValueNEq a b = "!" <> P.parens (jsValueEq a b)

jsGroupBy :: Doc -> Doc -> Doc
jsGroupBy arr kf = "$groupBy" <> P.parens (arr <> ("," <+> kf))

jsZipWith :: Doc -> Doc -> Doc -> Doc
jsZipWith xs ys f =
  "$zipWith" <> P.parens (xs <> ("," <+> (ys <> ("," <+> f))))

groupByFirst :: [(Text, Value u)] -> [(Text, [Value u])]
groupByFirst kvs =
  [(k, reverse (M.findWithDefault [] k grouped)) | k <- reverse revOrder]
 where
  (grouped, revOrder) = foldl' step (M.empty, []) kvs
  step (acc, ks) (k, v)
    | M.member k acc = (M.adjust (v :) k acc, ks)
    | otherwise = (M.insert k [v] acc, k : ks)

groupRow :: (Text, [Value u]) -> Value ('Object (GroupBy u))
groupRow (k, vs) =
  ValueFrozen
    [ FieldLit @"key" (Literal (ValueString k))
    , FieldLit @"items" (Literal (ValueArray vs))
    ]

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
data CG = CG
  { cgIdent :: {-# UNPACK #-} !Int
  , cgTag :: {-# UNPACK #-} !Int
  , cgHelpers :: !(Set Helper)
  }

startCG :: CG
startCG = CG 0 (-2) Set.empty

allocTag :: CG -> (Int, CG)
allocTag s = (cgTag s, s {cgTag = cgTag s - 1})

allocIdent :: CG -> (Int, CG)
allocIdent s = (cgIdent s, s {cgIdent = cgIdent s + 1})

useHelper :: Helper -> CG -> CG
useHelper h s = s {cgHelpers = Set.insert h (cgHelpers s)}

nestedDummyId :: Int
nestedDummyId = minBound

nestedDummy :: Stamp u
nestedDummy = Name nestedDummyId

constBind :: Int -> Doc -> Doc
constBind n ref = ("const" <+> P.text ('n' : show n) <+> "=" <+> ref) <> P.semi

-- | Ident already allocated for this effect (@Lift (Var n1)@). Not a
-- counter guess: only a binder that is already in the tree.
liveBinder :: Effect Stamp u -> Maybe Int
liveBinder (Lift e) = liveBinderExpr e
liveBinder _ = Nothing

liveBinderExpr :: Expr Stamp u -> Maybe Int
liveBinderExpr (Var (Stamp n)) | n >= 0 = Just n
liveBinderExpr (UnsafeNullable e) = liveBinderExpr e
liveBinderExpr (UnsafeEffectExpr e) = liveBinder e
liveBinderExpr _ = Nothing

-- | @const n = x; k n@ aliases. Dropping them leaks the body stamp.
isAliasBind :: Effect Stamp u -> Bool
isAliasBind (Lift (Var _)) = True
isAliasBind (Lift (UnsafeNullable (Var _))) = True
isAliasBind (Lift (UnsafeEffectExpr e)) = isAliasBind e
isAliasBind _ = False

jsCall :: Doc -> Doc -> Doc
jsCall f a = P.parens f <> P.parens a

-- | Needs no parentheses as an operand: already a primary JS expression.
isSimple :: Expr Stamp u -> Bool
isSimple = \case
  Literal {} -> True
  Var {} -> True
  Show {} -> True
  TypeOf {} -> True
  Negate {} -> True
  ExprUnary {} -> True
  ExprBinary {} -> True
  ExprTernary {} -> True
  ExprMap {} -> True
  ExprFilter {} -> True
  ExprGroupBy {} -> True
  ExprZipWith {} -> True
  ExprIndex {} -> True
  MathUnary {} -> True
  MathBinary {} -> True
  UnsafeNullable x -> isSimple x
  FrozenLit {} -> True
  GetField {} -> True
  -- Single-use Effect spliced into an Expr hole (e.g. inlined 'ffi').
  UnsafeEffectExpr e -> isSimpleEffect e
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
      (Sum . countLazyExpr t)
      (Sum . countEffect t)
      (Sum . countLazyEffect t)

-- Re-opt only small trees. A second walk of a @bindRec@ / do-chain
-- paint body is what hung todo-mvc and breakout.
optSmall :: Int
optSmall = 16

sizeExpr :: Expr Stamp u -> Int
sizeExpr e = case e of
  Var (Embed e') -> sizeExpr e'
  _ -> 1 + getSum (foldExpr nestedDummy s s sf e)
 where
  s = Sum . sizeExpr
  sf = Sum . sizeEffect

sizeEffect :: Effect Stamp u -> Int
sizeEffect e = 1 + getSum (foldEff nestedDummy s s sf sf e)
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

mapFieldLit :: (forall u. Expr f u -> Expr f u) -> FieldLit f r -> FieldLit f r
mapFieldLit g (FieldLit @k e) = FieldLit @k (g e)

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
  Concat x y -> Concat (ge x) (ge y)
  Plus x y -> Plus (ge x) (ge y)
  Times x y -> Times (ge x) (ge y)
  Minus x y -> Minus (ge x) (ge y)
  Negate x -> Negate (ge x)
  FracDiv x y -> FracDiv (ge x) (ge y)
  Rem x y -> Rem (ge x) (ge y)
  BitAnd x y -> BitAnd (ge x) (ge y)
  BitOr x y -> BitOr (ge x) (ge y)
  BitXor x y -> BitXor (ge x) (ge y)
  Shl x y -> Shl (ge x) (ge y)
  Shr x y -> Shr (ge x) (ge y)
  UShr x y -> UShr (ge x) (ge y)
  And x y -> And (ge x) (ge y)
  Or x y -> Or (ge x) (ge y)
  Eq x y -> Eq (ge x) (ge y)
  NEq x y -> NEq (ge x) (ge y)
  GTh x y -> GTh (ge x) (ge y)
  LTh x y -> LTh (ge x) (ge y)
  GTEq x y -> GTEq (ge x) (ge y)
  LTEq x y -> LTEq (ge x) (ge y)
  Let x g -> Let (ge x) (ge . g)
  LetRec rhs body -> LetRec (ge . rhs) (ge . body)
  Lambda g -> Lambda (ge . g)
  Apply f x -> Apply (ge f) (ge x)
  Show x -> Show (ge x)
  TypeOf x -> TypeOf (ge x)
  If c u v -> If (ge c) (ge u) (ge v)
  OptionCase o n s -> OptionCase (ge o) (ge n) (ge . s)
  ResultOk x -> ResultOk (ge x)
  ResultErr x -> ResultErr (ge x)
  ResultCase o e s -> ResultCase (ge o) (ge . e) (ge . s)
  UnsafeEffectExpr e -> UnsafeEffectExpr (gf e)
  ExprUnary n x -> ExprUnary n (ge x)
  ExprBinary n x y -> ExprBinary n (ge x) (ge y)
  ExprTernary n x y z -> ExprTernary n (ge x) (ge y) (ge z)
  ExprMap x f -> ExprMap (ge x) (ge . f)
  ExprFilter x f -> ExprFilter (ge x) (ge . f)
  ExprGroupBy x f -> ExprGroupBy (ge x) (ge . f)
  ExprZipWith x y f -> ExprZipWith (ge x) (ge y) (\a b -> ge (f a b))
  ExprReduce x z f -> ExprReduce (ge x) (ge z) (\a b -> ge (f a b))
  ExprReduceRight x z f -> ExprReduceRight (ge x) (ge z) (\a b -> ge (f a b))
  ExprIndex x i -> ExprIndex (ge x) (ge i)
  MathUnary n x -> MathUnary n (ge x)
  MathBinary n x y -> MathBinary n (ge x) (ge y)
  UnsafeNullable x -> UnsafeNullable (ge x)
  FrozenLit fs -> FrozenLit (map (mapFieldLit ge) fs)
  GetField @k o -> GetField @k (ge o)

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
  OptionCaseE o n s -> OptionCaseE (ge o) (gf n) (gf . s)
  ResultCaseE o e s -> ResultCaseE (ge o) (gf . e) (gf . s)
  StringCaseE o arms d ->
    StringCaseE (ge o) (map (fmap gf) arms) (gf d)
  Throw x -> Throw (ge x)
  Try a k -> Try (gf a) (gf . k)
  ObjectLit fs -> ObjectLit (map (mapFieldLit ge) fs)
  DeleteProp o k -> DeleteProp (gf o) (ge k)
  ArrayLit es -> ArrayLit (map gf es)
  ArraySort xs f -> ArraySort (ge xs) (\a b -> ge (f a b))

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
  Concat x y -> se x <> se y
  Plus x y -> se x <> se y
  Times x y -> se x <> se y
  Minus x y -> se x <> se y
  Negate x -> se x
  FracDiv x y -> se x <> se y
  Rem x y -> se x <> se y
  BitAnd x y -> se x <> se y
  BitOr x y -> se x <> se y
  BitXor x y -> se x <> se y
  Shl x y -> se x <> se y
  Shr x y -> se x <> se y
  UShr x y -> se x <> se y
  And x y -> se x <> le y
  Or x y -> se x <> le y
  Eq x y -> se x <> se y
  NEq x y -> se x <> se y
  GTh x y -> se x <> se y
  LTh x y -> se x <> se y
  GTEq x y -> se x <> se y
  LTEq x y -> se x <> se y
  Let x g -> se x <> se (g dummy)
  LetRec r b -> le (r dummy) <> se (b dummy)
  Lambda g -> le (g dummy)
  Apply f x -> se f <> se x
  Show x -> se x
  TypeOf x -> se x
  If c u v -> se c <> le u <> le v
  OptionCase o n s -> se o <> le n <> le (s dummy)
  ResultOk x -> se x
  ResultErr x -> se x
  ResultCase o e s -> se o <> le (e dummy) <> le (s dummy)
  UnsafeEffectExpr e -> sf e
  ExprUnary _ x -> se x
  ExprBinary _ x y -> se x <> se y
  ExprTernary _ x y z -> se x <> se y <> se z
  ExprMap x f -> se x <> le (f dummy)
  ExprFilter x f -> se x <> le (f dummy)
  ExprGroupBy x f -> se x <> le (f dummy)
  ExprZipWith x y f -> se x <> se y <> le (f dummy dummy)
  ExprReduce x z f -> se x <> se z <> le (f dummy dummy)
  ExprReduceRight x z f -> se x <> se z <> le (f dummy dummy)
  ExprIndex x i -> se x <> se i
  MathUnary _ x -> se x
  MathBinary _ x y -> se x <> se y
  UnsafeNullable x -> se x
  FrozenLit fs -> foldMap (\(FieldLit e) -> se e) fs
  GetField o -> se o

foldEff ::
  forall f m u.
  Monoid m =>
  (forall v. f v)
  -> (forall v. Expr f v -> m)
  -> (forall v. Expr f v -> m)
  -> (forall v. Effect f v -> m)
  -> (forall v. Effect f v -> m)
  -> Effect f u
  -> m
foldEff dummy se le sf lf = \case
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
  OptionCaseE o n s -> se o <> lf n <> lf (s dummy)
  ResultCaseE o e s -> se o <> lf (e dummy) <> lf (s dummy)
  StringCaseE o arms d -> se o <> foldMap (lf . snd) arms <> lf d
  Throw x -> se x
  Try a k -> sf a <> lf (k dummy)
  ObjectLit fs -> foldMap (\(FieldLit e) -> se e) fs
  DeleteProp o k -> sf o <> se k
  ArrayLit es -> foldMap sf es
  ArraySort xs f -> se xs <> le (f dummy dummy)
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

fieldsPure :: PhoasDummy f => [FieldLit f r] -> Bool
fieldsPure = all (\(FieldLit e) -> isPureExpr e)

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
  e -> mapExpr flattenExpr flattenEff e

flattenEff :: Effect Stamp u -> Effect Stamp u
flattenEff = mapEff flattenExpr flattenEff

-- | Replace 'Stamp' @old@ with @new@. Phantom in the universe, so this
-- does not need a cast. Used after the one 'optUnder' apply of @f@.
renameExpr :: Int -> Int -> Expr Stamp u -> Expr Stamp u
renameExpr old new = \case
  Var (Embed e) -> renameExpr old new (flattenExpr e)
  Var (Stamp t) | t == old -> Var (Stamp new)
  Var s -> Var s
  e -> mapExpr (renameExpr old new) (renameEff old new) e

renameEff :: Int -> Int -> Effect Stamp u -> Effect Stamp u
renameEff old new = mapEff (renameExpr old new) (renameEff old new)

inlineExpr :: (Stamp u -> Expr Stamp v) -> Expr Stamp u -> Expr Stamp v
inlineExpr f x = flattenExpr (f (Embed x))

inlineEff :: (Stamp u -> Effect Stamp v) -> Expr Stamp u -> Effect Stamp v
inlineEff f x = flattenEff (f (Embed x))

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

optUnder :: Int -> (Stamp u -> Expr Stamp v) -> (Int, Int, Expr Stamp v)
optUnder t0 f =
  let
    tag = t0
    (t1, body) = optExpr (t0 - 1) (f (Stamp tag))
   in
    (t1, tag, body)

optUnderE :: Int -> (Stamp u -> Effect Stamp v) -> (Int, Int, Effect Stamp v)
optUnderE t0 f =
  let
    tag = t0
    (t1, body) = optEffect (t0 - 1) (f (Stamp tag))
   in
    (t1, tag, body)

optUnder2 ::
  Int -> (Stamp a -> Stamp b -> Expr Stamp v) -> (Int, Int, Int, Expr Stamp v)
optUnder2 t0 f =
  let
    tA = t0
    tB = t0 - 1
    (t1, body) = optExpr (t0 - 2) (f (Stamp tA) (Stamp tB))
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
  ValueArray {} -> False
  ValueFunction {} -> False
  ValueFrozen {} -> False

isCheap :: Expr Stamp u -> Bool
isCheap = \case
  Literal v -> isCheapValue v
  UnsafeNullable x -> isCheap x
  FrozenLit fs -> all (\(FieldLit e) -> isCheap e) fs
  GetField o -> isCheap o
  _ -> False

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
  UnsafeEffectExpr _ -> False
  ExprUnary n x -> isPureStdUnary n && isPureExpr x
  _ -> getAll (foldExpr phoasDummy p p pe e)
 where
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
  Throw {} -> False
  Try {} -> False
  DeleteProp {} -> False
  ArraySort {} -> False
  _ ->
    getAll
      ( foldEff
          phoasDummy
          (All . isPureExpr)
          (All . isPureExpr)
          (All . isPureEffectStamp)
          (All . isPureEffectStamp)
          e
      )

-- | @JSON.stringify@ throws on bigint / circular values, so unused
-- stringify is kept.
isPureStdUnary :: StdUnary a b -> Bool
isPureStdUnary StdStringify = False
isPureStdUnary _ = True

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
foldEq = foldFrozenEq valueEq Eq

foldNEq :: Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool
foldNEq = foldFrozenEq (\a b -> not (valueEq a b)) NEq

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
peelFrozen = traverse $ \(FieldLit @k e) -> case e of
  Literal v -> Just (FieldLit @k (Literal v))
  _ -> Nothing

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
  _ -> ExprIndex arr idx

foldMathUnary :: MathFn1 -> Expr Stamp 'Number -> Expr Stamp 'Number
foldMathUnary n x = case x of
  Literal (ValueNumber a)
    | Just r <- exactMathUnary n a -> Literal (ValueNumber r)
  _ -> MathUnary n x

foldMathBinary ::
  MathFn2 -> Expr Stamp 'Number -> Expr Stamp 'Number -> Expr Stamp 'Number
foldMathBinary n x y = case (x, y) of
  (Literal (ValueNumber a), Literal (ValueNumber b))
    | Just r <- exactMathBinary n a b -> Literal (ValueNumber r)
  _ -> MathBinary n x y

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

boundAsExpr :: Effect Stamp u -> Expr Stamp u
boundAsExpr (Lift e) = e
boundAsExpr e = UnsafeEffectExpr e

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
      , elimSplice = \t' -> optEffect t' (inlineEff f (boundAsExpr x))
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

optExpr :: Int -> Expr Stamp u -> (Int, Expr Stamp u)
optExpr t0 = \case
  Literal v -> (t0, Literal v)
  Var (Embed e) -> optExpr t0 (flattenExpr e)
  Var v -> (t0, Var v)
  Concat x y ->
    let
      (t1, x') = optExpr t0 x
      (t2, y') = optExpr t1 y
     in
      (t2, foldConcat x' y')
  Plus x y -> binNum (+) Plus x y
  Times x y -> binNum (*) Times x y
  Minus x y -> binNum (-) Minus x y
  FracDiv x y -> binNum (/) FracDiv x y
  Rem x y -> binNum jsRem Rem x y
  BitAnd x y -> binNum (jsBit2 (.&.)) BitAnd x y
  BitOr x y -> binNum (jsBit2 (.|.)) BitOr x y
  BitXor x y -> binNum (jsBit2 xor) BitXor x y
  Shl x y -> binNum jsShl Shl x y
  Shr x y -> binNum jsShr Shr x y
  UShr x y -> binNum jsUShr UShr x y
  Negate x -> unNum negate Negate x
  And x y ->
    let
      (t1, x') = optExpr t0 x
     in
      case x' of
        -- JS && short-circuits: `false && e` does not evaluate `e`.
        Literal (ValueBool False) -> (t1, Literal (ValueBool False))
        Literal (ValueBool True) -> optExpr t1 y
        _ ->
          let
            (t2, y') = optExpr t1 y
           in
            (t2, foldAnd x' y')
  Or x y ->
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
  Eq x y -> optBin t0 foldEq x y
  NEq x y -> optBin t0 foldNEq x y
  GTh x y -> optBin t0 (foldOrd GT GTh) x y
  LTh x y -> optBin t0 (foldOrd LT LTh) x y
  GTEq x y -> optBin t0 (foldOrdNeq LT GTEq) x y
  LTEq x y -> optBin t0 (foldOrdNeq GT LTEq) x y
  Let x f -> optLet t0 x f
  LetRec r b ->
    let
      tag = t0
      (t1, r') = optExpr (t0 - 1) (r (Stamp tag))
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
  Show x ->
    let
      (t1, x') = optExpr t0 x
     in
      (t1, foldShow x')
  TypeOf x ->
    let
      (t1, x') = optExpr t0 x
     in
      (t1, foldTypeOf x')
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
  UnsafeEffectExpr e ->
    let
      (t1, e') = optEffect t0 e
     in
      case e' of
        Lift x -> (t1, x)
        _ -> (t1, UnsafeEffectExpr e')
  ExprUnary n x ->
    let
      (t1, x') = optExpr t0 x
     in
      (t1, ExprUnary n x')
  ExprBinary n x y ->
    let
      (t1, x') = optExpr t0 x
      (t2, y') = optExpr t1 y
     in
      (t2, ExprBinary n x' y')
  ExprTernary n x y z ->
    let
      (t1, x') = optExpr t0 x
      (t2, y') = optExpr t1 y
      (t3, z') = optExpr t2 z
     in
      (t3, ExprTernary n x' y' z')
  ExprMap x f ->
    let
      (t1, x') = optExpr t0 x
      (t2, tag, body) = optUnder t1 f
     in
      (t2, ExprMap x' (keepExprCont t2 tag body f))
  ExprFilter x f ->
    let
      (t1, x') = optExpr t0 x
      (t2, tag, body) = optUnder t1 f
     in
      (t2, ExprFilter x' (keepExprCont t2 tag body f))
  ExprGroupBy x f ->
    let
      (t1, x') = optExpr t0 x
      (t2, tag, body) = optUnder t1 f
     in
      (t2, ExprGroupBy x' (keepExprCont t2 tag body f))
  ExprZipWith x y f ->
    let
      (t1, x') = optExpr t0 x
      (t2, y') = optExpr t1 y
      (t3, tA, tB, body) = optUnder2 t2 f
      wrap a b
        | sizeExpr body <= optSmall = reoptExpr2 t3 f a b
        | otherwise = rebindExpr2 tA tB body a b
     in
      (t3, ExprZipWith x' y' wrap)
  ExprReduce x z f ->
    let
      (t1, x') = optExpr t0 x
      (t2, z') = optExpr t1 z
      (t3, tA, tB, body) = optUnder2 t2 f
      wrap a b
        | sizeExpr body <= optSmall = reoptExpr2 t3 f a b
        | otherwise = rebindExpr2 tA tB body a b
     in
      (t3, ExprReduce x' z' wrap)
  ExprReduceRight x z f ->
    let
      (t1, x') = optExpr t0 x
      (t2, z') = optExpr t1 z
      (t3, tA, tB, body) = optUnder2 t2 f
      wrap a b
        | sizeExpr body <= optSmall = reoptExpr2 t3 f a b
        | otherwise = rebindExpr2 tA tB body a b
     in
      (t3, ExprReduceRight x' z' wrap)
  ExprIndex arr idx ->
    let
      (t1, arr') = optExpr t0 arr
      (t2, idx') = optExpr t1 idx
     in
      (t2, foldIndex arr' idx')
  MathUnary n x ->
    let
      (t1, x') = optExpr t0 x
     in
      (t1, foldMathUnary n x')
  MathBinary n x y ->
    let
      (t1, x') = optExpr t0 x
      (t2, y') = optExpr t1 y
     in
      (t2, foldMathBinary n x' y')
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
 where
  binNum f k x y =
    let
      (t1, x') = optExpr t0 x
      (t2, y') = optExpr t1 y
     in
      (t2, foldNum2 f k x' y')
  unNum f k x =
    let
      (t1, x') = optExpr t0 x
     in
      (t1, foldNum1 f k x')

optEffect :: Int -> Effect Stamp u -> (Int, Effect Stamp u)
optEffect t0 = \case
  Lift x ->
    let
      (t1, x') = optExpr t0 x
     in
      case x' of
        UnsafeEffectExpr e -> (t1, e)
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
      (t1, r') = optEffect (t0 - 1) (r (Stamp tag))
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
  ArraySort xs f ->
    let
      (t1, xs') = optExpr t0 xs
      (t2, tA, tB, body) = optUnder2 t1 f
      wrap a b
        | sizeExpr body <= optSmall = reoptExpr2 t2 f a b
        | otherwise = rebindExpr2 tA tB body a b
     in
      (t2, ArraySort xs' wrap)

mapAccumAST :: (s -> a -> (s, b)) -> s -> [a] -> (s, [b])
mapAccumAST _ t [] = (t, [])
mapAccumAST f t (x : xs) =
  let
    (t1, x') = f t x
    (t2, xs') = mapAccumAST f t1 xs
   in
    (t2, x' : xs')

mapAccumField :: Int -> [FieldLit Stamp r] -> (Int, [FieldLit Stamp r])
mapAccumField = mapAccumAST $ \t (FieldLit @k e) ->
  let
    (t1, e') = optExpr t e
   in
    (t1, FieldLit @k e')

mapAccumEffs :: Int -> [Effect Stamp u] -> (Int, [Effect Stamp u])
mapAccumEffs = mapAccumAST optEffect

mapAccumArms ::
  Int -> [(Text, Effect Stamp u)] -> (Int, [(Text, Effect Stamp u)])
mapAccumArms = mapAccumAST $ \t (k, e) ->
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
  Lift (UnsafeEffectExpr e) -> isUnitWitness e
  Lift _ -> False
  While {} -> True
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
    obj = 'n' : show nObj
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
        rv = 'n' : show n
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
    catchHead = "catch" <+> P.parens (P.text ('n' : show catchN))
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
    <+> P.parens (P.text $ 'n' : show nParam)
    <+> P.braces (decl $$ ret)
 where
  -- Empty ref is Unit (event handlers, forEach of noOp). `return ()`
  -- is a SyntaxError; HughesPJ `parens` of empty is `()`.
  ret
    | P.isEmpty ref = "return"
    | otherwise = "return" <+> P.parens ref

effectfulAST' :: forall v. CG -> Effect Stamp v -> (CG, Code)
effectfulAST' !s0 = \case
  Lift x -> pureAST' s0 x
  FFI fn args ->
    let
      (s1, argDecl, argRefs) = renderArgList argAST s0 args
     in
      (s1, fxCode argDecl (P.text fn <> P.parens argRefs))
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
            cond = P.text ('n' : show nBind) <+> "===" <+> "null"
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
      n = P.text ('n' : show nBind)
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
  ArraySort xs f ->
    let
      (s1, Code xDecl xRef) = pureAST' s0 xs
      (s2, cb) = renderBinaryFn s1 f
      call = xRef <> ".sort" <> P.parens cb
     in
      (s2, fxCode xDecl call)
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
        (s1, exprs) = mapAccumAST (\s x -> pureAST' s (Literal x)) s0 xs
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
    ValueBool True -> (s0, Code mempty "true")
    ValueBool False -> (s0, Code mempty "false")
    ValueFrozen {} -> error "JShark.pureAST: ValueFrozen is eval-only"
  Concat x y -> renderBin "+" s0 x y
  Plus x y -> renderBin "+" s0 x y
  Minus x y -> renderBin "-" s0 x y
  Times x y -> renderBin "*" s0 x y
  FracDiv x y -> renderBin "/" s0 x y
  Rem x y -> renderBin "%" s0 x y
  BitAnd x y -> renderBin "&" s0 x y
  BitOr x y -> renderBin "|" s0 x y
  BitXor x y -> renderBin "^" s0 x y
  Shl x y -> renderBin "<<" s0 x y
  Shr x y -> renderBin ">>" s0 x y
  UShr x y -> renderBin ">>>" s0 x y
  Show x ->
    let
      (s1, Code x1Decl x1Ref) = pureAST' s0 x
     in
      (s1, Code x1Decl $ "String" <> P.parens x1Ref)
  TypeOf x ->
    let
      (s1, Code x1Decl x1Ref) = pureAST' s0 x
      wrapped = case x of
        FrozenLit {} -> P.parens x1Ref
        _ -> x1Ref
     in
      (s1, Code x1Decl $ "typeof" <+> wrapped)
  Negate x ->
    let
      (s1, Code x1Decl x1Ref) = pureAST' s0 x
     in
      (s1, Code x1Decl $ "-" <> P.parens x1Ref)
  Lambda f -> emitExprLambda s0 f
  And x y -> renderBin "&&" s0 x y
  Or x y -> renderBin "||" s0 x y
  Eq x y -> renderBinApp jsValueEq (useHelper HelperEq s0) x y
  NEq x y -> renderBinApp jsValueNEq (useHelper HelperEq s0) x y
  GTh x y -> renderBin ">" s0 x y
  LTh x y -> renderBin "<" s0 x y
  GTEq x y -> renderBin ">=" s0 x y
  LTEq x y -> renderBin "<=" s0 x y
  -- Inline a let used once in a strict position; drop one never used;
  -- `const` when shared or used under a lambda/loop/short-circuit.
  Let x g -> letCode s0 x g
  LetRec r b ->
    let
      (nBind, s1) = allocIdent s0
      n = P.text ('n' : show nBind)
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
  Var s
    -- Tags and the unused-binder dummy are negative; never emit them as JS.
    | stampId s < 0 -> (s0, Code mempty mempty)
    | otherwise -> (s0, Code mempty $ P.text ('n' : show (stampId s)))
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
          optVar = 'n' : show i
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
          optVar = 'n' : show nBind
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
  UnsafeEffectExpr eff -> effectfulAST' s0 eff
  ExprUnary n recv ->
    let
      (s1, Code rDecl rRef) = pureAST' s0 recv
     in
      (s1, Code rDecl (stdUnaryJS n rRef))
  ExprBinary n recv arg ->
    let
      (s1, Code rDecl rRef) = pureAST' s0 recv
      (s2, Code aDecl aRef) = pureAST' s1 arg
     in
      (s2, Code (rDecl $$ aDecl) (stdBinaryJS n rRef aRef))
  ExprTernary n recv a b ->
    let
      (s1, Code rDecl rRef) = pureAST' s0 recv
      (s2, Code aDecl aRef) = pureAST' s1 a
      (s3, Code bDecl bRef) = pureAST' s2 b
     in
      (s3, Code (rDecl $$ aDecl $$ bDecl) (stdTernaryJS n rRef aRef bRef))
  ExprMap recv f -> renderCallbackMethod "map" s0 recv f
  ExprFilter recv f -> renderCallbackMethod "filter" s0 recv f
  ExprGroupBy recv f -> renderGroupBy s0 recv f
  ExprZipWith xs ys f -> renderZipWith s0 xs ys f
  ExprReduce recv z f ->
    let
      (s1, Code rDecl rRef) = pureAST' s0 recv
      (s2, Code zDecl zRef) = pureAST' s1 z
      (s3, cb) = renderBinaryFn s2 f
      call = rRef <> ".reduce" <> P.parens (cb <> ", " <> zRef)
     in
      (s3, Code (rDecl $$ zDecl) call)
  ExprReduceRight recv z f ->
    let
      (s1, Code rDecl rRef) = pureAST' s0 recv
      (s2, Code zDecl zRef) = pureAST' s1 z
      (s3, cb) = renderBinaryFn s2 f
      call = rRef <> ".reduceRight" <> P.parens (cb <> ", " <> zRef)
     in
      (s3, Code (rDecl $$ zDecl) call)
  ExprIndex arr idx ->
    let
      (s1, Code aDecl aRef) = pureAST' s0 arr
      (s2, Code iDecl iRef) = pureAST' s1 idx
     in
      (s2, Code (aDecl $$ iDecl) (jsCheckedIndex aRef iRef))
  MathUnary name x ->
    let
      (s1, Code xDecl xRef) = pureAST' s0 x
     in
      ( s1
      , Code xDecl ("Math." <> P.text (T.unpack (mathFn1Name name)) <> P.parens xRef)
      )
  MathBinary name x y ->
    let
      (s1, Code xDecl xRef) = pureAST' s0 x
      (s2, Code yDecl yRef) = pureAST' s1 y
     in
      ( s2
      , Code
          (xDecl $$ yDecl)
          ( "Math."
              <> P.text (T.unpack (mathFn2Name name))
              <> P.parens (xRef <> ", " <> yRef)
          )
      )
  UnsafeNullable x -> pureAST' s0 x
  FrozenLit fs -> renderObjectLit s0 fs
  GetField @k o ->
    let
      (s1, Code d r) = pureAST' s0 o
     in
      (s1, Code d (jsDotOrBracket r (symbolVal (Proxy @k))))

stdUnaryJS :: StdUnary a b -> Doc -> Doc
stdUnaryJS n r = case n of
  StdToUpper -> r <> ".toUpperCase()"
  StdToLower -> r <> ".toLowerCase()"
  StdTrim -> r <> ".trim()"
  StdArrLen -> dotLength
  StdStrLen -> dotLength
  StdStringify -> "JSON.stringify" <> P.parens r
 where
  dotLength = r <> ".length"

stdBinaryJS :: StdBinary a b c -> Doc -> Doc -> Doc
stdBinaryJS n r a = case n of
  StdIndexOf -> r <> ".indexOf" <> P.parens a
  StdSplit -> r <> ".split" <> P.parens a
  StdIncludes -> r <> ".includes" <> P.parens a
  StdConcat -> r <> ".concat" <> P.parens a
  StdJoin -> r <> ".join" <> P.parens a
  StdTest -> r <> ".test" <> P.parens a
  StdParseInt -> "parseInt" <> P.parens (r <> ", " <> a)

stdTernaryJS :: StdTernary a b c d -> Doc -> Doc -> Doc -> Doc
stdTernaryJS n r a b = case n of
  StdSlice -> slice
  StdArrSlice -> slice
  StdReplace -> r <> ".replace" <> P.parens (a <> ", " <> b)
 where
  slice = r <> ".slice" <> P.parens (a <> ", " <> b)

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
    (s1, cs) = mapAccumAST effectfulAST' s0 es
    (decls, refs) = partitionCode cs
   in
    (s1, Code (P.vcat decls) (P.brackets (P.hcat (P.punctuate ", " refs))))

renderObjectLit :: CG -> [FieldLit Stamp r] -> (CG, Code)
renderObjectLit s0 fs =
  let
    (s1, parts) =
      mapAccumAST
        ( \s fl@(FieldLit e) ->
            let
              (s', Code d r) = pureAST' s e
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
    resultVar = 'n' : show resultN
    renderArm s e =
      let
        (s', Code d r) = effectfulAST' s e
        body = if unit then asStmt d r else d $$ assignResult resultVar r
       in
        (s', body)
    (s3, caseDocs) =
      mapAccumAST
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
        resultVar = 'n' : show resultN
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
    (nA, s1) = allocIdent s0
    (nB, s2) = allocIdent s1
    (s3, Code bDecl bRef) = pureAST' s2 (f (Name nA) (Name nB))
    acc = 'n' : show nA
    x = 'n' : show nB
    cb =
      "function"
        <+> P.parens ((P.text acc <> ",") <+> P.text x)
        <+> P.braces (bDecl $$ "return" <+> bRef)
   in
    (s3, cb)

renderGroupBy ::
  CG
  -> Expr Stamp ('Array u)
  -> (Stamp u -> Expr Stamp 'String)
  -> (CG, Code)
renderGroupBy s00 recv f =
  let
    s0 = useHelper HelperGroupBy s00
    (s1, Code rDecl rRef) = pureAST' s0 recv
    (nParam, s2) = allocIdent s1
    ex = f (Name nParam)
    (s3, Code exDecl exRef) = pureAST' s2 ex
    paramName = 'n' : show nParam
    callback =
      "function"
        <+> P.parens (P.text paramName)
        <+> P.braces (exDecl $$ "return" <+> exRef)
   in
    (s3, Code rDecl (jsGroupBy rRef callback))

renderZipWith ::
  CG
  -> Expr Stamp ('Array a)
  -> Expr Stamp ('Array b)
  -> (Stamp a -> Stamp b -> Expr Stamp c)
  -> (CG, Code)
renderZipWith s00 xs ys f =
  let
    s0 = useHelper HelperZipWith s00
    (s1, Code xDecl xRef) = pureAST' s0 xs
    (s2, Code yDecl yRef) = pureAST' s1 ys
    (nA, s3) = allocIdent s2
    (nB, s4) = allocIdent s3
    ex = f (Name nA) (Name nB)
    (s5, Code exDecl exRef) = pureAST' s4 ex
    callback =
      "function"
        <+> P.parens (P.text ('n' : show nA) <> ("," <+> P.text ('n' : show nB)))
        <+> P.braces (exDecl $$ "return" <+> exRef)
   in
    (s5, Code (xDecl $$ yDecl) (jsZipWith xRef yRef callback))

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
    ex = f (Name nParam)
    (s3, Code exDecl exRef) = pureAST' s2 ex
    paramName = 'n' : show nParam
    callback =
      "function"
        <+> P.parens (P.text paramName)
        <+> P.braces (exDecl $$ "return" <+> exRef)
    call = rRef <> "." <> P.text name <> P.parens callback
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
