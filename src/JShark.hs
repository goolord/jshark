{-# LANGUAGE
    AllowAmbiguousTypes
  , BangPatterns
  , ConstraintKinds
  , DataKinds
  , FlexibleInstances
  , GADTs
  ,     LambdaCase
  , OverloadedStrings
  , PatternSynonyms
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TupleSections
  , TypeAbstractions
  , TypeApplications
  , TypeOperators
#-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module JShark
  ( Expr
    ( Literal, Concat, Plus, Times, Minus, Negate, FracDiv
    , Rem, BitAnd, BitOr, BitXor, Shl, Shr, UShr
    , And, Or, Eq, NEq, GTh, LTh, GTEq, LTEq
    , Let, LetRec, Lambda, Apply, Show, TypeOf, Var, If, OptionCase
    , ResultOk, ResultErr, ResultCase
    , ExprIndex, MathUnary, MathBinary
    , ExprUnary, ExprBinary, ExprTernary, ExprMap, ExprFilter, ExprReduce
    , UnsafeNullable
    , FrozenLit, GetField
    )
  , Value(..)
  , Arg(..)
  , ClosedExpr
  , ClosedEffect
  , Effect
    ( Lift, FFI, UnsafeObject, UnsafeObjectGet, UnsafeObjectAssign
    , CallMethod, Bind, BindRec, LambdaE, ApplyE, IfE, While
    , OptionCaseE, ResultCaseE, Throw, Try, ObjectLit, DeleteProp
    , ArrayLit, ArraySort
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
  ) where

-- Indexed PHOAS (binders @f u@, closed terms @forall f@) in the style of
-- Chlipala / Kmett's parametric HOAS, with host-language sharing as in
-- https://www.reddit.com/r/haskell/comments/85een6/sharing_from_phoas_multiple_interpreters_from_free/dvxhlba
--
-- 'Expr' is the pure tree; 'Effect' is the impure tree. They join at FFI
-- through 'Arg', not by treating effects as expressions.

import Data.Bits ((.&.), (.|.), xor, shiftL, shiftR)
import Data.Char (digitToInt, isSpace)
import qualified Data.Char as Char
import Data.Int (Int32)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Typeable (Typeable, eqT, type (:~:)(..))
import GHC.TypeLits (KnownSymbol, sameSymbol, symbolVal)
import Data.Word (Word32)
import Numeric (readInt, showFFloat, showHex)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, eqStableName, hashStableName, makeStableName)
import Text.PrettyPrint ((<+>), Doc, ($$))
import JShark.Rec
import JShark.Types
import qualified Data.Text as T
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
valueEq ValueFrozen{} ValueFrozen{} =
  error "evaluate: frozen objects cannot be compared for equality"
valueEq (ValueFunction _) (ValueFunction _) =
  error "evaluate: functions cannot be compared for equality"

-- | Only numbers, strings, and booleans support ordering comparisons.
valueCompare :: Value u -> Value u -> Ordering
valueCompare (ValueNumber a) (ValueNumber b) = compare a b
valueCompare (ValueString a) (ValueString b) = compare a b
valueCompare (ValueBool a) (ValueBool b) = compare a b
valueCompare _ _ =
  error "evaluate: only numbers, strings, and booleans support ordering comparisons"

-- | Mimics JS's @String(x)@ coercion closely enough for the reference interpreter.
jsShow :: Value u -> Text
jsShow (ValueNumber d) = T.pack (jsShowNumber d)
jsShow (ValueString s) = s
jsShow (ValueBool True) = "true"
jsShow (ValueBool False) = "false"
jsShow ValueUnit = "undefined"
jsShow (ValueArray xs) = T.intercalate "," (map jsShow xs)
jsShow (ValueOption Nothing) = "null"
jsShow (ValueOption (Just x)) = jsShow x
jsShow (ValueResult (Right x)) = jsShow x
jsShow (ValueResult (Left x)) = jsShow x
jsShow (ValueRegex s) = s
jsShow ValueFrozen{} = "[object Object]"
jsShow (ValueFunction _) = error "evaluate: cannot show a function"

-- | JS @typeof@. @null@ is @\"object\"@.
typeOfValue :: Value u -> Text
typeOfValue = \case
  ValueNumber{} -> "number"
  ValueString{} -> "string"
  ValueBool{} -> "boolean"
  ValueUnit -> "undefined"
  ValueFunction{} -> "function"
  ValueArray{} -> "object"
  ValueOption Nothing -> "object"
  ValueOption (Just v) -> typeOfValue v
  ValueResult{} -> "object"
  ValueRegex{} -> "object"
  ValueFrozen{} -> "object"

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
  MathSqrt | a >= 0, let r = sqrt a, r * r == a -> Just r
  MathFloor | isFiniteDouble a -> Just (fromIntegral (floor a :: Integer))
  MathCeil | isFiniteDouble a -> Just (fromIntegral (ceiling a :: Integer))
  MathRound | isFiniteDouble a -> Just (fromIntegral (floor (a + 0.5) :: Integer))
  MathTrunc | isFiniteDouble a -> Just (fromIntegral (truncate a :: Integer))
  _ -> Nothing

exactMathBinary :: MathFn2 -> Double -> Double -> Maybe Double
exactMathBinary _ _ _ = Nothing

cannotEval :: String -> a
cannotEval what = error ("evaluate: cannot evaluate " ++ what)

isOrderableValue :: Value u -> Bool
isOrderableValue = \case
  ValueNumber{} -> True
  ValueString{} -> True
  ValueBool{} -> True
  _ -> False

eqFoldableValue :: Value u -> Bool
eqFoldableValue ValueFunction{} = False
eqFoldableValue ValueFrozen{} = False
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
  | isNaN a || isNaN b || isInfinite a || b == 0 = 0/0
  | isInfinite b = a
  | otherwise = a - b * fromInteger (truncate (a / b))

jsParseInt :: Text -> Int -> Double
jsParseInt s r
  | r < 2 || r > 36 = 0/0
  | otherwise =
      let t0 = dropWhile isSpace (T.unpack s)
          (neg, t1) = case t0 of
            '-':xs -> (True, xs)
            '+':xs -> (False, xs)
            xs -> (False, xs)
       in case readInt (fromIntegral r :: Integer) okDigit digitToInt t1 of
            (n, _):_ -> fromInteger (if neg then negate n else n)
            [] -> 0/0
  where
    okDigit c =
      let v | c >= '0' && c <= '9' = Char.ord c - Char.ord '0'
            | c >= 'a' && c <= 'z' = Char.ord c - Char.ord 'a' + 10
            | c >= 'A' && c <= 'Z' = Char.ord c - Char.ord 'A' + 10
            | otherwise = 99
       in v < r

-- | JS @Array.prototype.slice@: ToInteger, negatives from the end, clamp.
jsArraySlice :: [a] -> Double -> Double -> [a]
jsArraySlice vs start end =
  let len = length vs
      k = jsSliceClamp len start
      final = jsSliceClamp len end
   in take (max 0 (final - k)) (drop k vs)

jsSliceClamp :: Int -> Double -> Int
jsSliceClamp len x
  | isNaN x = 0
  | isInfinite x && x < 0 = 0
  | isInfinite x = len
  | otherwise =
      let n = truncate x :: Int
       in if n < 0 then max 0 (len + n) else min n len

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
          let h = showHex (Char.ord c) ""
           in "\\u" ++ replicate (4 - length h) '0' ++ h
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

peelResult :: Expr Stamp ('Result e a) -> Maybe (Either (Expr Stamp e) (Expr Stamp a))
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

evaluateNumber :: ClosedExpr 'Number -> Double
evaluateNumber e = unNumber (evaluate e)

-- | Pure reference interpreter. Shared Haskell heap nodes are walked
-- once per occurrence (no memo table). Use 'evaluateCached' when host-level
-- sharing should be observed.
evaluate :: ClosedExpr u -> Value u
evaluate = evalValue

evalValue :: Expr Value v -> Value v
evalValue = \case
    Literal v -> v
    Plus x y -> ValueNumber (unNumber (evalValue x) + unNumber (evalValue y))
    Times x y -> ValueNumber (unNumber (evalValue x) * unNumber (evalValue y))
    Minus x y -> ValueNumber (unNumber (evalValue x) - unNumber (evalValue y))
    Negate x -> ValueNumber (negate (unNumber (evalValue x)))
    FracDiv x y -> ValueNumber (unNumber (evalValue x) / unNumber (evalValue y))
    Rem x y -> ValueNumber (jsRem (unNumber (evalValue x)) (unNumber (evalValue y)))
    BitAnd x y -> ValueNumber (jsBit2 (.&.) (unNumber (evalValue x)) (unNumber (evalValue y)))
    BitOr x y -> ValueNumber (jsBit2 (.|.) (unNumber (evalValue x)) (unNumber (evalValue y)))
    BitXor x y -> ValueNumber (jsBit2 xor (unNumber (evalValue x)) (unNumber (evalValue y)))
    Shl x y -> ValueNumber (jsShl (unNumber (evalValue x)) (unNumber (evalValue y)))
    Shr x y -> ValueNumber (jsShr (unNumber (evalValue x)) (unNumber (evalValue y)))
    UShr x y -> ValueNumber (jsUShr (unNumber (evalValue x)) (unNumber (evalValue y)))
    Var x -> x
    Apply g x -> unFunction (evalValue g) (evalValue x)
    Lambda g -> ValueFunction (evalValue . g)
    Concat x y -> ValueString (unString (evalValue x) <> unString (evalValue y))
    Show x -> ValueString (jsShow (evalValue x))
    TypeOf x -> ValueString (typeOfValue (evalValue x))
    And x y -> ValueBool (unBool (evalValue x) && unBool (evalValue y))
    Or x y -> ValueBool (unBool (evalValue x) || unBool (evalValue y))
    Eq x y -> ValueBool (valueEq (evalValue x) (evalValue y))
    NEq x y -> ValueBool (not (valueEq (evalValue x) (evalValue y)))
    GTh x y -> ValueBool (valueCompare (evalValue x) (evalValue y) == GT)
    LTh x y -> ValueBool (valueCompare (evalValue x) (evalValue y) == LT)
    GTEq x y -> ValueBool (valueCompare (evalValue x) (evalValue y) /= LT)
    LTEq x y -> ValueBool (valueCompare (evalValue x) (evalValue y) /= GT)
    Let x g -> evalValue (g (evalValue x))
    LetRec r b ->
      let rec = case r rec of
            Lambda g -> ValueFunction (evalValue . g)
            _ -> error "evaluate: LetRec rhs must be a Lambda"
       in evalValue (b rec)
    If c t e -> if unBool (evalValue c) then evalValue t else evalValue e
    OptionCase opt none' someF -> case evalValue opt of
      ValueOption Nothing -> evalValue none'
      ValueOption (Just x) -> evalValue (someF x)
    ResultOk x -> ValueResult (Right (evalValue x))
    ResultErr x -> ValueResult (Left (evalValue x))
    ResultCase res errF okF -> case evalValue res of
      ValueResult (Left e) -> evalValue (errF e)
      ValueResult (Right a) -> evalValue (okF a)
    UnsafeEffectExpr _ -> cannotEval "an embedded Effect (UnsafeEffectExpr)"
    ExprUnary{} -> cannotEval "a stdlib ExprUnary"
    ExprBinary n x y -> case n of
      StdParseInt ->
        ValueNumber (jsParseInt (unString (evalValue x)) (truncate (unNumber (evalValue y))))
      StdTest -> cannotEval "RegExp.test"
      _ -> cannotEval "a stdlib ExprBinary"
    ExprTernary n xs a b -> case n of
      StdArrSlice -> case evalValue xs of
        ValueArray vs ->
          ValueArray (jsArraySlice vs (unNumber (evalValue a)) (unNumber (evalValue b)))
      _ -> cannotEval "a stdlib ExprTernary"
    ExprMap{} -> cannotEval "ExprMap"
    ExprFilter{} -> cannotEval "ExprFilter"
    ExprReduce xs z f -> case evalValue xs of
      ValueArray vs ->
        foldl (\acc v -> evalValue (f acc v)) (evalValue z) vs
    ExprIndex xs i -> case evalValue xs of
      ValueArray vs ->
        -- JS array indexing truncates the index toward zero (as part of
        -- ToIntegerOrInfinity) rather than rounding, and returns @undefined@
        -- out of bounds rather than crashing; we can't represent
        -- @undefined@ generically here (there's no 'Value' inhabitant for
        -- an arbitrary universe @u@), so out-of-bounds access is a hard
        -- error in the reference interpreter.
        let idx = truncate (unNumber (evalValue i)) :: Int
         in if idx >= 0 && idx < length vs
              then vs !! idx
              else error "evaluate: array index out of bounds"
    MathUnary name x -> ValueNumber (mathUnaryFn name (unNumber (evalValue x)))
    MathBinary name x y -> ValueNumber (mathBinaryFn name (unNumber (evalValue x)) (unNumber (evalValue y)))
    UnsafeNullable x -> ValueOption (Just (evalValue x))
    FrozenLit fs -> ValueFrozen fs
    GetField @k o -> withFrozenField @k (evalValue o) evalValue

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
      modifyIORef' cache
        (IM.insertWith (++) (hashStableName sn) [CacheEntry sn v])
      pure v

lookupCache :: forall v. Typeable v => StableName (Expr Value v) -> Maybe [CacheEntry] -> Maybe (Value v)
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

-- | Memoize when the node’s result universe is a concrete 'Typeable'
-- type; otherwise evaluate without the table.
goOpen :: EvalCache -> Expr Value v -> IO (Value v)
goOpen cache e = case e of
  Plus{} -> go cache e
  Times{} -> go cache e
  Minus{} -> go cache e
  Negate{} -> go cache e
  FracDiv{} -> go cache e
  Rem{} -> go cache e
  BitAnd{} -> go cache e
  BitOr{} -> go cache e
  BitXor{} -> go cache e
  Shl{} -> go cache e
  Shr{} -> go cache e
  UShr{} -> go cache e
  Concat{} -> go cache e
  Show{} -> go cache e
  TypeOf{} -> go cache e
  And{} -> go cache e
  Or{} -> go cache e
  Eq{} -> go cache e
  NEq{} -> go cache e
  GTh{} -> go cache e
  LTh{} -> go cache e
  GTEq{} -> go cache e
  LTEq{} -> go cache e
  MathUnary{} -> go cache e
  MathBinary{} -> go cache e
  Literal ValueNumber{} -> go cache e
  Literal ValueString{} -> go cache e
  Literal ValueBool{} -> go cache e
  Literal ValueUnit -> go cache e
  Literal ValueRegex{} -> go cache e
  FrozenLit fs -> pure (ValueFrozen fs)
  GetField @k o -> do
    ov <- goOpen cache o
    withFrozenField @k ov (goOpen cache)
  _ -> pure (evalValue e)

goNode :: forall v. Typeable v => EvalCache -> Expr Value v -> IO (Value v)
goNode cache = \case
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
  Apply g x -> unFunction <$> goOpen cache g <*> goOpen cache x
  Lambda g -> pure (ValueFunction (applyCached cache g))
  Concat x y -> do
    a <- go cache x
    b <- go cache y
    pure (ValueString (unString a <> unString b))
  Show x -> ValueString . jsShow <$> goOpen cache x
  TypeOf x -> ValueString . typeOfValue <$> goOpen cache x
  And x y -> do
    a <- go cache x
    if unBool a then go cache y else pure (ValueBool False)
  Or x y -> do
    a <- go cache x
    if unBool a then pure (ValueBool True) else go cache y
  Eq x y -> ValueBool <$> (valueEq <$> goOpen cache x <*> goOpen cache y)
  NEq x y -> ValueBool . not <$> (valueEq <$> goOpen cache x <*> goOpen cache y)
  GTh x y -> ValueBool . (== GT) <$> (valueCompare <$> goOpen cache x <*> goOpen cache y)
  LTh x y -> ValueBool . (== LT) <$> (valueCompare <$> goOpen cache x <*> goOpen cache y)
  GTEq x y -> ValueBool . (/= LT) <$> (valueCompare <$> goOpen cache x <*> goOpen cache y)
  LTEq x y -> ValueBool . (/= GT) <$> (valueCompare <$> goOpen cache x <*> goOpen cache y)
  Let x g -> goOpen cache x >>= go cache . g
  LetRec r b ->
    let rec = case r rec of
          Lambda g -> ValueFunction (applyCached cache g)
          _ -> error "evaluateCached: LetRec rhs must be a Lambda"
     in go cache (b rec)
  If c t e -> do
    cv <- go cache c
    if unBool cv then go cache t else go cache e
  OptionCase opt none' someF -> do
    ov <- goOpen cache opt
    case ov of
      ValueOption Nothing -> go cache none'
      ValueOption (Just x) -> go cache (someF x)
  ResultOk x -> ValueResult . Right <$> goOpen cache x
  ResultErr x -> ValueResult . Left <$> goOpen cache x
  ResultCase res errF okF -> do
    rv <- goOpen cache res
    case rv of
      ValueResult (Left e) -> go cache (errF e)
      ValueResult (Right a) -> go cache (okF a)
  UnsafeEffectExpr _ -> cannotEval "an embedded Effect (UnsafeEffectExpr)"
  ExprUnary{} -> cannotEval "a stdlib ExprUnary"
  ExprBinary{} -> cannotEval "a stdlib ExprBinary"
  ExprTernary n xs a b -> case n of
    StdArrSlice -> do
      arr <- goOpen cache xs
      i <- go cache a
      j <- go cache b
      case arr of
        ValueArray vs ->
          pure (ValueArray (jsArraySlice vs (unNumber i) (unNumber j)))
    _ -> cannotEval "a stdlib ExprTernary"
  ExprMap{} -> cannotEval "ExprMap"
  ExprFilter{} -> cannotEval "ExprFilter"
  ExprReduce xs z f -> do
    arr <- goOpen cache xs
    z0 <- go cache z
    case arr of
      ValueArray vs -> goFold z0 vs
    where
      goFold acc [] = pure acc
      goFold acc (v:vs) = do
        acc' <- go cache (f acc v)
        goFold acc' vs
  ExprIndex xs i -> do
    arr <- goOpen cache xs
    iv <- go cache i
    case arr of
      ValueArray vs ->
        let idx = truncate (unNumber iv) :: Int
         in if idx >= 0 && idx < length vs
              then pure (vs !! idx)
              else error "evaluate: array index out of bounds"
  MathUnary name x -> ValueNumber . mathUnaryFn name . unNumber <$> go cache x
  MathBinary name x y ->
    ValueNumber <$> (mathBinaryFn name <$> (unNumber <$> go cache x) <*> (unNumber <$> go cache y))
  UnsafeNullable x -> ValueOption . Just <$> goOpen cache x
  FrozenLit fs -> pure (ValueFrozen fs)
  GetField @k o -> do
    ov <- goOpen cache o
    withFrozenField @k ov (goOpen cache)
  where
    num1 f x = ValueNumber . f . unNumber <$> go cache x
    num2 f x y = ValueNumber <$> (f <$> (unNumber <$> go cache x) <*> (unNumber <$> go cache y))

fromRightE :: Either [Char] c -> c
fromRightE = either error id

printComputation :: Doc -> IO ()
printComputation computation = putStrLn (renderJSCompact computation)

renderJS :: Doc -> String
renderJS = P.renderStyle P.style

-- | Linear dump. HughesPJ 'PageMode' on a large inlined @bindRec@
-- body is superlinear (breakout sat in 'renderJS' for tens of seconds).
-- 'compileEffect' uses this, then 'prettyJS' for 'Readable'.
renderJSCompact :: Doc -> String
renderJSCompact = P.renderStyle P.style { P.mode = P.LeftMode }

-- | @o.foo@ when @foo@ is an identifier; @o["0"]@ otherwise.
jsDotOrBracket :: Doc -> String -> Doc
jsDotOrBracket obj key
  | jsIdent key = obj <> "." <> P.text key
  | otherwise = obj <> "[" <> P.doubleQuotes (P.text key) <> "]"

jsIdent :: String -> Bool
jsIdent [] = False
jsIdent (c:cs) = jsIdStart c && all jsIdPart cs
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

-- | Wrap generated decls + result in an IIFE so a minifier treats the
-- result as live (plain expression statements get DCE'd).
renderIIFE :: Code -> Doc
renderIIFE (MkCode decls ref _) =
  let body = if P.isEmpty ref then decls else decls $$ (("return" <+> ref) <> P.semi)
   in "(() => {" $$ P.nest 2 body $$ "})()"

-- | Pure expression compiled to a self-contained JS program (IIFE).
pureProgram :: ClosedExpr u -> Doc
pureProgram e = renderIIFE . snd . pureAST' startCG $ optimize e

-- | Effectful computation compiled to a self-contained JS program (IIFE).
effectfulProgram :: ClosedEffect u -> Doc
effectfulProgram e = renderIIFE . snd . effectfulAST' startCG $ optimizeEffect e

partitionCode :: [Code] -> ([Doc], [Doc])
partitionCode ((MkCode a b _):cs) = let (as,bs) = partitionCode cs in ((a:as),(b:bs))
partitionCode [] = ([], [])

-- Codegen counters: `cgIdent` is the next emitted JS name (`n0`, `n1`, …);
-- `cgTag` is a decreasing negative id used only for use-counting/inlining
-- so nested Lets/Binds cannot collide (tags are never valid JS idents).
data CG = CG
  { cgIdent :: {-# UNPACK #-} !Int
  , cgTag :: {-# UNPACK #-} !Int
  }

startCG :: CG
startCG = CG 0 (-2)

allocTag :: CG -> (Int, CG)
allocTag (CG n t) = (t, CG n (t - 1))

allocIdent :: CG -> (Int, CG)
allocIdent (CG n t) = (n, CG (n + 1) t)

nestedDummyId :: Int
nestedDummyId = minBound

nestedDummy :: Stamp u
nestedDummy = Name nestedDummyId

constBind :: Int -> Doc -> Doc
constBind n ref = ("const" <+> P.text ('n' : show n) <+> "=" <+> ref) <> P.semi

jsCall :: Doc -> Doc -> Doc
jsCall f a = P.parens f <> P.parens a

isSimple :: Expr Stamp u -> Bool
isSimple = \case
  Literal{} -> True
  Var{} -> True
  Show{} -> True
  TypeOf{} -> True
  Negate{} -> True
  ExprUnary{} -> True
  ExprBinary{} -> True
  ExprTernary{} -> True
  ExprMap{} -> True
  ExprFilter{} -> True
  ExprIndex{} -> True
  MathUnary{} -> True
  MathBinary{} -> True
  UnsafeNullable x -> isSimple x
  FrozenLit{} -> True
  GetField{} -> True
  -- Single-use Effect spliced into an Expr hole (e.g. inlined 'ffi').
  UnsafeEffectExpr e -> isSimpleEffect e
  _ -> False

isSimpleEffect :: Effect Stamp u -> Bool
isSimpleEffect = \case
  Lift x -> isSimple x
  FFI{} -> True
  CallMethod{} -> True
  UnsafeObject{} -> True
  UnsafeObjectGet{} -> True
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

countArgs :: Int -> Rec (Arg Stamp) us -> Int
countArgs t = recFold (\n a -> n + countArg t a) 0

countArg :: Int -> Arg Stamp u -> Int
countArg t (ArgExpr e) = countExpr t e
countArg t (ArgEffect e) = countEffect t e

countExpr :: Int -> Expr Stamp u -> Int
countExpr t = \case
  Literal{} -> 0
  Var s -> case s of
    Stamp i -> if i == t then 1 else 0
    Embed e -> countExpr t e
  Concat x y -> countExpr t x + countExpr t y
  Plus x y -> countExpr t x + countExpr t y
  Times x y -> countExpr t x + countExpr t y
  Minus x y -> countExpr t x + countExpr t y
  Negate x -> countExpr t x
  FracDiv x y -> countExpr t x + countExpr t y
  Rem x y -> countExpr t x + countExpr t y
  BitAnd x y -> countExpr t x + countExpr t y
  BitOr x y -> countExpr t x + countExpr t y
  BitXor x y -> countExpr t x + countExpr t y
  Shl x y -> countExpr t x + countExpr t y
  Shr x y -> countExpr t x + countExpr t y
  UShr x y -> countExpr t x + countExpr t y
  And x y -> countExpr t x + countLazyExpr t y
  Or x y -> countExpr t x + countLazyExpr t y
  Eq x y -> countExpr t x + countExpr t y
  NEq x y -> countExpr t x + countExpr t y
  GTh x y -> countExpr t x + countExpr t y
  LTh x y -> countExpr t x + countExpr t y
  GTEq x y -> countExpr t x + countExpr t y
  LTEq x y -> countExpr t x + countExpr t y
  Let x g -> countExpr t x + countExpr t (g nestedDummy)
  LetRec r b -> countLazyExpr t (r nestedDummy) + countExpr t (b nestedDummy)
  Lambda g -> countLazyExpr t (g nestedDummy)
  Apply f x -> countExpr t f + countExpr t x
  Show x -> countExpr t x
  TypeOf x -> countExpr t x
  If c u v -> countExpr t c + countLazyExpr t u + countLazyExpr t v
  OptionCase o n s ->
    countExpr t o + countLazyExpr t n + countLazyExpr t (s nestedDummy)
  ResultOk x -> countExpr t x
  ResultErr x -> countExpr t x
  ResultCase o e s ->
    countExpr t o + countLazyExpr t (e nestedDummy) + countLazyExpr t (s nestedDummy)
  UnsafeEffectExpr e -> countEffect t e
  ExprUnary _ x -> countExpr t x
  ExprBinary _ x y -> countExpr t x + countExpr t y
  ExprTernary _ x y z -> countExpr t x + countExpr t y + countExpr t z
  ExprMap x f -> countExpr t x + countLazyExpr t (f nestedDummy)
  ExprFilter x f -> countExpr t x + countLazyExpr t (f nestedDummy)
  ExprReduce x z f ->
    countExpr t x + countExpr t z + countLazyExpr t (f nestedDummy nestedDummy)
  ExprIndex x i -> countExpr t x + countExpr t i
  MathUnary _ x -> countExpr t x
  MathBinary _ x y -> countExpr t x + countExpr t y
  UnsafeNullable x -> countExpr t x
  FrozenLit fs -> sum (map (countFieldLit t) fs)
  GetField o -> countExpr t o

countEffect :: Int -> Effect Stamp u -> Int
countEffect t = \case
  Lift x -> countExpr t x
  FFI _ args -> countArgs t args
  UnsafeObject _ -> 0
  UnsafeObjectGet x _ -> countEffect t x
  UnsafeObjectAssign x y -> countEffect t x + countEffect t y
  CallMethod x _ args -> countEffect t x + countArgs t args
  Bind x f -> countEffect t x + countEffect t (f nestedDummy)
  BindRec r b -> countLazyEffect t (r nestedDummy) + countEffect t (b nestedDummy)
  LambdaE f -> countLazyEffect t (f nestedDummy)
  ApplyE f x -> countEffect t f + countEffect t x
  IfE c u v -> countEffect t c + countLazyEffect t u + countLazyEffect t v
  While c b -> countLazyEffect t c + countLazyEffect t b
  OptionCaseE o n s ->
    countExpr t o + countLazyEffect t n + countLazyEffect t (s nestedDummy)
  ResultCaseE o e s ->
    countExpr t o + countLazyEffect t (e nestedDummy) + countLazyEffect t (s nestedDummy)
  Throw x -> countExpr t x
  Try a k -> countEffect t a + countLazyEffect t (k nestedDummy)
  ObjectLit fs -> sum (map (countFieldLit t) fs)
  DeleteProp o k -> countEffect t o + countExpr t k
  ArrayLit es -> sum (map (countEffect t) es)
  ArraySort xs f -> countExpr t xs + countLazyExpr t (f nestedDummy nestedDummy)

countFieldLit :: Int -> FieldLit Stamp r -> Int
countFieldLit t (FieldLit e) = countExpr t e

-- Re-opt only small trees. A second walk of a @bindRec@ / do-chain
-- paint body is what hung todo-mvc and breakout.
optSmall :: Int
optSmall = 16

sizeFieldLit :: FieldLit Stamp r -> Int
sizeFieldLit (FieldLit e) = sizeExpr e

sizeExpr :: Expr Stamp u -> Int
sizeExpr = \case
  Literal{} -> 1
  Var (Embed e) -> sizeExpr e
  Var{} -> 1
  Concat x y -> 1 + sizeExpr x + sizeExpr y
  Plus x y -> 1 + sizeExpr x + sizeExpr y
  Times x y -> 1 + sizeExpr x + sizeExpr y
  Minus x y -> 1 + sizeExpr x + sizeExpr y
  Negate x -> 1 + sizeExpr x
  FracDiv x y -> 1 + sizeExpr x + sizeExpr y
  Rem x y -> 1 + sizeExpr x + sizeExpr y
  BitAnd x y -> 1 + sizeExpr x + sizeExpr y
  BitOr x y -> 1 + sizeExpr x + sizeExpr y
  BitXor x y -> 1 + sizeExpr x + sizeExpr y
  Shl x y -> 1 + sizeExpr x + sizeExpr y
  Shr x y -> 1 + sizeExpr x + sizeExpr y
  UShr x y -> 1 + sizeExpr x + sizeExpr y
  And x y -> 1 + sizeExpr x + sizeExpr y
  Or x y -> 1 + sizeExpr x + sizeExpr y
  Eq x y -> 1 + sizeExpr x + sizeExpr y
  NEq x y -> 1 + sizeExpr x + sizeExpr y
  GTh x y -> 1 + sizeExpr x + sizeExpr y
  LTh x y -> 1 + sizeExpr x + sizeExpr y
  GTEq x y -> 1 + sizeExpr x + sizeExpr y
  LTEq x y -> 1 + sizeExpr x + sizeExpr y
  Let x g -> 1 + sizeExpr x + sizeExpr (g nestedDummy)
  LetRec r b -> 1 + sizeExpr (r nestedDummy) + sizeExpr (b nestedDummy)
  Lambda g -> 1 + sizeExpr (g nestedDummy)
  Apply f x -> 1 + sizeExpr f + sizeExpr x
  Show x -> 1 + sizeExpr x
  TypeOf x -> 1 + sizeExpr x
  If c u v -> 1 + sizeExpr c + sizeExpr u + sizeExpr v
  OptionCase o n s -> 1 + sizeExpr o + sizeExpr n + sizeExpr (s nestedDummy)
  ResultOk x -> 1 + sizeExpr x
  ResultErr x -> 1 + sizeExpr x
  ResultCase o e s -> 1 + sizeExpr o + sizeExpr (e nestedDummy) + sizeExpr (s nestedDummy)
  UnsafeEffectExpr e -> 1 + sizeEffect e
  ExprUnary _ x -> 1 + sizeExpr x
  ExprBinary _ x y -> 1 + sizeExpr x + sizeExpr y
  ExprTernary _ x y z -> 1 + sizeExpr x + sizeExpr y + sizeExpr z
  ExprMap x f -> 1 + sizeExpr x + sizeExpr (f nestedDummy)
  ExprFilter x f -> 1 + sizeExpr x + sizeExpr (f nestedDummy)
  ExprReduce x z f -> 1 + sizeExpr x + sizeExpr z + sizeExpr (f nestedDummy nestedDummy)
  ExprIndex x i -> 1 + sizeExpr x + sizeExpr i
  MathUnary _ x -> 1 + sizeExpr x
  MathBinary _ x y -> 1 + sizeExpr x + sizeExpr y
  UnsafeNullable x -> 1 + sizeExpr x
  FrozenLit fs -> 1 + sum (map sizeFieldLit fs)
  GetField o -> 1 + sizeExpr o

sizeEffect :: Effect Stamp u -> Int
sizeEffect = \case
  Lift x -> 1 + sizeExpr x
  FFI _ args -> 1 + recFold (\n a -> n + sizeArg a) 0 args
  UnsafeObject{} -> 1
  UnsafeObjectGet x _ -> 1 + sizeEffect x
  UnsafeObjectAssign x y -> 1 + sizeEffect x + sizeEffect y
  CallMethod x _ args -> 1 + sizeEffect x + recFold (\n a -> n + sizeArg a) 0 args
  Bind x f -> 1 + sizeEffect x + sizeEffect (f nestedDummy)
  BindRec r b -> 1 + sizeEffect (r nestedDummy) + sizeEffect (b nestedDummy)
  LambdaE f -> 1 + sizeEffect (f nestedDummy)
  ApplyE f x -> 1 + sizeEffect f + sizeEffect x
  IfE c u v -> 1 + sizeEffect c + sizeEffect u + sizeEffect v
  While c b -> 1 + sizeEffect c + sizeEffect b
  OptionCaseE o n s -> 1 + sizeExpr o + sizeEffect n + sizeEffect (s nestedDummy)
  ResultCaseE o e s -> 1 + sizeExpr o + sizeEffect (e nestedDummy) + sizeEffect (s nestedDummy)
  Throw x -> 1 + sizeExpr x
  Try a k -> 1 + sizeEffect a + sizeEffect (k nestedDummy)
  ObjectLit fs -> 1 + sum (map sizeFieldLit fs)
  DeleteProp o k -> 1 + sizeEffect o + sizeExpr k
  ArrayLit es -> 1 + sum (map sizeEffect es)
  ArraySort xs f -> 1 + sizeExpr xs + sizeExpr (f nestedDummy nestedDummy)

sizeArg :: Arg Stamp u -> Int
sizeArg (ArgExpr e) = sizeExpr e
sizeArg (ArgEffect e) = sizeEffect e

-- | First-order reopen: rename the tag allocated by 'optUnder'. Never
-- re-applies the original PHOAS @f@.
rebindExpr :: Int -> Expr Stamp v -> Stamp u -> Expr Stamp v
rebindExpr tag body s = renameExpr tag (stampId s) body

rebindEff :: Int -> Effect Stamp v -> Stamp u -> Effect Stamp v
rebindEff tag body s = renameEff tag (stampId s) body

rebindExpr2 :: Int -> Int -> Expr Stamp v -> Stamp a -> Stamp b -> Expr Stamp v
rebindExpr2 tA tB body a b =
  renameExpr tA (stampId a) (renameExpr tB (stampId b) body)

keepExprCont
  :: Int
  -> Int
  -> Expr Stamp v
  -> (Stamp u -> Expr Stamp v)
  -> Stamp u
  -> Expr Stamp v
keepExprCont t tag body f
  | sizeExpr body <= optSmall = reoptExpr t f
  | otherwise = rebindExpr tag body

keepEffCont
  :: Int
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

lookupField :: forall k r f. KnownSymbol k => [FieldLit f r] -> Maybe (Expr f (Field r k))
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
projectFrozenField :: forall k r f. (KnownSymbol k, PhoasDummy f) => [FieldLit f r] -> Maybe (Expr f (Field r k))
projectFrozenField fs
  | fieldsPure fs = lookupField @k fs
  | otherwise = Nothing

withFrozenField
  :: forall k r a. KnownSymbol k
  => Value ('Object r)
  -> (Expr Value (Field r k) -> a)
  -> a
withFrozenField (ValueFrozen fs) k =
  case projectFrozenField @k fs of
    Just e -> k e
    Nothing -> cannotEval "GetField of a frozen object with effectful fields"

foldGetField :: forall k r f. (KnownSymbol k, PhoasDummy f) => Expr f ('Object r) -> Maybe (Expr f (Field r k))
foldGetField = \case
  FrozenLit fs -> projectFrozenField @k fs
  If (Literal (ValueBool True)) t _ -> foldGetField @k t
  If (Literal (ValueBool False)) _ e -> foldGetField @k e
  _ -> Nothing

flattenArg :: Arg Stamp u -> Arg Stamp u
flattenArg (ArgExpr e) = ArgExpr (flattenExpr e)
flattenArg (ArgEffect e) = ArgEffect (flattenEff e)

-- | Unwrap 'Embed' holes. The universe of the hole is the universe of the
-- 'Var', so this is ordinary GADT coverage — not a cast.
flattenExpr :: Expr Stamp u -> Expr Stamp u
flattenExpr = \case
  Var (Embed e) -> flattenExpr e
  Var s -> Var s
  Literal x -> Literal x
  Concat x y -> Concat (flattenExpr x) (flattenExpr y)
  Plus x y -> Plus (flattenExpr x) (flattenExpr y)
  Times x y -> Times (flattenExpr x) (flattenExpr y)
  Minus x y -> Minus (flattenExpr x) (flattenExpr y)
  Negate x -> Negate (flattenExpr x)
  FracDiv x y -> FracDiv (flattenExpr x) (flattenExpr y)
  Rem x y -> Rem (flattenExpr x) (flattenExpr y)
  BitAnd x y -> BitAnd (flattenExpr x) (flattenExpr y)
  BitOr x y -> BitOr (flattenExpr x) (flattenExpr y)
  BitXor x y -> BitXor (flattenExpr x) (flattenExpr y)
  Shl x y -> Shl (flattenExpr x) (flattenExpr y)
  Shr x y -> Shr (flattenExpr x) (flattenExpr y)
  UShr x y -> UShr (flattenExpr x) (flattenExpr y)
  And x y -> And (flattenExpr x) (flattenExpr y)
  Or x y -> Or (flattenExpr x) (flattenExpr y)
  Eq x y -> Eq (flattenExpr x) (flattenExpr y)
  NEq x y -> NEq (flattenExpr x) (flattenExpr y)
  GTh x y -> GTh (flattenExpr x) (flattenExpr y)
  LTh x y -> LTh (flattenExpr x) (flattenExpr y)
  GTEq x y -> GTEq (flattenExpr x) (flattenExpr y)
  LTEq x y -> LTEq (flattenExpr x) (flattenExpr y)
  Let x g -> Let (flattenExpr x) (flattenExpr . g)
  LetRec rhs body -> LetRec (flattenExpr . rhs) (flattenExpr . body)
  Lambda g -> Lambda (flattenExpr . g)
  Apply f x -> Apply (flattenExpr f) (flattenExpr x)
  Show x -> Show (flattenExpr x)
  TypeOf x -> TypeOf (flattenExpr x)
  If c u v -> If (flattenExpr c) (flattenExpr u) (flattenExpr v)
  OptionCase o n s -> OptionCase (flattenExpr o) (flattenExpr n) (flattenExpr . s)
  ResultOk x -> ResultOk (flattenExpr x)
  ResultErr x -> ResultErr (flattenExpr x)
  ResultCase o e s -> ResultCase (flattenExpr o) (flattenExpr . e) (flattenExpr . s)
  UnsafeEffectExpr e -> UnsafeEffectExpr (flattenEff e)
  ExprUnary n x -> ExprUnary n (flattenExpr x)
  ExprBinary n x y -> ExprBinary n (flattenExpr x) (flattenExpr y)
  ExprTernary n x y z -> ExprTernary n (flattenExpr x) (flattenExpr y) (flattenExpr z)
  ExprMap x f -> ExprMap (flattenExpr x) (flattenExpr . f)
  ExprFilter x f -> ExprFilter (flattenExpr x) (flattenExpr . f)
  ExprReduce x z f -> ExprReduce (flattenExpr x) (flattenExpr z) (\a b -> flattenExpr (f a b))
  ExprIndex x i -> ExprIndex (flattenExpr x) (flattenExpr i)
  MathUnary n x -> MathUnary n (flattenExpr x)
  MathBinary n x y -> MathBinary n (flattenExpr x) (flattenExpr y)
  UnsafeNullable x -> UnsafeNullable (flattenExpr x)
  FrozenLit fs -> FrozenLit (map (mapFieldLit flattenExpr) fs)
  GetField @k o -> GetField @k (flattenExpr o)

flattenEff :: Effect Stamp u -> Effect Stamp u
flattenEff = \case
  Lift x -> Lift (flattenExpr x)
  FFI n args -> FFI n (mapRec flattenArg args)
  UnsafeObject o -> UnsafeObject o
  UnsafeObjectGet x s -> UnsafeObjectGet (flattenEff x) s
  UnsafeObjectAssign x y -> UnsafeObjectAssign (flattenEff x) (flattenEff y)
  CallMethod x n args -> CallMethod (flattenEff x) n (mapRec flattenArg args)
  Bind x f -> Bind (flattenEff x) (flattenEff . f)
  BindRec rhs body -> BindRec (flattenEff . rhs) (flattenEff . body)
  LambdaE f -> LambdaE (flattenEff . f)
  ApplyE f x -> ApplyE (flattenEff f) (flattenEff x)
  IfE c u v -> IfE (flattenEff c) (flattenEff u) (flattenEff v)
  While c b -> While (flattenEff c) (flattenEff b)
  OptionCaseE o n s -> OptionCaseE (flattenExpr o) (flattenEff n) (flattenEff . s)
  ResultCaseE o e s -> ResultCaseE (flattenExpr o) (flattenEff . e) (flattenEff . s)
  Throw x -> Throw (flattenExpr x)
  Try a k -> Try (flattenEff a) (flattenEff . k)
  ObjectLit fs -> ObjectLit (map (mapFieldLit flattenExpr) fs)
  DeleteProp o k -> DeleteProp (flattenEff o) (flattenExpr k)
  ArrayLit es -> ArrayLit (map flattenEff es)
  ArraySort xs f -> ArraySort (flattenExpr xs) (\a b -> flattenExpr (f a b))

renameArg :: Int -> Int -> Arg Stamp u -> Arg Stamp u
renameArg old new (ArgExpr e) = ArgExpr (renameExpr old new e)
renameArg old new (ArgEffect e) = ArgEffect (renameEff old new e)

-- | Replace 'Stamp' @old@ with @new@. Phantom in the universe, so this
-- does not need a cast. Used after the one 'optUnder' apply of @f@.
renameExpr :: Int -> Int -> Expr Stamp u -> Expr Stamp u
renameExpr old new = \case
  Var (Embed e) -> renameExpr old new (flattenExpr e)
  Var (Stamp t) | t == old -> Var (Stamp new)
  Var s -> Var s
  Literal x -> Literal x
  Concat x y -> Concat (renameExpr old new x) (renameExpr old new y)
  Plus x y -> Plus (renameExpr old new x) (renameExpr old new y)
  Times x y -> Times (renameExpr old new x) (renameExpr old new y)
  Minus x y -> Minus (renameExpr old new x) (renameExpr old new y)
  Negate x -> Negate (renameExpr old new x)
  FracDiv x y -> FracDiv (renameExpr old new x) (renameExpr old new y)
  Rem x y -> Rem (renameExpr old new x) (renameExpr old new y)
  BitAnd x y -> BitAnd (renameExpr old new x) (renameExpr old new y)
  BitOr x y -> BitOr (renameExpr old new x) (renameExpr old new y)
  BitXor x y -> BitXor (renameExpr old new x) (renameExpr old new y)
  Shl x y -> Shl (renameExpr old new x) (renameExpr old new y)
  Shr x y -> Shr (renameExpr old new x) (renameExpr old new y)
  UShr x y -> UShr (renameExpr old new x) (renameExpr old new y)
  And x y -> And (renameExpr old new x) (renameExpr old new y)
  Or x y -> Or (renameExpr old new x) (renameExpr old new y)
  Eq x y -> Eq (renameExpr old new x) (renameExpr old new y)
  NEq x y -> NEq (renameExpr old new x) (renameExpr old new y)
  GTh x y -> GTh (renameExpr old new x) (renameExpr old new y)
  LTh x y -> LTh (renameExpr old new x) (renameExpr old new y)
  GTEq x y -> GTEq (renameExpr old new x) (renameExpr old new y)
  LTEq x y -> LTEq (renameExpr old new x) (renameExpr old new y)
  Let x g -> Let (renameExpr old new x) (renameExpr old new . g)
  LetRec rhs body -> LetRec (renameExpr old new . rhs) (renameExpr old new . body)
  Lambda g -> Lambda (renameExpr old new . g)
  Apply f x -> Apply (renameExpr old new f) (renameExpr old new x)
  Show x -> Show (renameExpr old new x)
  TypeOf x -> TypeOf (renameExpr old new x)
  If c u v -> If (renameExpr old new c) (renameExpr old new u) (renameExpr old new v)
  OptionCase o n s -> OptionCase (renameExpr old new o) (renameExpr old new n) (renameExpr old new . s)
  ResultOk x -> ResultOk (renameExpr old new x)
  ResultErr x -> ResultErr (renameExpr old new x)
  ResultCase o e s -> ResultCase (renameExpr old new o) (renameExpr old new . e) (renameExpr old new . s)
  UnsafeEffectExpr e -> UnsafeEffectExpr (renameEff old new e)
  ExprUnary n x -> ExprUnary n (renameExpr old new x)
  ExprBinary n x y -> ExprBinary n (renameExpr old new x) (renameExpr old new y)
  ExprTernary n x y z -> ExprTernary n (renameExpr old new x) (renameExpr old new y) (renameExpr old new z)
  ExprMap x f -> ExprMap (renameExpr old new x) (renameExpr old new . f)
  ExprFilter x f -> ExprFilter (renameExpr old new x) (renameExpr old new . f)
  ExprReduce x z f -> ExprReduce (renameExpr old new x) (renameExpr old new z) (\a b -> renameExpr old new (f a b))
  ExprIndex x i -> ExprIndex (renameExpr old new x) (renameExpr old new i)
  MathUnary n x -> MathUnary n (renameExpr old new x)
  MathBinary n x y -> MathBinary n (renameExpr old new x) (renameExpr old new y)
  UnsafeNullable x -> UnsafeNullable (renameExpr old new x)
  FrozenLit fs -> FrozenLit (map (mapFieldLit (renameExpr old new)) fs)
  GetField @k o -> GetField @k (renameExpr old new o)

renameEff :: Int -> Int -> Effect Stamp u -> Effect Stamp u
renameEff old new = \case
  Lift x -> Lift (renameExpr old new x)
  FFI n args -> FFI n (mapRec (renameArg old new) args)
  UnsafeObject o -> UnsafeObject o
  UnsafeObjectGet x s -> UnsafeObjectGet (renameEff old new x) s
  UnsafeObjectAssign x y -> UnsafeObjectAssign (renameEff old new x) (renameEff old new y)
  CallMethod x n args -> CallMethod (renameEff old new x) n (mapRec (renameArg old new) args)
  Bind x f -> Bind (renameEff old new x) (renameEff old new . f)
  BindRec rhs body -> BindRec (renameEff old new . rhs) (renameEff old new . body)
  LambdaE f -> LambdaE (renameEff old new . f)
  ApplyE f x -> ApplyE (renameEff old new f) (renameEff old new x)
  IfE c u v -> IfE (renameEff old new c) (renameEff old new u) (renameEff old new v)
  While c b -> While (renameEff old new c) (renameEff old new b)
  OptionCaseE o n s -> OptionCaseE (renameExpr old new o) (renameEff old new n) (renameEff old new . s)
  ResultCaseE o e s -> ResultCaseE (renameExpr old new o) (renameEff old new . e) (renameEff old new . s)
  Throw x -> Throw (renameExpr old new x)
  Try a k -> Try (renameEff old new a) (renameEff old new . k)
  ObjectLit fs -> ObjectLit (map (mapFieldLit (renameExpr old new)) fs)
  DeleteProp o k -> DeleteProp (renameEff old new o) (renameExpr old new k)
  ArrayLit es -> ArrayLit (map (renameEff old new) es)
  ArraySort xs f -> ArraySort (renameExpr old new xs) (\a b -> renameExpr old new (f a b))

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

reoptExpr2 :: Int -> (Stamp u -> Stamp w -> Expr Stamp v) -> Stamp u -> Stamp w -> Expr Stamp v
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
  let tag = t0
      (t1, body) = optExpr (t0 - 1) (f (Stamp tag))
   in (t1, tag, body)

optUnderE :: Int -> (Stamp u -> Effect Stamp v) -> (Int, Int, Effect Stamp v)
optUnderE t0 f =
  let tag = t0
      (t1, body) = optEffect (t0 - 1) (f (Stamp tag))
   in (t1, tag, body)

optUnder2 :: Int -> (Stamp a -> Stamp b -> Expr Stamp v) -> (Int, Int, Int, Expr Stamp v)
optUnder2 t0 f =
  let tA = t0
      tB = t0 - 1
      (t1, body) = optExpr (t0 - 2) (f (Stamp tA) (Stamp tB))
   in (t1, tA, tB, body)

isCheapValue :: Value u -> Bool
isCheapValue = \case
  ValueNumber{} -> True
  ValueString{} -> True
  ValueBool{} -> True
  ValueUnit -> True
  ValueOption Nothing -> True
  ValueOption (Just v) -> isCheapValue v
  ValueResult (Left v) -> isCheapValue v
  ValueResult (Right v) -> isCheapValue v
  ValueRegex{} -> False
  ValueArray{} -> False
  ValueFunction{} -> False
  ValueFrozen{} -> False

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
  UnsafeObject{} -> False
  _ -> False

class PhoasDummy f where
  phoasDummy :: f u

instance PhoasDummy Stamp where
  phoasDummy = nestedDummy

instance PhoasDummy Value where
  phoasDummy = error "JShark.isPureExpr: Value binder"

isPureExpr :: PhoasDummy f => Expr f u -> Bool
isPureExpr = \case
  Literal{} -> True
  Var{} -> True
  Concat x y -> isPureExpr x && isPureExpr y
  Plus x y -> isPureExpr x && isPureExpr y
  Times x y -> isPureExpr x && isPureExpr y
  Minus x y -> isPureExpr x && isPureExpr y
  Negate x -> isPureExpr x
  FracDiv x y -> isPureExpr x && isPureExpr y
  Rem x y -> isPureExpr x && isPureExpr y
  BitAnd x y -> isPureExpr x && isPureExpr y
  BitOr x y -> isPureExpr x && isPureExpr y
  BitXor x y -> isPureExpr x && isPureExpr y
  Shl x y -> isPureExpr x && isPureExpr y
  Shr x y -> isPureExpr x && isPureExpr y
  UShr x y -> isPureExpr x && isPureExpr y
  And x y -> isPureExpr x && isPureExpr y
  Or x y -> isPureExpr x && isPureExpr y
  Eq x y -> isPureExpr x && isPureExpr y
  NEq x y -> isPureExpr x && isPureExpr y
  GTh x y -> isPureExpr x && isPureExpr y
  LTh x y -> isPureExpr x && isPureExpr y
  GTEq x y -> isPureExpr x && isPureExpr y
  LTEq x y -> isPureExpr x && isPureExpr y
  Let x g -> isPureExpr x && isPureExpr (g phoasDummy)
  LetRec r b -> isPureExpr (r phoasDummy) && isPureExpr (b phoasDummy)
  Lambda g -> isPureExpr (g phoasDummy)
  Apply f x -> isPureExpr f && isPureExpr x
  Show x -> isPureExpr x
  TypeOf x -> isPureExpr x
  If c t e -> isPureExpr c && isPureExpr t && isPureExpr e
  OptionCase o n s -> isPureExpr o && isPureExpr n && isPureExpr (s phoasDummy)
  ResultOk x -> isPureExpr x
  ResultErr x -> isPureExpr x
  ResultCase o e s -> isPureExpr o && isPureExpr (e phoasDummy) && isPureExpr (s phoasDummy)
  UnsafeEffectExpr _ -> False
  ExprUnary n x -> isPureStdUnary n && isPureExpr x
  ExprBinary _ x y -> isPureExpr x && isPureExpr y
  ExprTernary _ x y z -> isPureExpr x && isPureExpr y && isPureExpr z
  ExprMap x f -> isPureExpr x && isPureExpr (f phoasDummy)
  ExprFilter x f -> isPureExpr x && isPureExpr (f phoasDummy)
  ExprReduce x z f -> isPureExpr x && isPureExpr z && isPureExpr (f phoasDummy phoasDummy)
  ExprIndex x i -> isPureExpr x && isPureExpr i
  MathUnary _ x -> isPureExpr x
  MathBinary _ x y -> isPureExpr x && isPureExpr y
  UnsafeNullable x -> isPureExpr x
  FrozenLit fs -> all (\(FieldLit e) -> isPureExpr e) fs
  GetField o -> isPureExpr o

-- | @JSON.stringify@ throws on bigint / circular values, so unused
-- stringify is kept.
isPureStdUnary :: StdUnary a b -> Bool
isPureStdUnary StdStringify = False
isPureStdUnary _ = True

isPureEffect :: Effect Stamp u -> Bool
isPureEffect = \case
  Lift x -> isPureExpr x
  FFI{} -> False
  UnsafeObject{} -> True
  UnsafeObjectGet{} -> False
  UnsafeObjectAssign{} -> False
  CallMethod{} -> False
  Bind x f -> isPureEffect x && isPureEffect (f nestedDummy)
  BindRec r b -> isPureEffect (r nestedDummy) && isPureEffect (b nestedDummy)
  LambdaE f -> isPureEffect (f nestedDummy)
  ApplyE{} -> False
  IfE c t e -> isPureEffect c && isPureEffect t && isPureEffect e
  While{} -> False
  OptionCaseE o n s -> isPureExpr o && isPureEffect n && isPureEffect (s nestedDummy)
  ResultCaseE o e s -> isPureExpr o && isPureEffect (e nestedDummy) && isPureEffect (s nestedDummy)
  Throw{} -> False
  Try{} -> False
  ObjectLit fs -> all (\(FieldLit e) -> isPureExpr e) fs
  DeleteProp{} -> False
  ArrayLit es -> all isPureEffect es
  ArraySort{} -> False

optArgs :: Int -> Rec (Arg Stamp) us -> (Int, Rec (Arg Stamp) us)
optArgs = mapAccumRec optArg

optArg :: Int -> Arg Stamp u -> (Int, Arg Stamp u)
optArg t (ArgExpr e) = fmap ArgExpr (optExpr t e)
optArg t (ArgEffect e) = fmap ArgEffect (optEffect t e)

foldNum1 :: (Double -> Double) -> (Expr Stamp 'Number -> Expr Stamp 'Number) -> Expr Stamp 'Number -> Expr Stamp 'Number
foldNum1 f k = \case
  Literal (ValueNumber a) -> Literal (ValueNumber (f a))
  x -> k x

foldNum2 :: (Double -> Double -> Double) -> (Expr Stamp 'Number -> Expr Stamp 'Number -> Expr Stamp 'Number) -> Expr Stamp 'Number -> Expr Stamp 'Number -> Expr Stamp 'Number
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

foldEq :: Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool
foldEq x y = case (x, y) of
  (Literal a, Literal b)
    | eqFoldableValue a && eqFoldableValue b -> Literal (ValueBool (valueEq a b))
  _ -> Eq x y

foldNEq :: Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool
foldNEq x y = case (x, y) of
  (Literal a, Literal b)
    | eqFoldableValue a && eqFoldableValue b -> Literal (ValueBool (not (valueEq a b)))
  _ -> NEq x y

foldOrd :: Ordering -> (Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool) -> Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool
foldOrd ord k x y = case (x, y) of
  (Literal a, Literal b)
    | isOrderableValue a && isOrderableValue b -> Literal (ValueBool (valueCompare a b == ord))
  _ -> k x y

foldOrdNeq :: Ordering -> (Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool) -> Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool
foldOrdNeq ord k x y = case (x, y) of
  (Literal a, Literal b)
    | isOrderableValue a && isOrderableValue b -> Literal (ValueBool (valueCompare a b /= ord))
  _ -> k x y

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
    | let i = truncate d :: Int
    , i >= 0 && i < length vs -> Literal (vs !! i)
  _ -> ExprIndex arr idx

foldMathUnary :: MathFn1 -> Expr Stamp 'Number -> Expr Stamp 'Number
foldMathUnary n x = case x of
  Literal (ValueNumber a)
    | Just r <- exactMathUnary n a -> Literal (ValueNumber r)
  _ -> MathUnary n x

foldMathBinary :: MathFn2 -> Expr Stamp 'Number -> Expr Stamp 'Number -> Expr Stamp 'Number
foldMathBinary n x y = case (x, y) of
  (Literal (ValueNumber a), Literal (ValueNumber b))
    | Just r <- exactMathBinary n a b -> Literal (ValueNumber r)
  _ -> MathBinary n x y

optLet :: Int -> Expr Stamp u -> (Stamp u -> Expr Stamp v) -> (Int, Expr Stamp v)
optLet t0 x f =
  let (t1, x') = optExpr t0 x
      (t2, tag, body) = optUnder t1 f
   in elimLetFrom t2 x' f tag body

-- Count uses on the already-optimized body. Large tails keep that
-- body (rename-only reopen). Small @f@ may still be applied once more
-- so nested lets / optionCase peel fold.
elimLetFrom :: Int -> Expr Stamp u -> (Stamp u -> Expr Stamp v) -> Int -> Expr Stamp v -> (Int, Expr Stamp v)
elimLetFrom t x f tag body =
  let uses = countExpr tag body
      rebuild = Let x (rebindExpr tag body)
      splice
        | sizeExpr body > optSmall = (t, rebuild)
        | otherwise = optExpr t (inlineExpr f x)
   in case uses of
        -- uses==1 is always a single strict use: countExpr already
        -- treats lambda/loop/?:/&&-|| RHS positions as 2.
        0 | isPureExpr x -> (t, body)
        0 -> (t, rebuild)
        1 -> splice
        _ | isCheap x -> splice
        _ -> (t, rebuild)

boundAsExpr :: Effect Stamp u -> Expr Stamp u
boundAsExpr (Lift e) = e
boundAsExpr e = UnsafeEffectExpr e

optBind :: Int -> Effect Stamp u -> (Stamp u -> Effect Stamp v) -> (Int, Effect Stamp v)
optBind t0 x f =
  let (t1, x') = optEffect t0 x
      (t2, tag, body) = optUnderE t1 f
   in elimBindFrom t2 x' f tag body

elimBindFrom :: Int -> Effect Stamp u -> (Stamp u -> Effect Stamp v) -> Int -> Effect Stamp v -> (Int, Effect Stamp v)
elimBindFrom t x f tag body =
  let uses = countEffect tag body
      rebuild = Bind x (rebindEff tag body)
      splice
        | sizeEffect body > optSmall = (t, rebuild)
        | otherwise = optEffect t (inlineEff f (boundAsExpr x))
   in case uses of
        0 | isPureEffect x -> (t, body)
        0 -> (t, rebuild)
        1 -> splice
        _ | isCheapEffect x -> splice
        _ -> (t, rebuild)

optExpr :: Int -> Expr Stamp u -> (Int, Expr Stamp u)
optExpr t0 = \case
  Literal v -> (t0, Literal v)
  Var (Embed e) -> optExpr t0 (flattenExpr e)
  Var v -> (t0, Var v)
  Concat x y ->
    let (t1, x') = optExpr t0 x
        (t2, y') = optExpr t1 y
     in (t2, foldConcat x' y')
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
    let (t1, x') = optExpr t0 x
     in case x' of
          -- JS && short-circuits: `false && e` does not evaluate `e`.
          Literal (ValueBool False) -> (t1, Literal (ValueBool False))
          Literal (ValueBool True) -> optExpr t1 y
          _ ->
            let (t2, y') = optExpr t1 y
             in (t2, foldAnd x' y')
  Or x y ->
    let (t1, x') = optExpr t0 x
     in case x' of
          Literal (ValueBool True) -> (t1, Literal (ValueBool True))
          Literal (ValueBool False) -> optExpr t1 y
          _ ->
            let (t2, y') = optExpr t1 y
             in (t2, foldOr x' y')
  Eq x y ->
    let (t1, x') = optExpr t0 x
        (t2, y') = optExpr t1 y
     in (t2, foldEq x' y')
  NEq x y ->
    let (t1, x') = optExpr t0 x
        (t2, y') = optExpr t1 y
     in (t2, foldNEq x' y')
  GTh x y ->
    let (t1, x') = optExpr t0 x
        (t2, y') = optExpr t1 y
     in (t2, foldOrd GT GTh x' y')
  LTh x y ->
    let (t1, x') = optExpr t0 x
        (t2, y') = optExpr t1 y
     in (t2, foldOrd LT LTh x' y')
  GTEq x y ->
    let (t1, x') = optExpr t0 x
        (t2, y') = optExpr t1 y
     in (t2, foldOrdNeq LT GTEq x' y')
  LTEq x y ->
    let (t1, x') = optExpr t0 x
        (t2, y') = optExpr t1 y
     in (t2, foldOrdNeq GT LTEq x' y')
  Let x f -> optLet t0 x f
  LetRec r b ->
    let tag = t0
        (t1, r') = optExpr (t0 - 1) (r (Stamp tag))
        (t2, b') = optExpr t1 (b (Stamp tag))
     in (t2, LetRec (keepExprCont t2 tag r' r) (keepExprCont t2 tag b' b))
  Lambda f ->
    let (t1, tag, body) = optUnder t0 f
     in (t1, Lambda (keepExprCont t1 tag body f))
  Apply f x ->
    let (t1, f') = optExpr t0 f
        (t2, x') = optExpr t1 x
     in case f' of
          Lambda g -> optLet t2 x' g
          _ -> (t2, Apply f' x')
  Show x ->
    let (t1, x') = optExpr t0 x
     in (t1, foldShow x')
  TypeOf x ->
    let (t1, x') = optExpr t0 x
     in (t1, foldTypeOf x')
  If c t e ->
    let (t1, c') = optExpr t0 c
     in case c' of
          Literal (ValueBool True) -> optExpr t1 t
          Literal (ValueBool False) -> optExpr t1 e
          _ ->
            let (t2, t') = optExpr t1 t
                (t3, e') = optExpr t2 e
             in (t3, If c' t' e')
  OptionCase o n s ->
    let (t1, o') = optExpr t0 o
     in case peelOption o' of
          Just Nothing -> optExpr t1 n
          Just (Just x) ->
            let (t2, tag, body) = optUnder t1 s
             in elimLetFrom t2 x s tag body
          Nothing ->
            let (t2, n') = optExpr t1 n
                (t3, tag, body) = optUnder t2 s
             in (t3, OptionCase o' n' (keepExprCont t3 tag body s))
  ResultOk x -> fmap ResultOk (optExpr t0 x)
  ResultErr x -> fmap ResultErr (optExpr t0 x)
  ResultCase o e s ->
    let (t1, o') = optExpr t0 o
     in case peelResult o' of
          Just (Left x) ->
            let (t2, tag, body) = optUnder t1 e
             in elimLetFrom t2 x e tag body
          Just (Right x) ->
            let (t2, tag, body) = optUnder t1 s
             in elimLetFrom t2 x s tag body
          Nothing ->
            let (t2, tE, e') = optUnder t1 e
                (t3, tS, s') = optUnder t2 s
             in (t3, ResultCase o' (keepExprCont t3 tE e' e) (keepExprCont t3 tS s' s))
  UnsafeEffectExpr e ->
    let (t1, e') = optEffect t0 e
     in case e' of
          Lift x -> (t1, x)
          _ -> (t1, UnsafeEffectExpr e')
  ExprUnary n x ->
    let (t1, x') = optExpr t0 x
     in (t1, ExprUnary n x')
  ExprBinary n x y ->
    let (t1, x') = optExpr t0 x
        (t2, y') = optExpr t1 y
     in (t2, ExprBinary n x' y')
  ExprTernary n x y z ->
    let (t1, x') = optExpr t0 x
        (t2, y') = optExpr t1 y
        (t3, z') = optExpr t2 z
     in (t3, ExprTernary n x' y' z')
  ExprMap x f ->
    let (t1, x') = optExpr t0 x
        (t2, tag, body) = optUnder t1 f
     in (t2, ExprMap x' (keepExprCont t2 tag body f))
  ExprFilter x f ->
    let (t1, x') = optExpr t0 x
        (t2, tag, body) = optUnder t1 f
     in (t2, ExprFilter x' (keepExprCont t2 tag body f))
  ExprReduce x z f ->
    let (t1, x') = optExpr t0 x
        (t2, z') = optExpr t1 z
        (t3, tA, tB, body) = optUnder2 t2 f
        wrap a b
          | sizeExpr body <= optSmall = reoptExpr2 t3 f a b
          | otherwise = rebindExpr2 tA tB body a b
     in (t3, ExprReduce x' z' wrap)
  ExprIndex arr idx ->
    let (t1, arr') = optExpr t0 arr
        (t2, idx') = optExpr t1 idx
     in (t2, foldIndex arr' idx')
  MathUnary n x ->
    let (t1, x') = optExpr t0 x
     in (t1, foldMathUnary n x')
  MathBinary n x y ->
    let (t1, x') = optExpr t0 x
        (t2, y') = optExpr t1 y
     in (t2, foldMathBinary n x' y')
  UnsafeNullable x -> fmap UnsafeNullable (optExpr t0 x)
  FrozenLit fs ->
    let (t1, fs') = mapAccumField t0 fs
     in (t1, FrozenLit fs')
  GetField @k o ->
    let (t1, o') = optExpr t0 o
     in case foldGetField @k o' of
          Just e -> optExpr t1 e
          Nothing -> (t1, GetField @k o')
  where
    binNum f k x y =
      let (t1, x') = optExpr t0 x
          (t2, y') = optExpr t1 y
       in (t2, foldNum2 f k x' y')
    unNum f k x =
      let (t1, x') = optExpr t0 x
       in (t1, foldNum1 f k x')

optEffect :: Int -> Effect Stamp u -> (Int, Effect Stamp u)
optEffect t0 = \case
  Lift x ->
    let (t1, x') = optExpr t0 x
     in case x' of
          UnsafeEffectExpr e -> (t1, e)
          _ -> (t1, Lift x')
  FFI n args -> fmap (FFI n) (optArgs t0 args)
  UnsafeObject o -> (t0, UnsafeObject o)
  UnsafeObjectGet x s ->
    let (t1, x') = optEffect t0 x
     in (t1, UnsafeObjectGet x' s)
  UnsafeObjectAssign x y ->
    let (t1, x') = optEffect t0 x
        (t2, y') = optEffect t1 y
     in (t2, UnsafeObjectAssign x' y')
  CallMethod x n args ->
    let (t1, x') = optEffect t0 x
        (t2, args') = optArgs t1 args
     in (t2, CallMethod x' n args')
  Bind x f -> optBind t0 x f
  BindRec r b ->
    let tag = t0
        (t1, r') = optEffect (t0 - 1) (r (Stamp tag))
        (t2, b') = optEffect t1 (b (Stamp tag))
     in (t2, BindRec (keepEffCont t2 tag r' r) (keepEffCont t2 tag b' b))
  LambdaE f ->
    let (t1, tag, body) = optUnderE t0 f
     in (t1, LambdaE (keepEffCont t1 tag body f))
  ApplyE f x ->
    let (t1, f') = optEffect t0 f
        (t2, x') = optEffect t1 x
     in case f' of
          LambdaE g -> optBind t2 x' g
          _ -> (t2, ApplyE f' x')
  IfE c t e ->
    let (t1, c') = optEffect t0 c
     in case peelBoolEffect c' of
          Just True -> optEffect t1 t
          Just False -> optEffect t1 e
          Nothing ->
            let (t2, t') = optEffect t1 t
                (t3, e') = optEffect t2 e
             in (t3, IfE c' t' e')
  While c b ->
    let (t1, c') = optEffect t0 c
     in case peelBoolEffect c' of
          Just False -> (t1, Lift (Literal ValueUnit))
          _ ->
            let (t2, b') = optEffect t1 b
             in (t2, While c' b')
  OptionCaseE o n s ->
    let (t1, o') = optExpr t0 o
     in case peelOption o' of
          Just Nothing -> optEffect t1 n
          Just (Just x) ->
            let (t2, tag, body) = optUnderE t1 s
             in elimBindFrom t2 (Lift x) s tag body
          Nothing ->
            let (t2, n') = optEffect t1 n
                (t3, tag, body) = optUnderE t2 s
             in (t3, OptionCaseE o' n' (keepEffCont t3 tag body s))
  ResultCaseE o e s ->
    let (t1, o') = optExpr t0 o
     in case peelResult o' of
          Just (Left x) ->
            let (t2, tag, body) = optUnderE t1 e
             in elimBindFrom t2 (Lift x) e tag body
          Just (Right x) ->
            let (t2, tag, body) = optUnderE t1 s
             in elimBindFrom t2 (Lift x) s tag body
          Nothing ->
            let (t2, tE, e') = optUnderE t1 e
                (t3, tS, s') = optUnderE t2 s
             in (t3, ResultCaseE o' (keepEffCont t3 tE e' e) (keepEffCont t3 tS s' s))
  Throw x ->
    let (t1, x') = optExpr t0 x
     in (t1, Throw x')
  Try a k ->
    let (t1, a') = optEffect t0 a
        (t2, tag, body) = optUnderE t1 k
     in (t2, Try a' (keepEffCont t2 tag body k))
  ObjectLit fs ->
    let (t1, fs') = mapAccumField t0 fs
     in (t1, ObjectLit fs')
  DeleteProp o k ->
    let (t1, o') = optEffect t0 o
        (t2, k') = optExpr t1 k
     in (t2, DeleteProp o' k')
  ArrayLit es ->
    let (t1, es') = mapAccumEffs t0 es
     in (t1, ArrayLit es')
  ArraySort xs f ->
    let (t1, xs') = optExpr t0 xs
        (t2, tA, tB, body) = optUnder2 t1 f
        wrap a b
          | sizeExpr body <= optSmall = reoptExpr2 t2 f a b
          | otherwise = rebindExpr2 tA tB body a b
     in (t2, ArraySort xs' wrap)

mapAccumField :: Int -> [FieldLit Stamp r] -> (Int, [FieldLit Stamp r])
mapAccumField t [] = (t, [])
mapAccumField t (FieldLit @k e : fs) =
  let (t1, e') = optExpr t e
      (t2, fs') = mapAccumField t1 fs
   in (t2, FieldLit @k e' : fs')

mapAccumEffs :: Int -> [Effect Stamp u] -> (Int, [Effect Stamp u])
mapAccumEffs t [] = (t, [])
mapAccumEffs t (e:es) =
  let (t1, e') = optEffect t e
      (t2, es') = mapAccumEffs t1 es
   in (t2, e' : es')

-- Bind of an Effect: when the continuation uses the binder once in a
-- strict position, splice the effect in place (so `x <- getEl; x.foo()`
-- becomes `getEl().foo()`); when never, keep it as a statement.
bindEffectCode :: CG -> Effect Stamp u -> (Stamp u -> Effect Stamp v) -> (CG, Code)
bindEffectCode s0 x f =
  let (tag, sTag) = allocTag s0
      tagged = f (Stamp tag)
      uses = countEffect tag tagged
   in case uses of
        0 ->
          let (s1, MkCode xDecl xRef xFX) = effectfulAST' sTag x
              (s2, MkCode yDecl yRef yFX) = effectfulAST' s1 (f nestedDummy)
              -- Value-producing effects (ifE) put work in xDecl and leave
              -- a result ident in xRef (codeRefFX False). Assignments and
              -- calls keep the side effect in xRef (fxCode).
              stmt
                | P.isEmpty xRef = xDecl
                | not xFX && not (P.isEmpty xDecl) = xDecl
                | otherwise = asStmt xDecl xRef
           in (s2, MkCode (stmt $$ yDecl) yRef yFX)
        _ ->
          let (s1, MkCode xDecl xRef _) = effectfulAST' sTag x
           in if P.isEmpty xRef
                then
                  let (s2, MkCode yDecl yRef yFX) = effectfulAST' s1 (f (Name (cgIdent s1 - 1)))
                   in (s2, MkCode (xDecl $$ yDecl) yRef yFX)
                else
                  let (nBind, s2) = allocIdent s1
                      (s3, MkCode yDecl yRef yFX) = effectfulAST' s2 (f (Name nBind))
                   in (s3, MkCode (xDecl $$ constBind nBind xRef $$ yDecl) yRef yFX)

effectfulAST :: ClosedEffect u -> Doc
effectfulAST e = renderCode . snd . effectfulAST' startCG $ optimizeEffect e

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
  While{} -> True
  Bind _ f -> isUnitWitness (f nestedDummy)
  BindRec _ f -> isUnitWitness (f nestedDummy)
  IfE _ t e -> isUnitWitness t && isUnitWitness e
  OptionCaseE _ n s -> isUnitWitness n && isUnitWitness (s nestedDummy)
  ResultCaseE _ e s -> isUnitWitness (e nestedDummy) && isUnitWitness (s nestedDummy)
  Throw{} -> True
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

effectfulAST' :: forall v. CG -> Effect Stamp v -> (CG, Code)
effectfulAST' !s0 = \case
  Lift x -> pureAST' s0 x
  FFI fn args ->
    let (s1, argDecl, argRefs) = renderArgList argAST s0 args
     in (s1, fxCode argDecl (P.text fn <> P.parens argRefs))
  IfE c t e
    | isUnitWitness t && isUnitWitness e ->
        let (s1, Code cDecl cRef) = effectfulAST' s0 c
            (s2, Code tDecl tRef) = effectfulAST' s1 t
            (s3, Code eDecl eRef) = effectfulAST' s2 e
         in (s3, Code (cDecl $$ ifElseStmt cRef tDecl tRef eDecl eRef) mempty)
    | otherwise ->
        -- Value-producing @if@: a shared result var is assigned in both
        -- arms. Do not use emptiness to pick a ternary — a Unit leftover
        -- ref is not a genuinely-empty Doc.
        let (s1, Code cDecl cRef) = effectfulAST' s0 c
            (resultN, s2) = allocIdent s1
            resultVar = 'n' : show resultN
            (s3, Code tDecl tRef) = effectfulAST' s2 t
            (s4, Code eDecl eRef) = effectfulAST' s3 e
            assign ref = if P.isEmpty ref then mempty else (P.text resultVar <+> "=" <+> ref) <> P.semi
            ifStmt = ("let" <+> P.text resultVar) <> P.semi
              $$ ("if" <+> P.parens cRef <+> bracesNest (tDecl $$ assign tRef))
              $$ ("else" <+> bracesNest (eDecl $$ assign eRef))
         in (s4, Code (cDecl $$ ifStmt) (P.text resultVar))
  While cond body ->
    let (s1, Code condDecl condRef) = effectfulAST' s0 cond
        (s2, Code bodyDecl bodyRef) = effectfulAST' s1 body
        bodyStmt = if P.isEmpty bodyRef then bodyDecl else bodyDecl $$ (bodyRef <> P.semi)
        whileStmt = "while" <+> P.parens condRef <+> P.braces (P.nest 2 bodyStmt)
     in (s2, Code (condDecl $$ whileStmt) mempty)
  OptionCaseE opt noneE someF
    | isUnitWitness noneE && isUnitWitness (someF nestedDummy) ->
        let (s1, Code oDecl oRef) = pureAST' s0 opt
            (nBind, s2) = allocIdent s1
            optVar = 'n' : show nBind
            (s3, Code nDecl nRef) = effectfulAST' s2 noneE
            (s4, Code sDecl sRef) = effectfulAST' s3 (someF (Name nBind))
            stmt = oDecl $$ constBind nBind oRef
              $$ ifElseStmt (P.text optVar <+> "===" <+> "null") nDecl nRef sDecl sRef
         in (s4, Code stmt mempty)
    | otherwise ->
        let (s1, Code oDecl oRef) = pureAST' s0 opt
            (nBind, s2) = allocIdent s1
            optVar = 'n' : show nBind
            (resultN, s3) = allocIdent s2
            resultVar = 'n' : show resultN
            (s4, Code nDecl nRef) = effectfulAST' s3 noneE
            (s5, Code sDecl sRef) = effectfulAST' s4 (someF (Name nBind))
            assign ref = if P.isEmpty ref then mempty else (P.text resultVar <+> "=" <+> ref) <> P.semi
            stmt = oDecl $$ constBind nBind oRef
              $$ ("let" <+> P.text resultVar) <> P.semi
              $$ ("if" <+> P.parens (P.text optVar <+> "===" <+> "null")
                    <+> P.braces (P.nest 2 (nDecl $$ assign nRef)))
              $$ ("else" <+> P.braces (P.nest 2 (sDecl $$ assign sRef)))
         in (s5, Code stmt (P.text resultVar))
  Try a k
    | isUnitWitness a && isUnitWitness (k nestedDummy) ->
        let (s1, Code aDecl aRef) = effectfulAST' s0 a
            (catchN, s2) = allocIdent s1
            (s3, Code bDecl bRef) = effectfulAST' s2 (k (Name catchN))
            stmt = "try" <+> bracesNest (asStmt aDecl aRef)
              $$ ("catch" <+> P.parens (P.text ('n' : show catchN))
                    <+> bracesNest (asStmt bDecl bRef))
         in (s3, Code stmt mempty)
    | otherwise ->
        let (resultN, s1) = allocIdent s0
            resultVar = 'n' : show resultN
            (s2, Code aDecl aRef) = effectfulAST' s1 a
            (catchN, s3) = allocIdent s2
            (s4, Code bDecl bRef) = effectfulAST' s3 (k (Name catchN))
            assign ref = if P.isEmpty ref then mempty else (P.text resultVar <+> "=" <+> ref) <> P.semi
            stmt = ("let" <+> P.text resultVar) <> P.semi
              $$ ("try" <+> P.braces (P.nest 2 (aDecl $$ assign aRef)))
              $$ ("catch" <+> P.parens (P.text ('n' : show catchN))
                    <+> P.braces (P.nest 2 (bDecl $$ assign bRef)))
         in (s4, Code stmt (P.text resultVar))
  Bind x f -> bindEffectCode s0 x f
  BindRec r b ->
    let (nBind, s1) = allocIdent s0
        n = P.text ('n' : show nBind)
        (s2, MkCode rDecl rRef _) = effectfulAST' s1 (r (Name nBind))
        (s3, MkCode bDecl bRef bFX) = effectfulAST' s2 (b (Name nBind))
        stmt = ("let" <+> n) <> P.semi $$ rDecl $$ (n <+> "=" <+> rRef) <> P.semi
     in (s3, MkCode (stmt $$ bDecl) bRef bFX)
  Throw x ->
    let (s1, Code xDecl xRef) = pureAST' s0 x
     in (s1, Code (xDecl $$ (("throw" <+> xRef) <> P.semi)) mempty)
  ObjectLit fs -> renderObjectLit s0 fs
  ArrayLit es -> renderArrayLit s0 es
  DeleteProp o k ->
    let (s1, Code oDecl oRef) = effectfulAST' s0 o
        (s2, Code kDecl kRef) = pureAST' s1 k
     in (s2, fxCode (oDecl $$ kDecl) (("delete" <+> oRef) <> P.brackets kRef))
  ArraySort xs f ->
    let (s1, Code xDecl xRef) = pureAST' s0 xs
        (nA, s2) = allocIdent s1
        (nB, s3) = allocIdent s2
        (s4, Code bDecl bRef) = pureAST' s3 (f (Name nA) (Name nB))
        acc = 'n' : show nA
        x = 'n' : show nB
        cb = "function" <+> P.parens ((P.text acc <> ",") <+> P.text x)
          <+> P.braces (bDecl $$ "return" <+> bRef)
        call = xRef <> ".sort" <> P.parens cb
     in (s4, fxCode xDecl call)
  ResultCaseE res errF okF -> renderResultCaseE s0 res errF okF
  UnsafeObject obj -> (s0, Code mempty $ P.text $ T.unpack obj)
  UnsafeObjectGet x string ->
    let (s1, Code x1Decl x1Ref) = effectfulAST' s0 x
    in (s1, Code x1Decl $ jsDotOrBracket x1Ref string)
  UnsafeObjectAssign x y ->
    let (s1, Code x1Decl x1Ref) = effectfulAST' s0 x
        (s2, Code y1Decl y1Ref) = effectfulAST' s1 y
    in (s2, fxCode (x1Decl $$ y1Decl) $ x1Ref <> " = " <> y1Ref )
  CallMethod recv name args ->
    let (s1, Code rDecl rRef) = effectfulAST' s0 recv
        (s2, argDecl, argRefs) = renderArgList argAST s1 args
     in (s2, fxCode (rDecl $$ argDecl) (rRef <> "." <> P.text name <> P.parens argRefs))
  LambdaE f ->
    let (nParam, s1) = allocIdent s0
        (s2, Code exprXDecl exprXRef) = effectfulAST' s1 (f (Name nParam))
     in ( s2
        , Code mempty
            $ "function"
            <+> P.parens (P.text $ 'n':show nParam)
            <+> P.braces ( (exprXDecl $$ "return" <+> exprXRef) )
        )
  ApplyE fex ex ->
    let (s1, Code exprXDecl exprXRef) = effectfulAST' s0 fex
        (s2, Code exprYDecl exprYRef) = effectfulAST' s1 ex
     in (s2, fxCode (exprXDecl $$ exprYDecl) (jsCall exprXRef exprYRef))

letCode :: CG -> Expr Stamp u -> (Stamp u -> Expr Stamp v) -> (CG, Code)
letCode s0 x g =
  let (tag, sTag) = allocTag s0
      tagged = g (Stamp tag)
      uses = countExpr tag tagged
   in case uses of
        0 ->
          let (s1, MkCode xDecl xRef _) = pureAST' sTag x
              (s2, y) = pureAST' s1 (g nestedDummy)
              stmt
                | P.isEmpty xDecl && not (P.isEmpty xRef) = xRef <> P.semi
                | otherwise = xDecl
           in (s2, keepRef (stmt $$ codeDecl y) y)
        _ ->
          let (s1, MkCode xDecl xRef _) = pureAST' sTag x
              (nBind, s2) = allocIdent s1
              (s3, y) = pureAST' s2 (g (Name nBind))
           in (s3, keepRef (xDecl $$ constBind nBind xRef $$ codeDecl y) y)

pureAST :: ClosedExpr u -> Doc
pureAST e = renderCode . snd . pureAST' startCG $ optimize e

pureAST' :: forall v. CG -> Expr Stamp v
   -> (CG, Code)
pureAST' !s0 = \case
  Literal v -> case v of
    ValueNumber d -> (s0, Code mempty (P.text $ showFFloat Nothing d ""))
    ValueArray xs ->
      let foo :: CG -> [Value u] -> (CG, [Code])
          foo s'0 (x:xs') =
            let (s'1, x') = pureAST' s'0 (Literal x)
                (s'2, cs) = foo s'1 xs'
             in (s'2, x' : cs)
          foo s' [] = (s', [])
          (s1, exprs) = foo s0 xs
          (exprDecls, exprRefs) = partitionCode exprs
       in (s1, Code (P.vcat exprDecls) $ P.brackets (P.hcat $ P.punctuate ", " exprRefs))
    ValueString s -> (s0, Code mempty (jsQuote s))
    ValueFunction _f -> undefined
    ValueUnit -> (s0, mempty)
    ValueOption (Just x) -> pureAST' s0 (Literal x)
    ValueOption Nothing -> (s0, Code mempty "null")
    ValueResult (Right x) -> renderResultLit True s0 x
    ValueResult (Left x) -> renderResultLit False s0 x
    ValueRegex s ->
      (s0, Code mempty ("new RegExp" <> P.parens (jsQuote s)))
    ValueBool True -> (s0, Code mempty "true")
    ValueBool False -> (s0, Code mempty "false")
    ValueFrozen{} -> error "JShark.pureAST: ValueFrozen is eval-only"
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
    let (s1, Code x1Decl x1Ref) = pureAST' s0 x
     in (s1, Code x1Decl $ "String" <> P.parens x1Ref)
  TypeOf x ->
    let (s1, Code x1Decl x1Ref) = pureAST' s0 x
        wrapped = case x of
          FrozenLit{} -> P.parens x1Ref
          _ -> x1Ref
     in (s1, Code x1Decl $ "typeof" <+> wrapped)
  Negate x ->
    let (s1, Code x1Decl x1Ref) = pureAST' s0 x
     in (s1, Code x1Decl $ "-" <> P.parens x1Ref)
  Lambda f ->
    let ident0 = cgIdent s0
        ex = f (Name ident0)
        (s1, Code exprXDecl exprXRef) = pureAST' s0 ex
     in ( s1
        , Code exprXDecl
            $ "function"
            <+> P.parens (P.text $ 'n':show ident0)
            <+> P.braces ("return" <+> (P.parens exprXRef))
        )
  And x y -> renderBin "&&" s0 x y
  Or x y -> renderBin "||" s0 x y
  Eq x y -> renderBin "===" s0 x y
  NEq x y -> renderBin "!==" s0 x y
  GTh x y -> renderBin ">" s0 x y
  LTh x y -> renderBin "<" s0 x y
  GTEq x y -> renderBin ">=" s0 x y
  LTEq x y -> renderBin "<=" s0 x y
  -- Inline a let used once in a strict position; drop one never used;
  -- `const` when shared or used under a lambda/loop/short-circuit.
  Let x g -> letCode s0 x g
  LetRec r b ->
    let (nBind, s1) = allocIdent s0
        n = P.text ('n' : show nBind)
        (s2, MkCode rDecl rRef _) = pureAST' s1 (r (Name nBind))
        (s3, bCode) = pureAST' s2 (b (Name nBind))
        stmt = ("let" <+> n) <> P.semi $$ rDecl $$ (n <+> "=" <+> rRef) <> P.semi
     in (s3, keepRef (stmt $$ codeDecl bCode) bCode)
  Apply fex ex ->
    let (s1, Code exprXDecl exprXRef) = pureAST' s0 fex
        (s2, Code exprYDecl exprYRef) = pureAST' s1 ex
     in (s2, Code (exprXDecl $$ exprYDecl) (jsCall exprXRef exprYRef))
  Var (Embed e) -> pureAST' s0 (flattenExpr e)
  Var s
    -- Tags and the unused-binder dummy are negative; never emit them as JS.
    | stampId s < 0 -> (s0, Code mempty mempty)
    | otherwise -> (s0, Code mempty $ P.text ('n':show (stampId s)))
  If c t e ->
    let (s1, Code cDecl cRef) = pureAST' s0 c
        (s2, Code tDecl tRef) = pureAST' s1 t
        (s3, Code eDecl eRef) = pureAST' s2 e
     in (s3, Code (cDecl $$ tDecl $$ eDecl) (P.parens (cRef <+> "?" <+> tRef <+> ":" <+> eRef)))
  OptionCase opt none' someF ->
    case opt of
      Var (Embed e) -> pureAST' s0 (OptionCase (flattenExpr e) none' someF)
      Var s ->
        let i = stampId s
            optVar = 'n' : show i
            (s2, Code noneDecl noneRef) = pureAST' s0 none'
            (s3, Code someDecl someRef) = pureAST' s2 (someF (Name i))
         in ( s3
            , Code (noneDecl $$ someDecl)
                (P.parens (P.text optVar <+> "===" <+> "null" <+> "?" <+> noneRef <+> ":" <+> someRef))
            )
      _ ->
        let (s1, Code optDecl optRef) = pureAST' s0 opt
            (nBind, s2) = allocIdent s1
            optVar = 'n' : show nBind
            (s3, Code noneDecl noneRef) = pureAST' s2 none'
            (s4, Code someDecl someRef) = pureAST' s3 (someF (Name nBind))
         in ( s4
            , Code (optDecl $$ constBind nBind optRef $$ noneDecl $$ someDecl)
                (P.parens (P.text optVar <+> "===" <+> "null" <+> "?" <+> noneRef <+> ":" <+> someRef))
            )
  ResultOk x ->
    let (s1, Code d r) = pureAST' s0 x
     in (s1, Code d (resultObject True r))
  ResultErr x ->
    let (s1, Code d r) = pureAST' s0 x
     in (s1, Code d (resultObject False r))
  ResultCase res errF okF -> renderResultCase s0 res errF okF
  UnsafeEffectExpr eff -> effectfulAST' s0 eff
  ExprUnary n recv ->
    let (s1, Code rDecl rRef) = pureAST' s0 recv
     in (s1, Code rDecl (stdUnaryJS n rRef))
  ExprBinary n recv arg ->
    let (s1, Code rDecl rRef) = pureAST' s0 recv
        (s2, Code aDecl aRef) = pureAST' s1 arg
     in (s2, Code (rDecl $$ aDecl) (stdBinaryJS n rRef aRef))
  ExprTernary n recv a b ->
    let (s1, Code rDecl rRef) = pureAST' s0 recv
        (s2, Code aDecl aRef) = pureAST' s1 a
        (s3, Code bDecl bRef) = pureAST' s2 b
     in (s3, Code (rDecl $$ aDecl $$ bDecl) (stdTernaryJS n rRef aRef bRef))
  ExprMap recv f -> renderCallbackMethod "map" s0 recv f
  ExprFilter recv f -> renderCallbackMethod "filter" s0 recv f
  ExprReduce recv z f ->
    let (s1, Code rDecl rRef) = pureAST' s0 recv
        (s2, Code zDecl zRef) = pureAST' s1 z
        (nAcc, s3) = allocIdent s2
        (nX, s4) = allocIdent s3
        (s5, Code bDecl bRef) = pureAST' s4 (f (Name nAcc) (Name nX))
        acc = 'n' : show nAcc
        x = 'n' : show nX
        cb = "function" <+> P.parens ((P.text acc <> ",") <+> P.text x)
          <+> P.braces (bDecl $$ "return" <+> bRef)
        call = rRef <> ".reduce" <> P.parens (cb <> ", " <> zRef)
     in (s5, Code (rDecl $$ zDecl) call)
  ExprIndex arr idx ->
    let (s1, Code aDecl aRef) = pureAST' s0 arr
        (s2, Code iDecl iRef) = pureAST' s1 idx
     in (s2, Code (aDecl $$ iDecl) (aRef <> P.brackets iRef))
  MathUnary name x ->
    let (s1, Code xDecl xRef) = pureAST' s0 x
     in (s1, Code xDecl ("Math." <> P.text (T.unpack (mathFn1Name name)) <> P.parens xRef))
  MathBinary name x y ->
    let (s1, Code xDecl xRef) = pureAST' s0 x
        (s2, Code yDecl yRef) = pureAST' s1 y
     in (s2, Code (xDecl $$ yDecl) ("Math." <> P.text (T.unpack (mathFn2Name name)) <> P.parens (xRef <> ", " <> yRef)))
  UnsafeNullable x -> pureAST' s0 x
  FrozenLit fs -> renderObjectLit s0 fs
  GetField @k o ->
    let (s1, Code d r) = pureAST' s0 o
     in (s1, Code d (jsDotOrBracket r (symbolVal (Proxy @k))))

stdUnaryJS :: StdUnary a b -> Doc -> Doc
stdUnaryJS n r = case n of
  StdToUpper -> r <> ".toUpperCase()"
  StdToLower -> r <> ".toLowerCase()"
  StdTrim -> r <> ".trim()"
  StdArrLen -> r <> ".length"
  StdStrLen -> r <> ".length"
  StdStringify -> "JSON.stringify" <> P.parens r

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
  StdSlice -> r <> ".slice" <> P.parens (a <> ", " <> b)
  StdArrSlice -> r <> ".slice" <> P.parens (a <> ", " <> b)
  StdReplace -> r <> ".replace" <> P.parens (a <> ", " <> b)

resultPayloadRef :: Doc -> Doc
resultPayloadRef r
  | P.isEmpty r = "undefined"
  | otherwise = r

resultObject :: Bool -> Doc -> Doc
resultObject isOk payload =
  let flag = if isOk then "true" else "false"
   in P.braces ((("ok:" <+> flag) <> ",") <+> ("value:" <+> resultPayloadRef payload))

renderResultLit :: Bool -> CG -> Value u -> (CG, Code)
renderResultLit isOk s0 x =
  let (s1, Code d r) = pureAST' s0 (Literal x)
   in (s1, Code d (resultObject isOk r))

renderArrayLit :: CG -> [Effect Stamp u] -> (CG, Code)
renderArrayLit s0 es =
  let (s1, cs) = mapAccumAST effectfulAST' s0 es
      (decls, refs) = partitionCode cs
   in (s1, Code (P.vcat decls) (P.brackets (P.hcat (P.punctuate ", " refs))))

mapAccumAST :: (CG -> a -> (CG, Code)) -> CG -> [a] -> (CG, [Code])
mapAccumAST _ s [] = (s, [])
mapAccumAST f s (x:xs) =
  let (s1, c) = f s x
      (s2, cs) = mapAccumAST f s1 xs
   in (s2, c : cs)

renderObjectLit :: CG -> [FieldLit Stamp r] -> (CG, Code)
renderObjectLit s0 fs =
  let step (s, decl, acc) fl@(FieldLit e) =
        let (s', Code d r) = pureAST' s e
         in (s', decl $$ d, ((P.doubleQuotes (P.text (fieldKey fl)) <> ":") <+> r) : acc)
      (s1, decls, pairs) = foldl step (s0, mempty, []) fs
   in (s1, Code decls (P.braces (P.hcat (P.punctuate ", " (reverse pairs)))))

renderResultCase ::
     CG
  -> Expr Stamp ('Result e a)
  -> (Stamp e -> Expr Stamp v)
  -> (Stamp a -> Expr Stamp v)
  -> (CG, Code)
renderResultCase s0 res errF okF =
  let (s1, Code rDecl rRef) = pureAST' s0 res
      (nObj, s2) = allocIdent s1
      (nUnw, s3) = allocIdent s2
      obj = 'n' : show nObj
      (s4, Code eDecl eRef) = pureAST' s3 (errF (Name nUnw))
      (s5, Code oDecl oRef) = pureAST' s4 (okF (Name nUnw))
      pick = P.text obj <> ".ok"
      unwrap = P.text obj <> ".value"
      stmt = rDecl $$ constBind nObj rRef
        $$ constBind nUnw unwrap
        $$ eDecl $$ oDecl
   in (s5, Code stmt (P.parens (pick <+> "?" <+> oRef <+> ":" <+> eRef)))

renderResultCaseE ::
     CG
  -> Expr Stamp ('Result e a)
  -> (Stamp e -> Effect Stamp v)
  -> (Stamp a -> Effect Stamp v)
  -> (CG, Code)
renderResultCaseE s0 res errF okF
  | isUnitWitness (errF nestedDummy) && isUnitWitness (okF nestedDummy) =
      let (s1, Code rDecl rRef) = pureAST' s0 res
          (nObj, s2) = allocIdent s1
          (nUnw, s3) = allocIdent s2
          obj = 'n' : show nObj
          (s4, Code eDecl eRef) = effectfulAST' s3 (errF (Name nUnw))
          (s5, Code oDecl oRef) = effectfulAST' s4 (okF (Name nUnw))
          unwrap = P.text obj <> ".value"
          stmt = rDecl $$ constBind nObj rRef $$ constBind nUnw unwrap
            $$ ifElseStmt (P.text obj <> ".ok") oDecl oRef eDecl eRef
       in (s5, Code stmt mempty)
  | otherwise =
      let (s1, Code rDecl rRef) = pureAST' s0 res
          (nObj, s2) = allocIdent s1
          (nUnw, s3) = allocIdent s2
          (resultN, s4) = allocIdent s3
          obj = 'n' : show nObj
          resultVar = 'n' : show resultN
          (s5, Code eDecl eRef) = effectfulAST' s4 (errF (Name nUnw))
          (s6, Code oDecl oRef) = effectfulAST' s5 (okF (Name nUnw))
          assign ref = if P.isEmpty ref then mempty else (P.text resultVar <+> "=" <+> ref) <> P.semi
          unwrap = P.text obj <> ".value"
          stmt = rDecl $$ constBind nObj rRef $$ constBind nUnw unwrap
            $$ ("let" <+> P.text resultVar) <> P.semi
            $$ ifElseStmt (P.text obj <> ".ok")
                 (oDecl $$ assign oRef) mempty
                 (eDecl $$ assign eRef) mempty
       in (s6, Code stmt (P.text resultVar))

renderCallbackMethod ::
     String
  -> CG
  -> Expr Stamp a
  -> (Stamp b -> Expr Stamp c)
  -> (CG, Code)
renderCallbackMethod name s0 recv f =
  let (s1, Code rDecl rRef) = pureAST' s0 recv
      (nParam, s2) = allocIdent s1
      ex = f (Name nParam)
      (s3, Code exDecl exRef) = pureAST' s2 ex
      paramName = 'n' : show nParam
      callback = "function" <+> P.parens (P.text paramName) <+> P.braces (exDecl $$ "return" <+> exRef)
      call = rRef <> "." <> P.text name <> P.parens callback
   in (s3, Code rDecl call)

renderBin :: String -> CG -> Expr Stamp a -> Expr Stamp b -> (CG, Code)
renderBin op s0 x y =
  let (s1, Code xDecl xRef) = pureAST' s0 x
      (s2, Code yDecl yRef) = pureAST' s1 y
   in ( s2
      , Code (xDecl $$ yDecl)
          (wrapOperand x xRef <+> P.text op <+> wrapOperand y yRef)
      )

argAST :: CG -> Arg Stamp u -> (CG, Code)
argAST s (ArgExpr e) = pureAST' s e
argAST s (ArgEffect e) = effectfulAST' s e

renderArgList :: (forall x. CG -> f x -> (CG, Code)) -> CG -> Rec f us -> (CG, Doc, Doc)
renderArgList f s0 args =
  let (s1, cs) = recCodes f s0 args
      (decls, refs) = partitionCode cs
   in (s1, P.vcat decls, P.hcat (P.punctuate ", " refs))

