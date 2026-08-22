{-# LANGUAGE
    BangPatterns
  , ConstraintKinds
  , DataKinds
  , GADTs
  , LambdaCase
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TupleSections
  , TypeOperators
#-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module JShark
  ( Expr
    ( Literal, Concat, Plus, Times, Minus, Negate, FracDiv
    , And, Or, Eq, NEq, GTh, LTh, GTEq, LTEq
    , Let, Lambda, Apply, Show, TypeOf, Var, If, OptionCase
    , ExprIndex, MathUnary, MathBinary
    , ExprUnary, ExprBinary, ExprTernary, ExprMap, ExprFilter
    , UnsafeNullable
    )
  , Value(..)
  , Arg(..)
  , ClosedExpr
  , ClosedEffect
  , Effect
    ( Lift, FFI, UnsafeObject, UnsafeObjectGet, UnsafeObjectAssign
    , CallMethod, Bind, LambdaE, ApplyE, IfE, IfS, While, OptionCaseE, Try
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
  ) where

-- Indexed PHOAS (binders @f u@, closed terms @forall f@) in the style of
-- Chlipala / Kmett's parametric HOAS, with host-language sharing as in
-- https://www.reddit.com/r/haskell/comments/85een6/sharing_from_phoas_multiple_interpreters_from_free/dvxhlba
--
-- 'Expr' is the pure tree; 'Effect' is the impure tree. They join at FFI
-- through 'Arg', not by treating effects as expressions.

import Data.Functor.Const (Const(..))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import GHC.Exts (Any)
import Numeric (showFFloat)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, eqStableName, hashStableName, makeStableName)
import Text.PrettyPrint ((<+>), Doc, ($$))
import Unsafe.Coerce (unsafeCoerce)
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
eqFoldableValue _ = True

peelOption :: Expr (Const Int) ('Option u) -> Maybe (Maybe (Expr (Const Int) u))
peelOption = \case
  Literal (ValueOption Nothing) -> Just Nothing
  Literal (ValueOption (Just v)) -> Just (Just (Literal v))
  -- Host literals are never JS null. FFI / vars stay unpeeled so
  -- 'Storage.getItem' keeps its @=== null@ check.
  UnsafeNullable (Literal v) -> Just (Just (Literal v))
  _ -> Nothing

peelBoolEffect :: Effect (Const Int) 'Bool -> Maybe Bool
peelBoolEffect (Lift (Literal (ValueBool b))) = Just b
peelBoolEffect _ = Nothing

evaluateNumber :: ClosedExpr 'Number -> Double
evaluateNumber e = unNumber (evaluate e)

-- | Pure reference interpreter. Shared Haskell heap nodes are walked
-- once per occurrence (no memo table). Use 'evaluateCached' when host-level
-- sharing should be observed.
evaluate :: ClosedExpr u -> Value u
evaluate e0 = eval e0 where
  eval :: forall v. Expr Value v -> Value v
  eval = \case
    Literal v -> v
    Plus x y -> ValueNumber (unNumber (eval x) + unNumber (eval y))
    Times x y -> ValueNumber (unNumber (eval x) * unNumber (eval y))
    Minus x y -> ValueNumber (unNumber (eval x) - unNumber (eval y))
    Negate x -> ValueNumber (negate (unNumber (eval x)))
    FracDiv x y -> ValueNumber (unNumber (eval x) / unNumber (eval y))
    Var x -> x
    Apply g x -> unFunction (eval g) (eval x)
    Lambda g -> ValueFunction (eval . g)
    Concat x y -> ValueString (unString (eval x) <> unString (eval y))
    Show x -> ValueString (jsShow (eval x))
    TypeOf x -> ValueString (typeOfValue (eval x))
    And x y -> ValueBool (unBool (eval x) && unBool (eval y))
    Or x y -> ValueBool (unBool (eval x) || unBool (eval y))
    Eq x y -> ValueBool (valueEq (eval x) (eval y))
    NEq x y -> ValueBool (not (valueEq (eval x) (eval y)))
    GTh x y -> ValueBool (valueCompare (eval x) (eval y) == GT)
    LTh x y -> ValueBool (valueCompare (eval x) (eval y) == LT)
    GTEq x y -> ValueBool (valueCompare (eval x) (eval y) /= LT)
    LTEq x y -> ValueBool (valueCompare (eval x) (eval y) /= GT)
    Let x g -> eval (g (eval x))
    If c t e -> if unBool (eval c) then eval t else eval e
    OptionCase opt none' someF -> case eval opt of
      ValueOption Nothing -> eval none'
      ValueOption (Just x) -> eval (someF x)
    UnsafeEffectExpr _ -> cannotEval "an embedded Effect (UnsafeEffectExpr)"
    ExprUnary{} -> cannotEval "a stdlib ExprUnary"
    ExprBinary{} -> cannotEval "a stdlib ExprBinary"
    ExprTernary{} -> cannotEval "a stdlib ExprTernary"
    ExprMap{} -> cannotEval "ExprMap"
    ExprFilter{} -> cannotEval "ExprFilter"
    ExprIndex xs i -> case eval xs of
      ValueArray vs ->
        -- JS array indexing truncates the index toward zero (as part of
        -- ToIntegerOrInfinity) rather than rounding, and returns @undefined@
        -- out of bounds rather than crashing; we can't represent
        -- @undefined@ generically here (there's no 'Value' inhabitant for
        -- an arbitrary universe @u@), so out-of-bounds access is a hard
        -- error in the reference interpreter.
        let idx = truncate (unNumber (eval i)) :: Int
         in if idx >= 0 && idx < length vs
              then vs !! idx
              else error "evaluate: array index out of bounds"
    MathUnary name x -> ValueNumber (mathUnaryFn name (unNumber (eval x)))
    MathBinary name x y -> ValueNumber (mathBinaryFn name (unNumber (eval x)) (unNumber (eval y)))
    UnsafeNullable x -> ValueOption (Just (eval x))

-- Per-evaluation memo table keyed by 'StableName'. Recovers host-language
-- sharing (Haskell @let x = e in x + x@) so a shared 'Expr' node is only
-- interpreted once. Object-language 'Let' already preserves sharing on its
-- own; this cache is what makes the two coincide.
type EvalCache = IORef (IntMap [(StableName (), Any)])

-- | Like 'evaluate', but memoizes shared heap nodes via 'StableName'.
-- In 'IO' because observable sharing is inherently effectful.
evaluateCached :: ClosedExpr u -> IO (Value u)
evaluateCached e0 = do
  cache <- newIORef IM.empty
  go cache e0

go :: forall v. EvalCache -> Expr Value v -> IO (Value v)
go cache e = do
  sn <- makeStableName $! e
  m <- readIORef cache
  case lookupCache sn (IM.lookup (hashStableName sn) m) of
    Just v -> pure v
    Nothing -> do
      v <- goNode cache e
      modifyIORef' cache
        (IM.insertWith (++) (hashStableName sn) [(snToUnit sn, toAny v)])
      pure v

lookupCache :: StableName (Expr Value v) -> Maybe [(StableName (), Any)] -> Maybe (Value v)
lookupCache sn ments = do
  entries <- ments
  toAnyVal <- listToMaybe [ a | (sn', a) <- entries, eqStableName (snToUnit sn) sn' ]
  pure (fromAny toAnyVal)

snToUnit :: StableName a -> StableName ()
snToUnit = unsafeCoerce

toAny :: a -> Any
toAny = unsafeCoerce

fromAny :: Any -> a
fromAny = unsafeCoerce

-- Named and NOINLINE so GHC cannot CSE applications at different 'v'.
applyCached :: EvalCache -> (Value u -> Expr Value v) -> Value u -> Value v
applyCached cache g v = unsafePerformIO (go cache (g v))
{-# NOINLINE applyCached #-}

goNode :: forall v. EvalCache -> Expr Value v -> IO (Value v)
goNode cache = \case
  Literal v -> pure v
  Plus x y -> num2 (+) x y
  Times x y -> num2 (*) x y
  Minus x y -> num2 (-) x y
  Negate x -> num1 negate x
  FracDiv x y -> num2 (/) x y
  Var x -> pure x
  Apply g x -> unFunction <$> go cache g <*> go cache x
  Lambda g -> pure (ValueFunction (applyCached cache g))
  Concat x y -> do
    a <- go cache x
    b <- go cache y
    pure (ValueString (unString a <> unString b))
  Show x -> ValueString . jsShow <$> go cache x
  TypeOf x -> ValueString . typeOfValue <$> go cache x
  And x y -> do
    a <- go cache x
    if unBool a then go cache y else pure (ValueBool False)
  Or x y -> do
    a <- go cache x
    if unBool a then pure (ValueBool True) else go cache y
  Eq x y -> ValueBool <$> (valueEq <$> go cache x <*> go cache y)
  NEq x y -> ValueBool . not <$> (valueEq <$> go cache x <*> go cache y)
  GTh x y -> ValueBool . (== GT) <$> (valueCompare <$> go cache x <*> go cache y)
  LTh x y -> ValueBool . (== LT) <$> (valueCompare <$> go cache x <*> go cache y)
  GTEq x y -> ValueBool . (/= LT) <$> (valueCompare <$> go cache x <*> go cache y)
  LTEq x y -> ValueBool . (/= GT) <$> (valueCompare <$> go cache x <*> go cache y)
  Let x g -> go cache x >>= go cache . g
  If c t e -> do
    cv <- go cache c
    if unBool cv then go cache t else go cache e
  OptionCase opt none' someF -> do
    ov <- go cache opt
    case ov of
      ValueOption Nothing -> go cache none'
      ValueOption (Just x) -> go cache (someF x)
  UnsafeEffectExpr _ -> cannotEval "an embedded Effect (UnsafeEffectExpr)"
  ExprUnary{} -> cannotEval "a stdlib ExprUnary"
  ExprBinary{} -> cannotEval "a stdlib ExprBinary"
  ExprTernary{} -> cannotEval "a stdlib ExprTernary"
  ExprMap{} -> cannotEval "ExprMap"
  ExprFilter{} -> cannotEval "ExprFilter"
  ExprIndex xs i -> do
    arr <- go cache xs
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
  UnsafeNullable x -> ValueOption . Just <$> go cache x
  where
    num1 f x = ValueNumber . f . unNumber <$> go cache x
    num2 f x y = ValueNumber <$> (f <$> (unNumber <$> go cache x) <*> (unNumber <$> go cache y))

fromRightE :: Either [Char] c -> c
fromRightE = either error id

printComputation :: Doc -> IO ()
printComputation (computation) = do
  putStrLn $ P.renderStyle P.style computation

renderJS :: Doc -> String
renderJS = P.renderStyle P.style

data Code = Code
  { codeDecl :: Doc
  , codeRef :: Doc
  }

instance Semigroup Code where
  Code a b <> Code x y = Code (a <> b) (x <> y)

instance Monoid Code where
  mempty = Code mempty mempty

renderCode :: Code -> Doc
renderCode (Code a b) = a $$ b

-- | Wrap generated decls + result in an IIFE so a minifier treats the
-- result as live (plain expression statements get DCE'd).
renderIIFE :: Code -> Doc
renderIIFE (Code decls ref) =
  let body = if P.isEmpty ref then decls else decls $$ (("return" <+> ref) <> P.semi)
   in "(() => {" $$ P.nest 2 body $$ "})()"

-- | Pure expression compiled to a self-contained JS program (IIFE).
pureProgram :: ClosedExpr u -> Doc
pureProgram e = renderIIFE . snd . pureAST' startCG $ optimize e

-- | Effectful computation compiled to a self-contained JS program (IIFE).
effectfulProgram :: ClosedEffect u -> Doc
effectfulProgram e = renderIIFE . snd . effectfulAST' startCG $ optimizeEffect e

partitionCode :: [Code] -> ([Doc], [Doc])
partitionCode ((Code a b):cs) = let (as,bs) = partitionCode cs in ((a:as),(b:bs))
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

nestedDummy :: Const Int u
nestedDummy = Const nestedDummyId

constBind :: Int -> Doc -> Doc
constBind n ref = ("const" <+> P.text ('n' : show n) <+> "=" <+> ref) <> P.semi

jsCall :: Doc -> Doc -> Doc
jsCall f a = P.parens f <> P.parens a

isSimple :: Expr (Const Int) u -> Bool
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
  -- Single-use Effect spliced into an Expr hole (e.g. inlined 'ffi').
  UnsafeEffectExpr e -> isSimpleEffect e
  _ -> False

isSimpleEffect :: Effect (Const Int) u -> Bool
isSimpleEffect = \case
  Lift x -> isSimple x
  FFI{} -> True
  CallMethod{} -> True
  UnsafeObject{} -> True
  UnsafeObjectGet{} -> True
  _ -> False

wrapOperand :: Expr (Const Int) u -> Doc -> Doc
wrapOperand e d = if isSimple e then d else P.parens d

-- A use under a lambda, loop, `&&`/`||` RHS, or `?:` branch is not a
-- candidate for inlining: the binder would be re-run or skipped.
countLazyExpr :: Int -> Expr (Const Int) u -> Int
countLazyExpr t e = if countExpr t e == 0 then 0 else 2

countLazyEffect :: Int -> Effect (Const Int) u -> Int
countLazyEffect t e = if countEffect t e == 0 then 0 else 2

countArgs :: Int -> Rec (Arg (Const Int)) us -> Int
countArgs t = recFold (\n a -> n + countArg t a) 0

countArg :: Int -> Arg (Const Int) u -> Int
countArg t (ArgExpr e) = countExpr t e
countArg t (ArgEffect e) = countEffect t e

countExpr :: Int -> Expr (Const Int) u -> Int
countExpr t = \case
  Literal{} -> 0
  Var (Const i) -> if i == t then 1 else 0
  Concat x y -> countExpr t x + countExpr t y
  Plus x y -> countExpr t x + countExpr t y
  Times x y -> countExpr t x + countExpr t y
  Minus x y -> countExpr t x + countExpr t y
  Negate x -> countExpr t x
  FracDiv x y -> countExpr t x + countExpr t y
  And x y -> countExpr t x + countLazyExpr t y
  Or x y -> countExpr t x + countLazyExpr t y
  Eq x y -> countExpr t x + countExpr t y
  NEq x y -> countExpr t x + countExpr t y
  GTh x y -> countExpr t x + countExpr t y
  LTh x y -> countExpr t x + countExpr t y
  GTEq x y -> countExpr t x + countExpr t y
  LTEq x y -> countExpr t x + countExpr t y
  Let x g -> countExpr t x + countExpr t (g nestedDummy)
  Lambda g -> countLazyExpr t (g nestedDummy)
  Apply f x -> countExpr t f + countExpr t x
  Show x -> countExpr t x
  TypeOf x -> countExpr t x
  If c u v -> countExpr t c + countLazyExpr t u + countLazyExpr t v
  OptionCase o n s ->
    countExpr t o + countLazyExpr t n + countLazyExpr t (s nestedDummy)
  UnsafeEffectExpr e -> countEffect t e
  ExprUnary _ x -> countExpr t x
  ExprBinary _ x y -> countExpr t x + countExpr t y
  ExprTernary _ x y z -> countExpr t x + countExpr t y + countExpr t z
  ExprMap x f -> countExpr t x + countLazyExpr t (f nestedDummy)
  ExprFilter x f -> countExpr t x + countLazyExpr t (f nestedDummy)
  ExprIndex x i -> countExpr t x + countExpr t i
  MathUnary _ x -> countExpr t x
  MathBinary _ x y -> countExpr t x + countExpr t y
  UnsafeNullable x -> countExpr t x

countEffect :: Int -> Effect (Const Int) u -> Int
countEffect t = \case
  Lift x -> countExpr t x
  FFI _ args -> countArgs t args
  UnsafeObject _ -> 0
  UnsafeObjectGet x _ -> countEffect t x
  UnsafeObjectAssign x y -> countEffect t x + countEffect t y
  CallMethod x _ args -> countEffect t x + countArgs t args
  Bind x f -> countEffect t x + countEffect t (f nestedDummy)
  LambdaE f -> countLazyEffect t (f nestedDummy)
  ApplyE f x -> countEffect t f + countEffect t x
  IfE c u v -> countEffect t c + countLazyEffect t u + countLazyEffect t v
  IfS c u v -> countEffect t c + countLazyEffect t u + countLazyEffect t v
  While c b -> countLazyEffect t c + countLazyEffect t b
  OptionCaseE o n s ->
    countExpr t o + countLazyEffect t n + countLazyEffect t (s nestedDummy)
  Try a b -> countEffect t a + countLazyEffect t b

substArgs :: Int -> Expr (Const Int) u -> Rec (Arg (Const Int)) us -> Rec (Arg (Const Int)) us
substArgs t r = mapRec (substArg t r)

substArg :: Int -> Expr (Const Int) u -> Arg (Const Int) v -> Arg (Const Int) v
substArg t r (ArgExpr e) = ArgExpr (substExpr t r e)
substArg t r (ArgEffect e) = ArgEffect (substEffect t r e)

substExpr :: Int -> Expr (Const Int) u -> Expr (Const Int) v -> Expr (Const Int) v
substExpr t r = goExpr
  where
    goExpr :: Expr (Const Int) w -> Expr (Const Int) w
    goExpr = \case
      v@(Var (Const i))
        | i == t -> unsafeCoerce r
        | otherwise -> v
      Literal x -> Literal x
      Concat x y -> Concat (goExpr x) (goExpr y)
      Plus x y -> Plus (goExpr x) (goExpr y)
      Times x y -> Times (goExpr x) (goExpr y)
      Minus x y -> Minus (goExpr x) (goExpr y)
      Negate x -> Negate (goExpr x)
      FracDiv x y -> FracDiv (goExpr x) (goExpr y)
      And x y -> And (goExpr x) (goExpr y)
      Or x y -> Or (goExpr x) (goExpr y)
      Eq x y -> Eq (goExpr x) (goExpr y)
      NEq x y -> NEq (goExpr x) (goExpr y)
      GTh x y -> GTh (goExpr x) (goExpr y)
      LTh x y -> LTh (goExpr x) (goExpr y)
      GTEq x y -> GTEq (goExpr x) (goExpr y)
      LTEq x y -> LTEq (goExpr x) (goExpr y)
      Let x g -> Let (goExpr x) (goExpr . g)
      Lambda g -> Lambda (goExpr . g)
      Apply f x -> Apply (goExpr f) (goExpr x)
      Show x -> Show (goExpr x)
      TypeOf x -> TypeOf (goExpr x)
      If c u v -> If (goExpr c) (goExpr u) (goExpr v)
      OptionCase o n s -> OptionCase (goExpr o) (goExpr n) (goExpr . s)
      UnsafeEffectExpr e -> UnsafeEffectExpr (substEffect t r e)
      ExprUnary n x -> ExprUnary n (goExpr x)
      ExprBinary n x y -> ExprBinary n (goExpr x) (goExpr y)
      ExprTernary n x y z -> ExprTernary n (goExpr x) (goExpr y) (goExpr z)
      ExprMap x f -> ExprMap (goExpr x) (goExpr . f)
      ExprFilter x f -> ExprFilter (goExpr x) (goExpr . f)
      ExprIndex x i -> ExprIndex (goExpr x) (goExpr i)
      MathUnary n x -> MathUnary n (goExpr x)
      MathBinary n x y -> MathBinary n (goExpr x) (goExpr y)
      UnsafeNullable x -> UnsafeNullable (goExpr x)

substEffect :: Int -> Expr (Const Int) u -> Effect (Const Int) v -> Effect (Const Int) v
substEffect t r = goE
  where
    goE :: Effect (Const Int) w -> Effect (Const Int) w
    goE = \case
      Lift x -> Lift (substExpr t r x)
      FFI n args -> FFI n (substArgs t r args)
      UnsafeObject o -> UnsafeObject o
      UnsafeObjectGet x s -> UnsafeObjectGet (goE x) s
      UnsafeObjectAssign x y -> UnsafeObjectAssign (goE x) (goE y)
      CallMethod x n args -> CallMethod (goE x) n (substArgs t r args)
      Bind x f -> Bind (goE x) (goE . f)
      LambdaE f -> LambdaE (goE . f)
      ApplyE f x -> ApplyE (goE f) (goE x)
      IfE c u v -> IfE (goE c) (goE u) (goE v)
      IfS c u v -> IfS (goE c) (goE u) (goE v)
      While c b -> While (goE c) (goE b)
      OptionCaseE o n s -> OptionCaseE (substExpr t r o) (goE n) (goE . s)
      Try a b -> Try (goE a) (goE b)

-- | Constant-fold and drop dead pure bindings. Applied automatically by
-- codegen. Literals are propagated even under lambdas; effectful or
-- non-cheap bindings follow the same strict-use rule as inlining.
optimize :: ClosedExpr u -> Expr (Const Int) u
optimize e = snd (optExpr (-2) e)

optimizeEffect :: ClosedEffect u -> Effect (Const Int) u
optimizeEffect e = snd (optEffect (-2) e)

optUnder :: Int -> (Const Int u -> Expr (Const Int) v) -> (Int, Int, Expr (Const Int) v)
optUnder t0 f =
  let tag = t0
      (t1, body) = optExpr (t0 - 1) (f (Const tag))
   in (t1, tag, body)

optUnderE :: Int -> (Const Int u -> Effect (Const Int) v) -> (Int, Int, Effect (Const Int) v)
optUnderE t0 f =
  let tag = t0
      (t1, body) = optEffect (t0 - 1) (f (Const tag))
   in (t1, tag, body)

rebind :: forall (u :: Universe) (v :: Universe).
  Int -> Expr (Const Int) v -> (Const Int u -> Expr (Const Int) v)
rebind tag body b = substExpr tag (Var b) body

rebindE :: forall (u :: Universe) (v :: Universe).
  Int -> Effect (Const Int) v -> (Const Int u -> Effect (Const Int) v)
rebindE tag body b = substEffect tag (Var b) body

isCheapValue :: Value u -> Bool
isCheapValue = \case
  ValueNumber{} -> True
  ValueString{} -> True
  ValueBool{} -> True
  ValueUnit -> True
  ValueOption Nothing -> True
  ValueOption (Just v) -> isCheapValue v
  ValueArray{} -> False
  ValueFunction{} -> False

isCheap :: Expr (Const Int) u -> Bool
isCheap = \case
  Literal v -> isCheapValue v
  UnsafeNullable x -> isCheap x
  _ -> False

isCheapEffect :: Effect (Const Int) u -> Bool
isCheapEffect = \case
  Lift x -> isCheap x
  -- Object literals are identity-sensitive (mutation / shared state).
  UnsafeObject{} -> False
  _ -> False

isPureExpr :: Expr (Const Int) u -> Bool
isPureExpr = \case
  Literal{} -> True
  Var{} -> True
  Concat x y -> isPureExpr x && isPureExpr y
  Plus x y -> isPureExpr x && isPureExpr y
  Times x y -> isPureExpr x && isPureExpr y
  Minus x y -> isPureExpr x && isPureExpr y
  Negate x -> isPureExpr x
  FracDiv x y -> isPureExpr x && isPureExpr y
  And x y -> isPureExpr x && isPureExpr y
  Or x y -> isPureExpr x && isPureExpr y
  Eq x y -> isPureExpr x && isPureExpr y
  NEq x y -> isPureExpr x && isPureExpr y
  GTh x y -> isPureExpr x && isPureExpr y
  LTh x y -> isPureExpr x && isPureExpr y
  GTEq x y -> isPureExpr x && isPureExpr y
  LTEq x y -> isPureExpr x && isPureExpr y
  Let x g -> isPureExpr x && isPureExpr (g nestedDummy)
  Lambda g -> isPureExpr (g nestedDummy)
  Apply f x -> isPureExpr f && isPureExpr x
  Show x -> isPureExpr x
  TypeOf x -> isPureExpr x
  If c t e -> isPureExpr c && isPureExpr t && isPureExpr e
  OptionCase o n s -> isPureExpr o && isPureExpr n && isPureExpr (s nestedDummy)
  UnsafeEffectExpr _ -> False
  ExprUnary n x -> isPureStdUnary n && isPureExpr x
  ExprBinary _ x y -> isPureExpr x && isPureExpr y
  ExprTernary _ x y z -> isPureExpr x && isPureExpr y && isPureExpr z
  ExprMap x f -> isPureExpr x && isPureExpr (f nestedDummy)
  ExprFilter x f -> isPureExpr x && isPureExpr (f nestedDummy)
  ExprIndex x i -> isPureExpr x && isPureExpr i
  MathUnary _ x -> isPureExpr x
  MathBinary _ x y -> isPureExpr x && isPureExpr y
  UnsafeNullable x -> isPureExpr x

-- | @JSON.stringify@ throws on bigint / circular values, so unused
-- stringify is kept.
isPureStdUnary :: StdUnary a b -> Bool
isPureStdUnary StdStringify = False
isPureStdUnary _ = True

isPureEffect :: Effect (Const Int) u -> Bool
isPureEffect = \case
  Lift x -> isPureExpr x
  FFI{} -> False
  UnsafeObject{} -> True
  UnsafeObjectGet{} -> False
  UnsafeObjectAssign{} -> False
  CallMethod{} -> False
  Bind x f -> isPureEffect x && isPureEffect (f nestedDummy)
  LambdaE f -> isPureEffect (f nestedDummy)
  ApplyE{} -> False
  IfE c t e -> isPureEffect c && isPureEffect t && isPureEffect e
  IfS c t e -> isPureEffect c && isPureEffect t && isPureEffect e
  While{} -> False
  OptionCaseE o n s -> isPureExpr o && isPureEffect n && isPureEffect (s nestedDummy)
  Try{} -> False

optArgs :: Int -> Rec (Arg (Const Int)) us -> (Int, Rec (Arg (Const Int)) us)
optArgs = mapAccumRec optArg

optArg :: Int -> Arg (Const Int) u -> (Int, Arg (Const Int) u)
optArg t (ArgExpr e) = fmap ArgExpr (optExpr t e)
optArg t (ArgEffect e) = fmap ArgEffect (optEffect t e)

foldNum1 :: (Double -> Double) -> (Expr (Const Int) 'Number -> Expr (Const Int) 'Number) -> Expr (Const Int) 'Number -> Expr (Const Int) 'Number
foldNum1 f k = \case
  Literal (ValueNumber a) -> Literal (ValueNumber (f a))
  x -> k x

foldNum2 :: (Double -> Double -> Double) -> (Expr (Const Int) 'Number -> Expr (Const Int) 'Number -> Expr (Const Int) 'Number) -> Expr (Const Int) 'Number -> Expr (Const Int) 'Number -> Expr (Const Int) 'Number
foldNum2 f k x y = case (x, y) of
  (Literal (ValueNumber a), Literal (ValueNumber b)) -> Literal (ValueNumber (f a b))
  _ -> k x y

foldConcat :: Expr (Const Int) 'String -> Expr (Const Int) 'String -> Expr (Const Int) 'String
foldConcat x y = case (x, y) of
  (Literal (ValueString a), Literal (ValueString b)) -> Literal (ValueString (a <> b))
  _ -> Concat x y

foldAnd :: Expr (Const Int) 'Bool -> Expr (Const Int) 'Bool -> Expr (Const Int) 'Bool
foldAnd x y = case (x, y) of
  (Literal (ValueBool False), _) -> Literal (ValueBool False)
  (Literal (ValueBool True), y') -> y'
  (_, Literal (ValueBool True)) -> x
  (x', Literal (ValueBool False)) | isPureExpr x' -> Literal (ValueBool False)
  _ -> And x y

foldOr :: Expr (Const Int) 'Bool -> Expr (Const Int) 'Bool -> Expr (Const Int) 'Bool
foldOr x y = case (x, y) of
  (Literal (ValueBool True), _) -> Literal (ValueBool True)
  (Literal (ValueBool False), y') -> y'
  (_, Literal (ValueBool False)) -> x
  (x', Literal (ValueBool True)) | isPureExpr x' -> Literal (ValueBool True)
  _ -> Or x y

foldEq :: Expr (Const Int) u -> Expr (Const Int) u -> Expr (Const Int) 'Bool
foldEq x y = case (x, y) of
  (Literal a, Literal b)
    | eqFoldableValue a && eqFoldableValue b -> Literal (ValueBool (valueEq a b))
  _ -> Eq x y

foldNEq :: Expr (Const Int) u -> Expr (Const Int) u -> Expr (Const Int) 'Bool
foldNEq x y = case (x, y) of
  (Literal a, Literal b)
    | eqFoldableValue a && eqFoldableValue b -> Literal (ValueBool (not (valueEq a b)))
  _ -> NEq x y

foldOrd :: Ordering -> (Expr (Const Int) u -> Expr (Const Int) u -> Expr (Const Int) 'Bool) -> Expr (Const Int) u -> Expr (Const Int) u -> Expr (Const Int) 'Bool
foldOrd ord k x y = case (x, y) of
  (Literal a, Literal b)
    | isOrderableValue a && isOrderableValue b -> Literal (ValueBool (valueCompare a b == ord))
  _ -> k x y

foldOrdNeq :: Ordering -> (Expr (Const Int) u -> Expr (Const Int) u -> Expr (Const Int) 'Bool) -> Expr (Const Int) u -> Expr (Const Int) u -> Expr (Const Int) 'Bool
foldOrdNeq ord k x y = case (x, y) of
  (Literal a, Literal b)
    | isOrderableValue a && isOrderableValue b -> Literal (ValueBool (valueCompare a b /= ord))
  _ -> k x y

foldShow :: Expr (Const Int) u -> Expr (Const Int) 'String
foldShow x = case x of
  Literal (ValueFunction _) -> Show x
  Literal v -> Literal (ValueString (jsShow v))
  _ -> Show x

foldTypeOf :: Expr (Const Int) u -> Expr (Const Int) 'String
foldTypeOf x = case x of
  Literal v -> Literal (ValueString (typeOfValue v))
  _ -> TypeOf x

foldIndex :: Expr (Const Int) ('Array u) -> Expr (Const Int) 'Number -> Expr (Const Int) u
foldIndex arr idx = case (arr, idx) of
  (Literal (ValueArray vs), Literal (ValueNumber d))
    | let i = truncate d :: Int
    , i >= 0 && i < length vs -> Literal (vs !! i)
  _ -> ExprIndex arr idx

foldMathUnary :: MathFn1 -> Expr (Const Int) 'Number -> Expr (Const Int) 'Number
foldMathUnary n x = case x of
  Literal (ValueNumber a)
    | Just r <- exactMathUnary n a -> Literal (ValueNumber r)
  _ -> MathUnary n x

foldMathBinary :: MathFn2 -> Expr (Const Int) 'Number -> Expr (Const Int) 'Number -> Expr (Const Int) 'Number
foldMathBinary n x y = case (x, y) of
  (Literal (ValueNumber a), Literal (ValueNumber b))
    | Just r <- exactMathBinary n a b -> Literal (ValueNumber r)
  _ -> MathBinary n x y

optLet :: Int -> Expr (Const Int) u -> (Const Int u -> Expr (Const Int) v) -> (Int, Expr (Const Int) v)
optLet t0 x f =
  let (t1, x') = optExpr t0 x
      (t2, tag, body) = optUnder t1 f
   in elimLetFrom t2 x' tag body

-- Count/subst only. Algebraic fold of the substituted body is a single
-- `foldExpr` walk so a chain of lets does not re-run let-elim on the
-- whole remainder (that was O(n²)).
elimLetFrom :: Int -> Expr (Const Int) u -> Int -> Expr (Const Int) v -> (Int, Expr (Const Int) v)
elimLetFrom t x tag body =
  let uses = countExpr tag body
      rebuild = Let x (rebind tag body)
   in case uses of
        -- uses==1 is always a single strict use: countExpr already
        -- treats lambda/loop/?:/&&-|| RHS positions as 2.
        0 | isPureExpr x -> (t, body)
        0 -> (t, rebuild)
        1 -> foldExpr t (substExpr tag x body)
        _ | isCheap x -> foldExpr t (substExpr tag x body)
        _ -> (t, rebuild)

boundAsExpr :: Effect (Const Int) u -> Expr (Const Int) u
boundAsExpr (Lift e) = e
boundAsExpr e = UnsafeEffectExpr e

optBind :: Int -> Effect (Const Int) u -> (Const Int u -> Effect (Const Int) v) -> (Int, Effect (Const Int) v)
optBind t0 x f =
  let (t1, x') = optEffect t0 x
      (t2, tag, body) = optUnderE t1 f
   in elimBindFrom t2 x' tag body

elimBindFrom :: Int -> Effect (Const Int) u -> Int -> Effect (Const Int) v -> (Int, Effect (Const Int) v)
elimBindFrom t x tag body =
  let uses = countEffect tag body
      rebuild = Bind x (rebindE tag body)
      inline = foldEffect t (substEffect tag (boundAsExpr x) body)
   in case uses of
        0 | isPureEffect x -> (t, body)
        0 -> (t, rebuild)
        1 -> inline
        _ | isCheapEffect x -> inline
        _ -> (t, rebuild)

optExpr :: Int -> Expr (Const Int) u -> (Int, Expr (Const Int) u)
optExpr t0 = \case
  Literal v -> (t0, Literal v)
  Var v -> (t0, Var v)
  Concat x y ->
    let (t1, x') = optExpr t0 x
        (t2, y') = optExpr t1 y
     in (t2, foldConcat x' y')
  Plus x y -> binNum (+) Plus x y
  Times x y -> binNum (*) Times x y
  Minus x y -> binNum (-) Minus x y
  FracDiv x y -> binNum (/) FracDiv x y
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
  Lambda f ->
    let (t1, tag, body) = optUnder t0 f
     in (t1, Lambda (rebind tag body))
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
             in elimLetFrom t2 x tag body
          Nothing ->
            let (t2, n') = optExpr t1 n
                (t3, tag, body) = optUnder t2 s
             in (t3, OptionCase o' n' (rebind tag body))
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
     in (t2, ExprMap x' (rebind tag body))
  ExprFilter x f ->
    let (t1, x') = optExpr t0 x
        (t2, tag, body) = optUnder t1 f
     in (t2, ExprFilter x' (rebind tag body))
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
  where
    binNum f k x y =
      let (t1, x') = optExpr t0 x
          (t2, y') = optExpr t1 y
       in (t2, foldNum2 f k x' y')
    unNum f k x =
      let (t1, x') = optExpr t0 x
       in (t1, foldNum1 f k x')

optEffect :: Int -> Effect (Const Int) u -> (Int, Effect (Const Int) u)
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
  LambdaE f ->
    let (t1, tag, body) = optUnderE t0 f
     in (t1, LambdaE (rebindE tag body))
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
  IfS c t e ->
    let (t1, c') = optEffect t0 c
     in case peelBoolEffect c' of
          Just True -> optEffect t1 t
          Just False -> optEffect t1 e
          Nothing ->
            let (t2, t') = optEffect t1 t
                (t3, e') = optEffect t2 e
             in (t3, IfS c' t' e')
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
             in elimBindFrom t2 (Lift x) tag body
          Nothing ->
            let (t2, n') = optEffect t1 n
                (t3, tag, body) = optUnderE t2 s
             in (t3, OptionCaseE o' n' (rebindE tag body))
  Try a b ->
    let (t1, a') = optEffect t0 a
        (t2, b') = optEffect t1 b
     in (t2, Try a' b')

-- Algebraic fold only (no let/bind inlining). Used after subst so we
-- still collapse `5+5` without re-running the full optExpr walk.
foldUnder :: Int -> (Const Int u -> Expr (Const Int) v) -> (Int, Int, Expr (Const Int) v)
foldUnder t0 f =
  let tag = t0
      (t1, body) = foldExpr (t0 - 1) (f (Const tag))
   in (t1, tag, body)

foldUnderE :: Int -> (Const Int u -> Effect (Const Int) v) -> (Int, Int, Effect (Const Int) v)
foldUnderE t0 f =
  let tag = t0
      (t1, body) = foldEffect (t0 - 1) (f (Const tag))
   in (t1, tag, body)

foldArgs :: Int -> Rec (Arg (Const Int)) us -> (Int, Rec (Arg (Const Int)) us)
foldArgs = mapAccumRec foldArg

foldArg :: Int -> Arg (Const Int) u -> (Int, Arg (Const Int) u)
foldArg t (ArgExpr e) = fmap ArgExpr (foldExpr t e)
foldArg t (ArgEffect e) = fmap ArgEffect (foldEffect t e)

foldExpr :: Int -> Expr (Const Int) u -> (Int, Expr (Const Int) u)
foldExpr t0 = \case
  Literal v -> (t0, Literal v)
  Var v -> (t0, Var v)
  Concat x y ->
    let (t1, x') = foldExpr t0 x
        (t2, y') = foldExpr t1 y
     in (t2, foldConcat x' y')
  Plus x y -> bin (+) Plus x y
  Times x y -> bin (*) Times x y
  Minus x y -> bin (-) Minus x y
  FracDiv x y -> bin (/) FracDiv x y
  Negate x -> un negate Negate x
  And x y ->
    let (t1, x') = foldExpr t0 x
     in case x' of
          Literal (ValueBool False) -> (t1, Literal (ValueBool False))
          Literal (ValueBool True) -> foldExpr t1 y
          _ ->
            let (t2, y') = foldExpr t1 y
             in (t2, foldAnd x' y')
  Or x y ->
    let (t1, x') = foldExpr t0 x
     in case x' of
          Literal (ValueBool True) -> (t1, Literal (ValueBool True))
          Literal (ValueBool False) -> foldExpr t1 y
          _ ->
            let (t2, y') = foldExpr t1 y
             in (t2, foldOr x' y')
  Eq x y ->
    let (t1, x') = foldExpr t0 x
        (t2, y') = foldExpr t1 y
     in (t2, foldEq x' y')
  NEq x y ->
    let (t1, x') = foldExpr t0 x
        (t2, y') = foldExpr t1 y
     in (t2, foldNEq x' y')
  GTh x y ->
    let (t1, x') = foldExpr t0 x
        (t2, y') = foldExpr t1 y
     in (t2, foldOrd GT GTh x' y')
  LTh x y ->
    let (t1, x') = foldExpr t0 x
        (t2, y') = foldExpr t1 y
     in (t2, foldOrd LT LTh x' y')
  GTEq x y ->
    let (t1, x') = foldExpr t0 x
        (t2, y') = foldExpr t1 y
     in (t2, foldOrdNeq LT GTEq x' y')
  LTEq x y ->
    let (t1, x') = foldExpr t0 x
        (t2, y') = foldExpr t1 y
     in (t2, foldOrdNeq GT LTEq x' y')
  Let x f ->
    let (t1, x') = foldExpr t0 x
        (t2, tag, body) = foldUnder t1 f
     in (t2, Let x' (rebind tag body))
  Lambda f ->
    let (t1, tag, body) = foldUnder t0 f
     in (t1, Lambda (rebind tag body))
  Apply f x ->
    let (t1, f') = foldExpr t0 f
        (t2, x') = foldExpr t1 x
     in (t2, Apply f' x')
  Show x ->
    let (t1, x') = foldExpr t0 x
     in (t1, foldShow x')
  TypeOf x ->
    let (t1, x') = foldExpr t0 x
     in (t1, foldTypeOf x')
  If c t e ->
    let (t1, c') = foldExpr t0 c
     in case c' of
          Literal (ValueBool True) -> foldExpr t1 t
          Literal (ValueBool False) -> foldExpr t1 e
          _ ->
            let (t2, t') = foldExpr t1 t
                (t3, e') = foldExpr t2 e
             in (t3, If c' t' e')
  OptionCase o n s ->
    let (t1, o') = foldExpr t0 o
     in case peelOption o' of
          Just Nothing -> foldExpr t1 n
          Just (Just x) ->
            let (t2, tag, body) = foldUnder t1 s
             in elimLetFrom t2 x tag body
          Nothing ->
            let (t2, n') = foldExpr t1 n
                (t3, tag, body) = foldUnder t2 s
             in (t3, OptionCase o' n' (rebind tag body))
  UnsafeEffectExpr e ->
    let (t1, e') = foldEffect t0 e
     in case e' of
          Lift x -> (t1, x)
          _ -> (t1, UnsafeEffectExpr e')
  ExprUnary n x ->
    let (t1, x') = foldExpr t0 x
     in (t1, ExprUnary n x')
  ExprBinary n x y ->
    let (t1, x') = foldExpr t0 x
        (t2, y') = foldExpr t1 y
     in (t2, ExprBinary n x' y')
  ExprTernary n x y z ->
    let (t1, x') = foldExpr t0 x
        (t2, y') = foldExpr t1 y
        (t3, z') = foldExpr t2 z
     in (t3, ExprTernary n x' y' z')
  ExprMap x f ->
    let (t1, x') = foldExpr t0 x
        (t2, tag, body) = foldUnder t1 f
     in (t2, ExprMap x' (rebind tag body))
  ExprFilter x f ->
    let (t1, x') = foldExpr t0 x
        (t2, tag, body) = foldUnder t1 f
     in (t2, ExprFilter x' (rebind tag body))
  ExprIndex arr idx ->
    let (t1, arr') = foldExpr t0 arr
        (t2, idx') = foldExpr t1 idx
     in (t2, foldIndex arr' idx')
  MathUnary n x ->
    let (t1, x') = foldExpr t0 x
     in (t1, foldMathUnary n x')
  MathBinary n x y ->
    let (t1, x') = foldExpr t0 x
        (t2, y') = foldExpr t1 y
     in (t2, foldMathBinary n x' y')
  UnsafeNullable x -> fmap UnsafeNullable (foldExpr t0 x)
  where
    bin f k x y =
      let (t1, x') = foldExpr t0 x
          (t2, y') = foldExpr t1 y
       in (t2, foldNum2 f k x' y')
    un f k x =
      let (t1, x') = foldExpr t0 x
       in (t1, foldNum1 f k x')

foldEffect :: Int -> Effect (Const Int) u -> (Int, Effect (Const Int) u)
foldEffect t0 = \case
  Lift x ->
    let (t1, x') = foldExpr t0 x
     in case x' of
          UnsafeEffectExpr e -> (t1, e)
          _ -> (t1, Lift x')
  FFI n args -> fmap (FFI n) (foldArgs t0 args)
  UnsafeObject o -> (t0, UnsafeObject o)
  UnsafeObjectGet x s ->
    let (t1, x') = foldEffect t0 x
     in (t1, UnsafeObjectGet x' s)
  UnsafeObjectAssign x y ->
    let (t1, x') = foldEffect t0 x
        (t2, y') = foldEffect t1 y
     in (t2, UnsafeObjectAssign x' y')
  CallMethod x n args ->
    let (t1, x') = foldEffect t0 x
        (t2, args') = foldArgs t1 args
     in (t2, CallMethod x' n args')
  Bind x f ->
    let (t1, x') = foldEffect t0 x
        (t2, tag, body) = foldUnderE t1 f
     in (t2, Bind x' (rebindE tag body))
  LambdaE f ->
    let (t1, tag, body) = foldUnderE t0 f
     in (t1, LambdaE (rebindE tag body))
  ApplyE f x ->
    let (t1, f') = foldEffect t0 f
        (t2, x') = foldEffect t1 x
     in (t2, ApplyE f' x')
  IfE c t e ->
    let (t1, c') = foldEffect t0 c
     in case peelBoolEffect c' of
          Just True -> foldEffect t1 t
          Just False -> foldEffect t1 e
          Nothing ->
            let (t2, t') = foldEffect t1 t
                (t3, e') = foldEffect t2 e
             in (t3, IfE c' t' e')
  IfS c t e ->
    let (t1, c') = foldEffect t0 c
     in case peelBoolEffect c' of
          Just True -> foldEffect t1 t
          Just False -> foldEffect t1 e
          Nothing ->
            let (t2, t') = foldEffect t1 t
                (t3, e') = foldEffect t2 e
             in (t3, IfS c' t' e')
  While c b ->
    let (t1, c') = foldEffect t0 c
     in case peelBoolEffect c' of
          Just False -> (t1, Lift (Literal ValueUnit))
          _ ->
            let (t2, b') = foldEffect t1 b
             in (t2, While c' b')
  OptionCaseE o n s ->
    let (t1, o') = foldExpr t0 o
     in case peelOption o' of
          Just Nothing -> foldEffect t1 n
          Just (Just x) ->
            let (t2, tag, body) = foldUnderE t1 s
             in (t2, substEffect tag x body)
          Nothing ->
            let (t2, n') = foldEffect t1 n
                (t3, tag, body) = foldUnderE t2 s
             in (t3, OptionCaseE o' n' (rebindE tag body))
  Try a b ->
    let (t1, a') = foldEffect t0 a
        (t2, b') = foldEffect t1 b
     in (t2, Try a' b')

-- Bind of an Effect: when the continuation uses the binder once in a
-- strict position, splice the effect in place (so `x <- getEl; x.foo()`
-- becomes `getEl().foo()`); when never, keep it as a statement.
bindEffectCode :: CG -> Effect (Const Int) u -> (Const Int u -> Effect (Const Int) v) -> (CG, Code)
bindEffectCode s0 x f =
  let (tag, sTag) = allocTag s0
      tagged = f (Const tag)
      uses = countEffect tag tagged
   in case uses of
        1 -> effectfulAST' sTag (substEffect tag (UnsafeEffectExpr x) tagged)
        0 ->
          let (s1, Code xDecl xRef) = effectfulAST' sTag x
              (s2, Code yDecl yRef) = effectfulAST' s1 (f nestedDummy)
              stmt
                | P.isEmpty xDecl && not (P.isEmpty xRef) = xRef <> P.semi
                | otherwise = xDecl
           in (s2, Code (stmt $$ yDecl) yRef)
        _ ->
          let (s1, Code xDecl xRef) = effectfulAST' sTag x
           in if P.isEmpty xRef
                then
                  let (s2, Code yDecl yRef) = effectfulAST' s1 (f (Const (cgIdent s1 - 1)))
                   in (s2, Code (xDecl $$ yDecl) yRef)
                else
                  let (nBind, s2) = allocIdent s1
                      (s3, Code yDecl yRef) = effectfulAST' s2 (f (Const nBind))
                   in (s3, Code (xDecl $$ constBind nBind xRef $$ yDecl) yRef)

-- | @UnsafeEffectExpr@ is only introduced here as a subst placeholder so a
-- single-use effect binder can be spliced into an 'Expr' hole; the renderer
-- unwraps it immediately.


effectfulAST :: ClosedEffect u -> Doc
effectfulAST e = renderCode . snd . effectfulAST' startCG $ optimizeEffect e

-- | Witness that forces @u ~ 'Unit@: @noOp@, 'While', 'IfS'. Polymorphic
-- nodes ('UnsafeObjectAssign', 'CallMethod', 'FFI') do not count — they
-- inhabit any @u@. One witness arm on 'IfE'/'Try'/'OptionCaseE' is enough
-- because it fixes the shared result type.
isUnitWitness :: Effect (Const Int) u -> Bool
isUnitWitness = \case
  Lift (Literal ValueUnit) -> True
  Lift (UnsafeEffectExpr e) -> isUnitWitness e
  Lift _ -> False
  While{} -> True
  IfS{} -> True
  Bind _ f -> isUnitWitness (f nestedDummy)
  IfE _ t e -> isUnitWitness t || isUnitWitness e
  OptionCaseE _ n s -> isUnitWitness n || isUnitWitness (s nestedDummy)
  Try a b -> isUnitWitness a || isUnitWitness b
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

effectfulAST' :: forall v. CG -> Effect (Const Int) v -> (CG, Code)
effectfulAST' !s0 = \case
  Lift x -> pureAST' s0 x
  FFI fn args ->
    let (s1, argDecl, argRefs) = renderArgList argAST s0 args
     in (s1, Code argDecl (P.text fn <> P.parens argRefs))
  IfS c t e ->
    let (s1, Code cDecl cRef) = effectfulAST' s0 c
        (s2, Code tDecl tRef) = effectfulAST' s1 t
        (s3, Code eDecl eRef) = effectfulAST' s2 e
     in (s3, Code (cDecl $$ ifElseStmt cRef tDecl tRef eDecl eRef) mempty)
  IfE c t e
    | isUnitWitness t || isUnitWitness e ->
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
    | isUnitWitness noneE || isUnitWitness (someF nestedDummy) ->
        let (s1, Code oDecl oRef) = pureAST' s0 opt
            (nBind, s2) = allocIdent s1
            optVar = 'n' : show nBind
            (s3, Code nDecl nRef) = effectfulAST' s2 noneE
            (s4, Code sDecl sRef) = effectfulAST' s3 (someF (Const nBind))
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
            (s5, Code sDecl sRef) = effectfulAST' s4 (someF (Const nBind))
            assign ref = if P.isEmpty ref then mempty else (P.text resultVar <+> "=" <+> ref) <> P.semi
            stmt = oDecl $$ constBind nBind oRef
              $$ ("let" <+> P.text resultVar) <> P.semi
              $$ ("if" <+> P.parens (P.text optVar <+> "===" <+> "null")
                    <+> P.braces (P.nest 2 (nDecl $$ assign nRef)))
              $$ ("else" <+> P.braces (P.nest 2 (sDecl $$ assign sRef)))
         in (s5, Code stmt (P.text resultVar))
  Try a b
    | isUnitWitness a || isUnitWitness b ->
        let (s1, Code aDecl aRef) = effectfulAST' s0 a
            (catchN, s2) = allocIdent s1
            (s3, Code bDecl bRef) = effectfulAST' s2 b
            stmt = "try" <+> bracesNest (asStmt aDecl aRef)
              $$ ("catch" <+> P.parens (P.text ('n' : show catchN))
                    <+> bracesNest (asStmt bDecl bRef))
         in (s3, Code stmt mempty)
    | otherwise ->
        let (resultN, s1) = allocIdent s0
            resultVar = 'n' : show resultN
            (s2, Code aDecl aRef) = effectfulAST' s1 a
            (catchN, s3) = allocIdent s2
            (s4, Code bDecl bRef) = effectfulAST' s3 b
            assign ref = if P.isEmpty ref then mempty else (P.text resultVar <+> "=" <+> ref) <> P.semi
            stmt = ("let" <+> P.text resultVar) <> P.semi
              $$ ("try" <+> P.braces (P.nest 2 (aDecl $$ assign aRef)))
              $$ ("catch" <+> P.parens (P.text ('n' : show catchN))
                    <+> P.braces (P.nest 2 (bDecl $$ assign bRef)))
         in (s4, Code stmt (P.text resultVar))
  Bind x f -> bindEffectCode s0 x f
  UnsafeObject obj -> (s0, Code mempty $ P.text $ T.unpack obj)
  UnsafeObjectGet x string ->
    let (s1, Code x1Decl x1Ref) = effectfulAST' s0 x
    in (s1, Code x1Decl $ x1Ref <> "." <> P.text string)
  UnsafeObjectAssign x y ->
    let (s1, Code x1Decl x1Ref) = effectfulAST' s0 x
        (s2, Code y1Decl y1Ref) = effectfulAST' s1 y
    in (s2, Code (x1Decl $$ y1Decl) $ x1Ref <> " = " <> y1Ref )
  CallMethod recv name args ->
    let (s1, Code rDecl rRef) = effectfulAST' s0 recv
        (s2, argDecl, argRefs) = renderArgList argAST s1 args
     in (s2, Code (rDecl $$ argDecl) (rRef <> "." <> P.text name <> P.parens argRefs))
  LambdaE f ->
    let (nParam, s1) = allocIdent s0
        (s2, Code exprXDecl exprXRef) = effectfulAST' s1 (f (Const nParam))
     in ( s2
        , Code mempty
            $ "function"
            <+> P.parens (P.text $ 'n':show nParam)
            <+> P.braces ( (exprXDecl $$ "return" <+> exprXRef) )
        )
  ApplyE fex ex ->
    let (s1, Code exprXDecl exprXRef) = effectfulAST' s0 fex
        (s2, Code exprYDecl exprYRef) = effectfulAST' s1 ex
     in (s2, Code (exprXDecl $$ exprYDecl) (jsCall exprXRef exprYRef))

pureAST :: ClosedExpr u -> Doc
pureAST e = renderCode . snd . pureAST' startCG $ optimize e

pureAST' :: forall v. CG -> Expr (Const Int) v
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
    ValueString s -> (s0, Code mempty $ P.doubleQuotes (P.text $ T.unpack s))
    ValueFunction _f -> undefined
    ValueUnit -> (s0, mempty)
    ValueOption (Just x) -> pureAST' s0 (Literal x)
    ValueOption Nothing -> (s0, Code mempty "null")
    ValueBool True -> (s0, Code mempty "true")
    ValueBool False -> (s0, Code mempty "false")
  Concat x y -> renderBin "+" s0 x y
  Plus x y -> renderBin "+" s0 x y
  Minus x y -> renderBin "-" s0 x y
  Times x y -> renderBin "*" s0 x y
  FracDiv x y -> renderBin "/" s0 x y
  Show x ->
    let (s1, Code x1Decl x1Ref) = pureAST' s0 x
     in (s1, Code x1Decl $ "String" <> P.parens x1Ref)
  TypeOf x ->
    let (s1, Code x1Decl x1Ref) = pureAST' s0 x
     in (s1, Code x1Decl $ "typeof" <+> x1Ref)
  Negate x ->
    let (s1, Code x1Decl x1Ref) = pureAST' s0 x
     in (s1, Code x1Decl $ "-" <> P.parens x1Ref)
  Lambda f ->
    let ident0 = cgIdent s0
        ex = f (Const ident0)
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
  Let x g ->
    let (tag, sTag) = allocTag s0
        tagged = g (Const tag)
        uses = countExpr tag tagged
     in case uses of
          1 -> pureAST' sTag (substExpr tag x tagged)
          0 ->
            let (s1, Code xDecl xRef) = pureAST' sTag x
                (s2, Code yDecl yRef) = pureAST' s1 (g nestedDummy)
                stmt
                  | P.isEmpty xDecl && not (P.isEmpty xRef) = xRef <> P.semi
                  | otherwise = xDecl
             in (s2, Code (stmt $$ yDecl) yRef)
          _ ->
            let (s1, Code xDecl xRef) = pureAST' sTag x
                (nBind, s2) = allocIdent s1
                (s3, Code yDecl yRef) = pureAST' s2 (g (Const nBind))
             in (s3, Code (xDecl $$ constBind nBind xRef $$ yDecl) yRef)
  Apply fex ex ->
    let (s1, Code exprXDecl exprXRef) = pureAST' s0 fex
        (s2, Code exprYDecl exprYRef) = pureAST' s1 ex
     in (s2, Code (exprXDecl $$ exprYDecl) (jsCall exprXRef exprYRef))
  Var (Const x)
    -- Tags and the unused-binder dummy are negative; never emit them as JS.
    | x < 0 -> (s0, Code mempty mempty)
    | otherwise -> (s0, Code mempty $ P.text ('n':show x))
  If c t e ->
    let (s1, Code cDecl cRef) = pureAST' s0 c
        (s2, Code tDecl tRef) = pureAST' s1 t
        (s3, Code eDecl eRef) = pureAST' s2 e
     in (s3, Code (cDecl $$ tDecl $$ eDecl) (P.parens (cRef <+> "?" <+> tRef <+> ":" <+> eRef)))
  OptionCase opt none' someF ->
    case opt of
      Var (Const i) ->
        let optVar = 'n' : show i
            (s2, Code noneDecl noneRef) = pureAST' s0 none'
            (s3, Code someDecl someRef) = pureAST' s2 (someF (Const i))
         in ( s3
            , Code (noneDecl $$ someDecl)
                (P.parens (P.text optVar <+> "===" <+> "null" <+> "?" <+> noneRef <+> ":" <+> someRef))
            )
      _ ->
        let (s1, Code optDecl optRef) = pureAST' s0 opt
            (nBind, s2) = allocIdent s1
            optVar = 'n' : show nBind
            (s3, Code noneDecl noneRef) = pureAST' s2 none'
            (s4, Code someDecl someRef) = pureAST' s3 (someF (Const nBind))
         in ( s4
            , Code (optDecl $$ constBind nBind optRef $$ noneDecl $$ someDecl)
                (P.parens (P.text optVar <+> "===" <+> "null" <+> "?" <+> noneRef <+> ":" <+> someRef))
            )
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

stdTernaryJS :: StdTernary a b c d -> Doc -> Doc -> Doc -> Doc
stdTernaryJS n r a b = case n of
  StdSlice -> r <> ".slice" <> P.parens (a <> ", " <> b)
  StdReplace -> r <> ".replace" <> P.parens (a <> ", " <> b)

renderCallbackMethod ::
     String
  -> CG
  -> Expr (Const Int) a
  -> (Const Int b -> Expr (Const Int) c)
  -> (CG, Code)
renderCallbackMethod name s0 recv f =
  let (s1, Code rDecl rRef) = pureAST' s0 recv
      (nParam, s2) = allocIdent s1
      ex = f (Const nParam)
      (s3, Code exDecl exRef) = pureAST' s2 ex
      paramName = 'n' : show nParam
      callback = "function" <+> P.parens (P.text paramName) <+> P.braces (exDecl $$ "return" <+> exRef)
      call = rRef <> "." <> P.text name <> P.parens callback
   in (s3, Code rDecl call)

renderBin :: String -> CG -> Expr (Const Int) a -> Expr (Const Int) b -> (CG, Code)
renderBin op s0 x y =
  let (s1, Code xDecl xRef) = pureAST' s0 x
      (s2, Code yDecl yRef) = pureAST' s1 y
   in ( s2
      , Code (xDecl $$ yDecl)
          (wrapOperand x xRef <+> P.text op <+> wrapOperand y yRef)
      )

argAST :: CG -> Arg (Const Int) u -> (CG, Code)
argAST s (ArgExpr e) = pureAST' s e
argAST s (ArgEffect e) = effectfulAST' s e

renderArgList :: (forall x. CG -> f x -> (CG, Code)) -> CG -> Rec f us -> (CG, Doc, Doc)
renderArgList f s0 args =
  let (s1, cs) = recCodes f s0 args
      (decls, refs) = partitionCode cs
   in (s1, P.vcat decls, P.hcat (P.punctuate ", " refs))

