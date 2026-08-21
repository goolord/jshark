{-# LANGUAGE
    BangPatterns
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
  ( Expr(..)
  , Value(..)
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

-- This uses a higher-order PHOAS approach as described by
-- https://www.reddit.com/r/haskell/comments/85een6/sharing_from_phoas_multiple_interpreters_from_free/dvxhlba

import Data.Functor.Const (Const(..))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Kind
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
valueEq (ValueResult a) (ValueResult b) = case (a, b) of
  (Left x, Left y) -> valueEq x y
  (Right x, Right y) -> valueEq x y
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
jsShow (ValueResult (Left x)) = "true," <> jsShow x
jsShow (ValueResult (Right x)) = "false," <> jsShow x
jsShow (ValueFunction _) = error "evaluate: cannot show a function"

jsShowNumber :: Double -> String
jsShowNumber d
  | isInt = show (truncate d :: Integer)
  | otherwise = show d
  where
  isInt = not (isNaN d) && not (isInfinite d) && d == fromInteger (truncate d)

isFiniteDouble :: Double -> Bool
isFiniteDouble d = not (isNaN d) && not (isInfinite d)

-- | Implements the unary @Math@ functions supported by 'MathUnary'.
mathUnaryOp :: Text -> Maybe (Double -> Double)
mathUnaryOp = \case
  "sin" -> Just sin
  "cos" -> Just cos
  "tan" -> Just tan
  "asin" -> Just asin
  "acos" -> Just acos
  "atan" -> Just atan
  "sqrt" -> Just sqrt
  "cbrt" -> Just (\x -> signum x * (abs x ** (1 / 3)))
  "exp" -> Just exp
  "log" -> Just log
  "log2" -> Just (logBase 2)
  "log10" -> Just (logBase 10)
  "floor" -> Just (fromIntegral . (floor :: Double -> Integer))
  "ceil" -> Just (fromIntegral . (ceiling :: Double -> Integer))
  -- JS's Math.round rounds half-way values toward +Infinity (e.g.
  -- Math.round(2.5) === 3, Math.round(-2.5) === -2), unlike Haskell's
  -- 'round' (banker's rounding to even: round 2.5 == 2). floor(x + 0.5)
  -- matches JS's semantics.
  "round" -> Just (fromIntegral . (floor :: Double -> Integer) . (+ 0.5))
  "trunc" -> Just (fromIntegral . (truncate :: Double -> Integer))
  _ -> Nothing

mathUnaryFn :: Text -> Double -> Double
mathUnaryFn name = case mathUnaryOp name of
  Just f -> f
  Nothing -> error ("evaluate: unknown Math unary function " ++ T.unpack name)

-- | Implements the binary @Math@ functions supported by 'MathBinary'.
mathBinaryOp :: Text -> Maybe (Double -> Double -> Double)
mathBinaryOp = \case
  "pow" -> Just (**)
  "atan2" -> Just atan2
  "max" -> Just max
  "min" -> Just min
  "hypot" -> Just (\x y -> sqrt (x * x + y * y))
  _ -> Nothing

mathBinaryFn :: Text -> Double -> Double -> Double
mathBinaryFn name = case mathBinaryOp name of
  Just f -> f
  Nothing -> error ("evaluate: unknown Math binary function " ++ T.unpack name)

-- | Fold @Math.*@ only when the Haskell result is known to match JS.
-- Transcendentals (@sin(1)@, @cbrt@, @pow@, …) stay in JS.
exactMathUnary :: Text -> Double -> Maybe Double
exactMathUnary n a = case n of
  "sin" | a == 0 -> Just 0
  "cos" | a == 0 -> Just 1
  "tan" | a == 0 -> Just 0
  "sqrt" | a >= 0, let r = sqrt a, r * r == a -> Just r
  "floor" | isFiniteDouble a -> Just (fromIntegral (floor a :: Integer))
  "ceil" | isFiniteDouble a -> Just (fromIntegral (ceiling a :: Integer))
  "round" | isFiniteDouble a -> Just (fromIntegral (floor (a + 0.5) :: Integer))
  "trunc" | isFiniteDouble a -> Just (fromIntegral (truncate a :: Integer))
  _ -> Nothing

exactMathBinary :: Text -> Double -> Double -> Maybe Double
exactMathBinary _ _ _ = Nothing

isOrderableValue :: Value u -> Bool
isOrderableValue = \case
  ValueNumber{} -> True
  ValueString{} -> True
  ValueBool{} -> True
  _ -> False

eqFoldableValue :: Value u -> Bool
eqFoldableValue ValueFunction{} = False
eqFoldableValue _ = True

-- | Rewrite packed option/result literals to the same constructors the
-- rest of the pass matches on.
optLiteral :: Value u -> Expr (Const Int) u
optLiteral = \case
  ValueOption Nothing -> None
  ValueOption (Just v) -> Some (optLiteral v)
  ValueResult (Left v) -> Ok (optLiteral v)
  ValueResult (Right v) -> Err (optLiteral v)
  v -> Literal v

peelOption :: Expr (Const Int) ('Option u) -> Maybe (Maybe (Expr (Const Int) u))
peelOption = \case
  None -> Just Nothing
  Some x -> Just (Just x)
  Literal (ValueOption Nothing) -> Just Nothing
  Literal (ValueOption (Just v)) -> Just (Just (Literal v))
  _ -> Nothing

peelResult :: Expr (Const Int) ('Result u v) -> Maybe (Either (Expr (Const Int) u) (Expr (Const Int) v))
peelResult = \case
  Ok x -> Just (Left x)
  Err x -> Just (Right x)
  Literal (ValueResult (Left v)) -> Just (Left (Literal v))
  Literal (ValueResult (Right v)) -> Just (Right (Literal v))
  _ -> Nothing

evaluateNumber :: (forall (f :: Universe -> Type). Expr f 'Number) -> Double
evaluateNumber e = unNumber (evaluate e)

-- | Pure reference interpreter. Shared Haskell heap nodes are walked
-- once per occurrence (no memo table). Use 'evaluateCached' when host-level
-- sharing should be observed.
evaluate :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Value u
evaluate e0 = eval e0 where
  eval :: forall v. Expr Value v -> Value v
  eval = \case
    Literal v -> v
    Plus x y -> ValueNumber (unNumber (eval x) + unNumber (eval y))
    Times x y -> ValueNumber (unNumber (eval x) * unNumber (eval y))
    Minus x y -> ValueNumber (unNumber (eval x) - unNumber (eval y))
    Abs x -> ValueNumber (abs (unNumber (eval x)))
    Sign x -> ValueNumber (signum (unNumber (eval x)))
    Negate x -> ValueNumber (negate (unNumber (eval x)))
    FracDiv x y -> ValueNumber (unNumber (eval x) / unNumber (eval y))
    Var x -> x
    Apply g x -> unFunction (eval g) (eval x)
    Lambda g -> ValueFunction (eval . g)
    Concat x y -> ValueString (unString (eval x) <> unString (eval y))
    Show x -> ValueString (jsShow (eval x))
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
    Some x -> ValueOption (Just (eval x))
    None -> ValueOption Nothing
    OptionCase opt none' someF -> case eval opt of
      ValueOption Nothing -> eval none'
      ValueOption (Just x) -> eval (someF x)
    Ok x -> ValueResult (Left (eval x))
    Err y -> ValueResult (Right (eval y))
    ResultCase r okF errF -> case eval r of
      ValueResult (Left x) -> eval (okF x)
      ValueResult (Right y) -> eval (errF y)
    UnsafeEffectExpr _ ->
      error "evaluate: cannot evaluate an embedded Effect (UnsafeEffectExpr)"
    ExprFFI name _ ->
      error ("evaluate: cannot evaluate a foreign function call: " ++ T.unpack name)
    ExprProp _ name ->
      error ("evaluate: cannot evaluate a foreign property access: " ++ T.unpack name)
    ExprMethod _ name _ ->
      error ("evaluate: cannot evaluate a foreign method call: " ++ T.unpack name)
    ExprMethodCallback _ name _ ->
      error ("evaluate: cannot evaluate a foreign method call: " ++ T.unpack name)
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
    UnsafeNullable _ ->
      error "evaluate: cannot evaluate UnsafeNullable (an FFI-derived Option)"

-- Per-evaluation memo table keyed by 'StableName'. Recovers host-language
-- sharing (Haskell @let x = e in x + x@) so a shared 'Expr' node is only
-- interpreted once. Object-language 'Let' already preserves sharing on its
-- own; this cache is what makes the two coincide.
type EvalCache = IORef (IntMap [(StableName (), Any)])

-- | Like 'evaluate', but memoizes shared heap nodes via 'StableName'.
-- In 'IO' because observable sharing is inherently effectful.
evaluateCached :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> IO (Value u)
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
  Abs x -> num1 abs x
  Sign x -> num1 signum x
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
  Some x -> ValueOption . Just <$> go cache x
  None -> pure (ValueOption Nothing)
  OptionCase opt none' someF -> do
    ov <- go cache opt
    case ov of
      ValueOption Nothing -> go cache none'
      ValueOption (Just x) -> go cache (someF x)
  Ok x -> ValueResult . Left <$> go cache x
  Err y -> ValueResult . Right <$> go cache y
  ResultCase r okF errF -> do
    rv <- go cache r
    case rv of
      ValueResult (Left x) -> go cache (okF x)
      ValueResult (Right y) -> go cache (errF y)
  UnsafeEffectExpr _ ->
    error "evaluate: cannot evaluate an embedded Effect (UnsafeEffectExpr)"
  ExprFFI name _ ->
    error ("evaluate: cannot evaluate a foreign function call: " ++ T.unpack name)
  ExprProp _ name ->
    error ("evaluate: cannot evaluate a foreign property access: " ++ T.unpack name)
  ExprMethod _ name _ ->
    error ("evaluate: cannot evaluate a foreign method call: " ++ T.unpack name)
  ExprMethodCallback _ name _ ->
    error ("evaluate: cannot evaluate a foreign method call: " ++ T.unpack name)
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
  UnsafeNullable _ ->
    error "evaluate: cannot evaluate UnsafeNullable (an FFI-derived Option)"
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

renderCode :: Code -> Doc
renderCode (Code a b) = a $$ b

-- | Wrap generated decls + result in an IIFE so a minifier treats the
-- result as live (plain expression statements get DCE'd).
renderIIFE :: Code -> Doc
renderIIFE (Code decls ref) =
  let body = if P.isEmpty ref then decls else decls $$ (("return" <+> ref) <> P.semi)
   in "(() => {" $$ P.nest 2 body $$ "})()"

-- | Pure expression compiled to a self-contained JS program (IIFE).
pureProgram :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Doc
pureProgram e = renderIIFE . snd . pureAST' startCG $ optimize e

-- | Effectful computation compiled to a self-contained JS program (IIFE).
effectfulProgram :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Effect f u)
  -> Doc
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
  None -> True
  Some x -> isSimple x
  Abs{} -> True
  Sign{} -> True
  Show{} -> True
  Negate{} -> True
  ExprFFI{} -> True
  ExprProp{} -> True
  ExprMethod{} -> True
  ExprMethodCallback{} -> True
  ExprIndex{} -> True
  MathUnary{} -> True
  MathBinary{} -> True
  UnsafeNullable x -> isSimple x
  _ -> False

wrapOperand :: Expr (Const Int) u -> Doc -> Doc
wrapOperand e d = if isSimple e then d else P.parens d

-- A use under a lambda, loop, `&&`/`||` RHS, or `?:` branch is not a
-- candidate for inlining: the binder would be re-run or skipped.
countLazyExpr :: Int -> Expr (Const Int) u -> Int
countLazyExpr t e = if countExpr t e == 0 then 0 else 2

countLazyEffect :: Int -> Effect (Const Int) u -> Int
countLazyEffect t e = if countEffect t e == 0 then 0 else 2

countRec :: Int -> Rec (Expr (Const Int)) us -> Int
countRec _ RecNil = 0
countRec t (RecCons x xs) = countExpr t x + countRec t xs

countExpr :: Int -> Expr (Const Int) u -> Int
countExpr t = \case
  Literal{} -> 0
  Var (Const i) -> if i == t then 1 else 0
  Concat x y -> countExpr t x + countExpr t y
  Plus x y -> countExpr t x + countExpr t y
  Times x y -> countExpr t x + countExpr t y
  Minus x y -> countExpr t x + countExpr t y
  Abs x -> countExpr t x
  Sign x -> countExpr t x
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
  If c u v -> countExpr t c + countLazyExpr t u + countLazyExpr t v
  Some x -> countExpr t x
  None -> 0
  OptionCase o n s ->
    countExpr t o + countLazyExpr t n + countLazyExpr t (s nestedDummy)
  Ok x -> countExpr t x
  Err x -> countExpr t x
  ResultCase r okF errF ->
    countExpr t r
      + countLazyExpr t (okF nestedDummy)
      + countLazyExpr t (errF nestedDummy)
  UnsafeEffectExpr e -> countEffect t e
  ExprFFI _ args -> countRec t args
  ExprProp x _ -> countExpr t x
  ExprMethod x _ args -> countExpr t x + countRec t args
  ExprMethodCallback x _ f -> countExpr t x + countLazyExpr t (f nestedDummy)
  ExprIndex x i -> countExpr t x + countExpr t i
  MathUnary _ x -> countExpr t x
  MathBinary _ x y -> countExpr t x + countExpr t y
  UnsafeNullable x -> countExpr t x

countEffect :: Int -> Effect (Const Int) u -> Int
countEffect t = \case
  Lift x -> countExpr t x
  FFI _ args -> countRec t args
  UnsafeObject _ -> 0
  UnsafeObjectGet x _ -> countEffect t x
  UnsafeObjectAssign x y -> countEffect t x + countEffect t y
  ObjectFFI x y -> countEffect t x + countEffect t y
  ForEach xs f -> countExpr t xs + countLazyEffect t (f nestedDummy)
  Bind x f -> countEffect t x + countEffect t (f nestedDummy)
  UnEffectful x -> countExpr t x
  LambdaE f -> countLazyEffect t (f nestedDummy)
  ApplyE f x -> countEffect t f + countEffect t x
  IfE c u v -> countExpr t c + countLazyEffect t u + countLazyEffect t v
  While c b -> countLazyExpr t c + countLazyEffect t b

substRec :: Int -> Expr (Const Int) u -> Rec (Expr (Const Int)) us -> Rec (Expr (Const Int)) us
substRec _ _ RecNil = RecNil
substRec t r (RecCons x xs) = RecCons (substExpr t r x) (substRec t r xs)

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
      Abs x -> Abs (goExpr x)
      Sign x -> Sign (goExpr x)
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
      If c u v -> If (goExpr c) (goExpr u) (goExpr v)
      Some x -> Some (goExpr x)
      None -> None
      OptionCase o n s -> OptionCase (goExpr o) (goExpr n) (goExpr . s)
      Ok x -> Ok (goExpr x)
      Err x -> Err (goExpr x)
      ResultCase e okF errF -> ResultCase (goExpr e) (goExpr . okF) (goExpr . errF)
      UnsafeEffectExpr e -> UnsafeEffectExpr (substEffect t r e)
      ExprFFI n args -> ExprFFI n (substRec t r args)
      ExprProp x n -> ExprProp (goExpr x) n
      ExprMethod x n args -> ExprMethod (goExpr x) n (substRec t r args)
      ExprMethodCallback x n f -> ExprMethodCallback (goExpr x) n (goExpr . f)
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
      FFI n args -> FFI n (substRec t r args)
      UnsafeObject o -> UnsafeObject o
      UnsafeObjectGet x s -> UnsafeObjectGet (goE x) s
      UnsafeObjectAssign x y -> UnsafeObjectAssign (goE x) (goE y)
      ObjectFFI x y -> ObjectFFI (goE x) (goE y)
      ForEach xs f -> ForEach (substExpr t r xs) (goE . f)
      Bind x f -> Bind (goE x) (goE . f)
      UnEffectful x -> UnEffectful (substExpr t r x)
      LambdaE f -> LambdaE (goE . f)
      ApplyE f x -> ApplyE (goE f) (goE x)
      IfE c u v -> IfE (substExpr t r c) (goE u) (goE v)
      While c b -> While (substExpr t r c) (goE b)

-- | Constant-fold and drop dead pure bindings. Applied automatically by
-- codegen. Literals are propagated even under lambdas; effectful or
-- non-cheap bindings follow the same strict-use rule as inlining.
optimize :: (forall (f :: Universe -> Type). Expr f u) -> Expr (Const Int) u
optimize e = snd (optExpr (-2) e)

optimizeEffect :: (forall (f :: Universe -> Type). Effect f u) -> Effect (Const Int) u
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
  ValueResult (Left v) -> isCheapValue v
  ValueResult (Right v) -> isCheapValue v
  ValueArray{} -> False
  ValueFunction{} -> False

isCheap :: Expr (Const Int) u -> Bool
isCheap = \case
  Literal v -> isCheapValue v
  None -> True
  Some x -> isCheap x
  Ok x -> isCheap x
  Err x -> isCheap x
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
  None -> True
  Concat x y -> isPureExpr x && isPureExpr y
  Plus x y -> isPureExpr x && isPureExpr y
  Times x y -> isPureExpr x && isPureExpr y
  Minus x y -> isPureExpr x && isPureExpr y
  Abs x -> isPureExpr x
  Sign x -> isPureExpr x
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
  If c t e -> isPureExpr c && isPureExpr t && isPureExpr e
  Some x -> isPureExpr x
  OptionCase o n s -> isPureExpr o && isPureExpr n && isPureExpr (s nestedDummy)
  Ok x -> isPureExpr x
  Err x -> isPureExpr x
  ResultCase r okF errF ->
    isPureExpr r && isPureExpr (okF nestedDummy) && isPureExpr (errF nestedDummy)
  UnsafeEffectExpr _ -> False
  ExprFFI{} -> False
  ExprProp{} -> False
  ExprMethod{} -> False
  ExprMethodCallback{} -> False
  ExprIndex x i -> isPureExpr x && isPureExpr i
  MathUnary _ x -> isPureExpr x
  MathBinary _ x y -> isPureExpr x && isPureExpr y
  UnsafeNullable x -> isPureExpr x

isPureEffect :: Effect (Const Int) u -> Bool
isPureEffect = \case
  Lift x -> isPureExpr x
  FFI{} -> False
  UnsafeObject{} -> True
  UnsafeObjectGet{} -> False
  UnsafeObjectAssign{} -> False
  ObjectFFI{} -> False
  ForEach{} -> False
  Bind x f -> isPureEffect x && isPureEffect (f nestedDummy)
  UnEffectful{} -> False
  LambdaE f -> isPureEffect (f nestedDummy)
  ApplyE{} -> False
  IfE c t e -> isPureExpr c && isPureEffect t && isPureEffect e
  While{} -> False

optRec :: Int -> Rec (Expr (Const Int)) us -> (Int, Rec (Expr (Const Int)) us)
optRec t RecNil = (t, RecNil)
optRec t (RecCons x xs) =
  let (t1, x') = optExpr t x
      (t2, xs') = optRec t1 xs
   in (t2, RecCons x' xs')

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

foldIndex :: Expr (Const Int) ('Array u) -> Expr (Const Int) 'Number -> Expr (Const Int) u
foldIndex arr idx = case (arr, idx) of
  (Literal (ValueArray vs), Literal (ValueNumber d))
    | let i = truncate d :: Int
    , i >= 0 && i < length vs -> Literal (vs !! i)
  _ -> ExprIndex arr idx

foldMathUnary :: Text -> Expr (Const Int) 'Number -> Expr (Const Int) 'Number
foldMathUnary n x = case x of
  Literal (ValueNumber a)
    | Just r <- exactMathUnary n a -> Literal (ValueNumber r)
  _ -> MathUnary n x

foldMathBinary :: Text -> Expr (Const Int) 'Number -> Expr (Const Int) 'Number -> Expr (Const Int) 'Number
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
  Literal v -> (t0, optLiteral v)
  Var v -> (t0, Var v)
  None -> (t0, None)
  Concat x y ->
    let (t1, x') = optExpr t0 x
        (t2, y') = optExpr t1 y
     in (t2, foldConcat x' y')
  Plus x y -> binNum (+) Plus x y
  Times x y -> binNum (*) Times x y
  Minus x y -> binNum (-) Minus x y
  FracDiv x y -> binNum (/) FracDiv x y
  Abs x -> unNum abs Abs x
  Sign x -> unNum signum Sign x
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
  If c t e ->
    let (t1, c') = optExpr t0 c
     in case c' of
          Literal (ValueBool True) -> optExpr t1 t
          Literal (ValueBool False) -> optExpr t1 e
          _ ->
            let (t2, t') = optExpr t1 t
                (t3, e') = optExpr t2 e
             in (t3, If c' t' e')
  Some x -> fmap Some (optExpr t0 x)
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
  Ok x -> fmap Ok (optExpr t0 x)
  Err x -> fmap Err (optExpr t0 x)
  ResultCase r okF errF ->
    let (t1, r') = optExpr t0 r
     in case peelResult r' of
          Just (Left x) ->
            let (t2, tagOk, okBody) = optUnder t1 okF
             in elimLetFrom t2 x tagOk okBody
          Just (Right x) ->
            let (t2, tagErr, errBody) = optUnder t1 errF
             in elimLetFrom t2 x tagErr errBody
          Nothing ->
            let (t2, tagOk, okBody) = optUnder t1 okF
                (t3, tagErr, errBody) = optUnder t2 errF
             in (t3, ResultCase r' (rebind tagOk okBody) (rebind tagErr errBody))
  UnsafeEffectExpr e ->
    let (t1, e') = optEffect t0 e
     in case e' of
          Lift x -> (t1, x)
          _ -> (t1, UnsafeEffectExpr e')
  ExprFFI n args -> fmap (ExprFFI n) (optRec t0 args)
  ExprProp x n ->
    let (t1, x') = optExpr t0 x
     in (t1, ExprProp x' n)
  ExprMethod x n args ->
    let (t1, x') = optExpr t0 x
        (t2, args') = optRec t1 args
     in (t2, ExprMethod x' n args')
  ExprMethodCallback x n f ->
    let (t1, x') = optExpr t0 x
        (t2, tag, body) = optUnder t1 f
     in (t2, ExprMethodCallback x' n (rebind tag body))
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
  FFI n args -> fmap (FFI n) (optRec t0 args)
  UnsafeObject o -> (t0, UnsafeObject o)
  UnsafeObjectGet x s ->
    let (t1, x') = optEffect t0 x
     in (t1, UnsafeObjectGet x' s)
  UnsafeObjectAssign x y ->
    let (t1, x') = optEffect t0 x
        (t2, y') = optEffect t1 y
     in (t2, UnsafeObjectAssign x' y')
  ObjectFFI x y ->
    let (t1, x') = optEffect t0 x
        (t2, y') = optEffect t1 y
     in (t2, ObjectFFI x' y')
  ForEach xs f ->
    let (t1, xs') = optExpr t0 xs
        (t2, tag, body) = optUnderE t1 f
     in (t2, ForEach xs' (rebindE tag body))
  Bind x f -> optBind t0 x f
  UnEffectful x -> fmap UnEffectful (optExpr t0 x)
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
    let (t1, c') = optExpr t0 c
     in case c' of
          Literal (ValueBool True) -> optEffect t1 t
          Literal (ValueBool False) -> optEffect t1 e
          _ ->
            let (t2, t') = optEffect t1 t
                (t3, e') = optEffect t2 e
             in (t3, IfE c' t' e')
  While c b ->
    let (t1, c') = optExpr t0 c
     in case c' of
          Literal (ValueBool False) -> (t1, Lift (Literal ValueUnit))
          _ ->
            let (t2, b') = optEffect t1 b
             in (t2, While c' b')

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

foldRec :: Int -> Rec (Expr (Const Int)) us -> (Int, Rec (Expr (Const Int)) us)
foldRec t RecNil = (t, RecNil)
foldRec t (RecCons x xs) =
  let (t1, x') = foldExpr t x
      (t2, xs') = foldRec t1 xs
   in (t2, RecCons x' xs')

foldExpr :: Int -> Expr (Const Int) u -> (Int, Expr (Const Int) u)
foldExpr t0 = \case
  Literal v -> (t0, optLiteral v)
  Var v -> (t0, Var v)
  None -> (t0, None)
  Concat x y ->
    let (t1, x') = foldExpr t0 x
        (t2, y') = foldExpr t1 y
     in (t2, foldConcat x' y')
  Plus x y -> bin (+) Plus x y
  Times x y -> bin (*) Times x y
  Minus x y -> bin (-) Minus x y
  FracDiv x y -> bin (/) FracDiv x y
  Abs x -> un abs Abs x
  Sign x -> un signum Sign x
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
  If c t e ->
    let (t1, c') = foldExpr t0 c
     in case c' of
          Literal (ValueBool True) -> foldExpr t1 t
          Literal (ValueBool False) -> foldExpr t1 e
          _ ->
            let (t2, t') = foldExpr t1 t
                (t3, e') = foldExpr t2 e
             in (t3, If c' t' e')
  Some x -> fmap Some (foldExpr t0 x)
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
  Ok x -> fmap Ok (foldExpr t0 x)
  Err x -> fmap Err (foldExpr t0 x)
  ResultCase r okF errF ->
    let (t1, r') = foldExpr t0 r
     in case peelResult r' of
          Just (Left x) ->
            let (t2, tagOk, okBody) = foldUnder t1 okF
             in elimLetFrom t2 x tagOk okBody
          Just (Right x) ->
            let (t2, tagErr, errBody) = foldUnder t1 errF
             in elimLetFrom t2 x tagErr errBody
          Nothing ->
            let (t2, tagOk, okBody) = foldUnder t1 okF
                (t3, tagErr, errBody) = foldUnder t2 errF
             in (t3, ResultCase r' (rebind tagOk okBody) (rebind tagErr errBody))
  UnsafeEffectExpr e ->
    let (t1, e') = foldEffect t0 e
     in case e' of
          Lift x -> (t1, x)
          _ -> (t1, UnsafeEffectExpr e')
  ExprFFI n args -> fmap (ExprFFI n) (foldRec t0 args)
  ExprProp x n ->
    let (t1, x') = foldExpr t0 x
     in (t1, ExprProp x' n)
  ExprMethod x n args ->
    let (t1, x') = foldExpr t0 x
        (t2, args') = foldRec t1 args
     in (t2, ExprMethod x' n args')
  ExprMethodCallback x n f ->
    let (t1, x') = foldExpr t0 x
        (t2, tag, body) = foldUnder t1 f
     in (t2, ExprMethodCallback x' n (rebind tag body))
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
  FFI n args -> fmap (FFI n) (foldRec t0 args)
  UnsafeObject o -> (t0, UnsafeObject o)
  UnsafeObjectGet x s ->
    let (t1, x') = foldEffect t0 x
     in (t1, UnsafeObjectGet x' s)
  UnsafeObjectAssign x y ->
    let (t1, x') = foldEffect t0 x
        (t2, y') = foldEffect t1 y
     in (t2, UnsafeObjectAssign x' y')
  ObjectFFI x y ->
    let (t1, x') = foldEffect t0 x
        (t2, y') = foldEffect t1 y
     in (t2, ObjectFFI x' y')
  ForEach xs f ->
    let (t1, xs') = foldExpr t0 xs
        (t2, tag, body) = foldUnderE t1 f
     in (t2, ForEach xs' (rebindE tag body))
  Bind x f ->
    let (t1, x') = foldEffect t0 x
        (t2, tag, body) = foldUnderE t1 f
     in (t2, Bind x' (rebindE tag body))
  UnEffectful x -> fmap UnEffectful (foldExpr t0 x)
  LambdaE f ->
    let (t1, tag, body) = foldUnderE t0 f
     in (t1, LambdaE (rebindE tag body))
  ApplyE f x ->
    let (t1, f') = foldEffect t0 f
        (t2, x') = foldEffect t1 x
     in (t2, ApplyE f' x')
  IfE c t e ->
    let (t1, c') = foldExpr t0 c
     in case c' of
          Literal (ValueBool True) -> foldEffect t1 t
          Literal (ValueBool False) -> foldEffect t1 e
          _ ->
            let (t2, t') = foldEffect t1 t
                (t3, e') = foldEffect t2 e
             in (t3, IfE c' t' e')
  While c b ->
    let (t1, c') = foldExpr t0 c
     in case c' of
          Literal (ValueBool False) -> (t1, Lift (Literal ValueUnit))
          _ ->
            let (t2, b') = foldEffect t1 b
             in (t2, While c' b')

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


effectfulAST :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Effect f u)
  -> Doc
effectfulAST e = renderCode . snd . effectfulAST' startCG $ optimizeEffect e

effectfulAST' :: forall v. CG -> Effect (Const Int) v -> (CG, Code)
effectfulAST' !s0 = \case
  Lift x -> pureAST' s0 x
  FFI fn args ->
    let foo :: CG -> Rec (Expr (Const Int)) u' -> (CG, [Code])
        foo s'0 (RecCons x xs) =
          let (s'1, x') = pureAST' s'0 x
              (s'2, cs) = foo s'1 xs
           in (s'2, x' : cs)
        foo s' RecNil = (s', [])
        (s1, lArgs') = foo s0 args
        (lVars, lArgs) = partitionCode lArgs'
        foreignFunction = P.text fn <> P.parens (P.hcat (P.punctuate ", " lArgs))
     in (s1, Code (P.vcat lVars) foreignFunction)
  ForEach xs f ->
    let (s1, Code xsDecl xsRef) = pureAST' s0 xs
        (nParam, s2) = allocIdent s1
        (s3, Code asDecl asRef) = effectfulAST' s2 (f (Const nParam))
        bodyStmt = if P.isEmpty asRef then asDecl else asDecl $$ (asRef <> P.semi)
        forE = xsRef <> ".forEach" <> (P.parens
               $ "function" <> P.parens (P.text ('n':show nParam))
               <> P.braces (P.nest 2 bodyStmt)) <> P.semi
     in (s3, Code xsDecl forE)
  IfE c t e ->
    -- We always render this as an if/else statement (rather than trying to
    -- special-case a ternary expression) because an effectful branch's
    -- rendered "ref" may be a leftover 'Unit' placeholder rather than a
    -- genuinely-empty Doc, which made an emptiness-based heuristic unsound
    -- (it could produce a ternary with an empty branch, e.g. `c ? x : `).
    -- Using a shared `let`-bound result variable, assigned in both
    -- branches, is correct regardless of whether the branches are 'Unit'
    -- or carry a real value.
    let (s1, Code cDecl cRef) = pureAST' s0 c
        (resultN, s2) = allocIdent s1
        resultVar = 'n' : show resultN
        (s3, Code tDecl tRef) = effectfulAST' s2 t
        (s4, Code eDecl eRef) = effectfulAST' s3 e
        assign ref = if P.isEmpty ref then mempty else (P.text resultVar <+> "=" <+> ref) <> P.semi
        ifStmt = ("let" <+> P.text resultVar) <> P.semi
          $$ ("if" <+> P.parens cRef <+> P.braces (P.nest 2 (tDecl $$ assign tRef)))
          $$ ("else" <+> P.braces (P.nest 2 (eDecl $$ assign eRef)))
     in (s4, Code (cDecl $$ ifStmt) (P.text resultVar))
  While cond body ->
    let (s1, Code condDecl condRef) = pureAST' s0 cond
        (s2, Code bodyDecl bodyRef) = effectfulAST' s1 body
        bodyStmt = if P.isEmpty bodyRef then bodyDecl else bodyDecl $$ (bodyRef <> P.semi)
        whileStmt = "while" <+> P.parens condRef <+> P.braces (P.nest 2 bodyStmt)
     in (s2, Code (condDecl $$ whileStmt) mempty)
  Bind x f -> bindEffectCode s0 x f
  UnsafeObject obj -> (s0, Code mempty $ P.text $ T.unpack obj)
  UnsafeObjectGet x string ->
    let (s1, Code x1Decl x1Ref) = effectfulAST' s0 x
    in (s1, Code x1Decl $ x1Ref <> "." <> P.text string)
  UnsafeObjectAssign x y ->
    let (s1, Code x1Decl x1Ref) = effectfulAST' s0 x
        (s2, Code y1Decl y1Ref) = effectfulAST' s1 y
    in (s2, Code (x1Decl $$ y1Decl) $ x1Ref <> " = " <> y1Ref )
  ObjectFFI x ffi ->
    let (s1, Code x1Decl x1Ref) = effectfulAST' s0 x
        (s2, Code ffi1Decl ffi1Ref) = effectfulAST' s1 ffi
    in (s2, Code (x1Decl $$ ffi1Decl) $ x1Ref <> "." <> ffi1Ref)
  UnEffectful x ->
    let (s1, Code a1Decl a1Ref) = pureAST' s0 x
     in (s1, Code a1Decl $ a1Ref <> P.parens mempty)
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

pureAST :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Doc
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
    ValueResult (Left x) ->
      let (s1, Code xDecl xRef) = pureAST' s0 (Literal x)
       in (s1, Code xDecl (P.brackets ("true" <> ", " <> xRef)))
    ValueResult (Right x) ->
      let (s1, Code xDecl xRef) = pureAST' s0 (Literal x)
       in (s1, Code xDecl (P.brackets ("false" <> ", " <> xRef)))
    ValueBool True -> (s0, Code mempty "true")
    ValueBool False -> (s0, Code mempty "false")
  Concat x y -> renderBin "+" s0 x y
  Plus x y -> renderBin "+" s0 x y
  Minus x y -> renderBin "-" s0 x y
  Times x y -> renderBin "*" s0 x y
  FracDiv x y -> renderBin "/" s0 x y
  Abs x ->
    let (s1, Code x1Decl x1Ref) = pureAST' s0 x
     in (s1, Code x1Decl $ "Math.abs" <> P.parens x1Ref)
  Sign x ->
    let (s1, Code x1Decl x1Ref) = pureAST' s0 x
     in (s1, Code x1Decl $ "Math.sign" <> P.parens x1Ref)
  Show x ->
    let (s1, Code x1Decl x1Ref) = pureAST' s0 x
     in (s1, Code x1Decl $ "String" <> P.parens x1Ref)
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
  Some x -> pureAST' s0 x
  None -> (s0, Code mempty "null")
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
  Ok x ->
    let (s1, Code xDecl xRef) = pureAST' s0 x
     in (s1, Code xDecl (P.brackets ("true" <> ", " <> xRef)))
  Err x ->
    let (s1, Code xDecl xRef) = pureAST' s0 x
     in (s1, Code xDecl (P.brackets ("false" <> ", " <> xRef)))
  ResultCase r okF errF ->
    let (s1, Code rDecl rRef) = pureAST' s0 r
        (nR, s2) = allocIdent s1
        rVar = 'n' : show nR
        constR = ("const" <+> P.text rVar <+> "=" <+> rRef) <> P.semi
        (nP, s3) = allocIdent s2
        payloadVar = 'n' : show nP
        constPayload = ("const" <+> P.text payloadVar <+> "=" <+> (P.text rVar <> P.brackets "1")) <> P.semi
        (s4, Code okDecl okRef) = pureAST' s3 (okF (Const nP))
        (s5, Code errDecl errRef) = pureAST' s4 (errF (Const nP))
     in ( s5
        , Code (rDecl $$ constR $$ constPayload $$ okDecl $$ errDecl)
            (P.parens ((P.text rVar <> P.brackets "0") <+> "?" <+> okRef <+> ":" <+> errRef))
        )
  UnsafeEffectExpr eff -> effectfulAST' s0 eff
  ExprFFI fn args ->
    let foo :: CG -> Rec (Expr (Const Int)) u' -> (CG, [Code])
        foo s'0 (RecCons x xs) =
          let (s'1, x') = pureAST' s'0 x
              (s'2, cs) = foo s'1 xs
           in (s'2, x' : cs)
        foo s' RecNil = (s', [])
        (s1, lArgs') = foo s0 args
        (lVars, lArgs) = partitionCode lArgs'
        call = P.text (T.unpack fn) <> P.parens (P.hcat (P.punctuate ", " lArgs))
     in (s1, Code (P.vcat lVars) call)
  ExprProp recv name ->
    let (s1, Code rDecl rRef) = pureAST' s0 recv
     in (s1, Code rDecl (rRef <> "." <> P.text (T.unpack name)))
  ExprMethod recv name args ->
    let (s1, Code rDecl rRef) = pureAST' s0 recv
        foo :: CG -> Rec (Expr (Const Int)) u' -> (CG, [Code])
        foo s'0 (RecCons x xs) =
          let (s'1, x') = pureAST' s'0 x
              (s'2, cs) = foo s'1 xs
           in (s'2, x' : cs)
        foo s' RecNil = (s', [])
        (s2, lArgs') = foo s1 args
        (lVars, lArgs) = partitionCode lArgs'
        call = rRef <> "." <> P.text (T.unpack name) <> P.parens (P.hcat (P.punctuate ", " lArgs))
     in (s2, Code (rDecl $$ P.vcat lVars) call)
  ExprMethodCallback recv name f ->
    let (s1, Code rDecl rRef) = pureAST' s0 recv
        (nParam, s2) = allocIdent s1
        ex = f (Const nParam)
        (s3, Code exDecl exRef) = pureAST' s2 ex
        paramName = 'n' : show nParam
        callback = "function" <+> P.parens (P.text paramName) <+> P.braces (exDecl $$ "return" <+> exRef)
        call = rRef <> "." <> P.text (T.unpack name) <> P.parens callback
     in (s3, Code rDecl call)
  ExprIndex arr idx ->
    let (s1, Code aDecl aRef) = pureAST' s0 arr
        (s2, Code iDecl iRef) = pureAST' s1 idx
     in (s2, Code (aDecl $$ iDecl) (aRef <> P.brackets iRef))
  MathUnary name x ->
    let (s1, Code xDecl xRef) = pureAST' s0 x
     in (s1, Code xDecl ("Math." <> P.text (T.unpack name) <> P.parens xRef))
  MathBinary name x y ->
    let (s1, Code xDecl xRef) = pureAST' s0 x
        (s2, Code yDecl yRef) = pureAST' s1 y
     in (s2, Code (xDecl $$ yDecl) ("Math." <> P.text (T.unpack name) <> P.parens (xRef <> ", " <> yRef)))
  UnsafeNullable x -> pureAST' s0 x

renderBin :: String -> CG -> Expr (Const Int) a -> Expr (Const Int) b -> (CG, Code)
renderBin op s0 x y =
  let (s1, Code xDecl xRef) = pureAST' s0 x
      (s2, Code yDecl yRef) = pureAST' s1 y
   in ( s2
      , Code (xDecl $$ yDecl)
          (wrapOperand x xRef <+> P.text op <+> wrapOperand y yRef)
      )

