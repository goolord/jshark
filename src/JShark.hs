{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}

{-# options_ghc -fno-warn-unused-top-binds #-}

module JShark
  ( Expr(..)
  , Value(..)
    -- Evaluation
  , evaluate
  , evaluateNumber
  , evaluateCached
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

-- | Implements the unary @Math@ functions supported by 'MathUnary'.
mathUnaryFn :: Text -> Double -> Double
mathUnaryFn name = case name of
  "sin" -> sin
  "cos" -> cos
  "tan" -> tan
  "asin" -> asin
  "acos" -> acos
  "atan" -> atan
  "sqrt" -> sqrt
  "cbrt" -> \x -> signum x * (abs x ** (1 / 3))
  "exp" -> exp
  "log" -> log
  "log2" -> logBase 2
  "log10" -> logBase 10
  "floor" -> fromIntegral . (floor :: Double -> Integer)
  "ceil" -> fromIntegral . (ceiling :: Double -> Integer)
  -- JS's Math.round rounds half-way values toward +Infinity (e.g.
  -- Math.round(2.5) === 3, Math.round(-2.5) === -2), unlike Haskell's
  -- 'round' (banker's rounding to even: round 2.5 == 2). floor(x + 0.5)
  -- matches JS's semantics.
  "round" -> fromIntegral . (floor :: Double -> Integer) . (+ 0.5)
  "trunc" -> fromIntegral . (truncate :: Double -> Integer)
  _ -> error ("evaluate: unknown Math unary function " ++ T.unpack name)

-- | Implements the binary @Math@ functions supported by 'MathBinary'.
mathBinaryFn :: Text -> Double -> Double -> Double
mathBinaryFn name = case name of
  "pow" -> (**)
  "atan2" -> atan2
  "max" -> max
  "min" -> min
  "hypot" -> \x y -> sqrt (x * x + y * y)
  _ -> error ("evaluate: unknown Math binary function " ++ T.unpack name)

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
pureProgram = renderIIFE . snd . pureAST' startCG

-- | Effectful computation compiled to a self-contained JS program (IIFE).
effectfulProgram :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Effect f u)
  -> Doc
effectfulProgram = renderIIFE . snd . effectfulAST' startCG

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
effectfulAST = renderCode . snd . effectfulAST' startCG

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
    let ident0 = cgIdent s0
        (s1, Code xsDecl xsRef) = pureAST' s0 xs
        (s2, Code asDecl asRef) = effectfulAST' s0 (f (Const ident0))
        bodyStmt = if P.isEmpty asRef then asDecl else asDecl $$ (asRef <> P.semi)
        forE = xsRef <> ".forEach" <> (P.parens
               $ "function" <> P.parens (P.text ('n':show (cgIdent s1)))
               <> P.braces (P.nest 2 bodyStmt)) <> P.semi
     in (s2, Code xsDecl forE)
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
    let ident0 = cgIdent s0
        ex = f (Const ident0)
        (s1, Code exprXDecl exprXRef) = effectfulAST' s0 ex
        param = cgIdent s1
        (_, s2) = allocIdent s1
     in ( s2
        , Code mempty
            $ "function"
            <+> P.parens (P.text $ 'n':show param)
            <+> P.braces ( (exprXDecl $$ "return" <+> exprXRef) )
        )
  ApplyE fex ex ->
    let (s1, Code exprXDecl exprXRef) = effectfulAST' s0 fex
        (s2, Code exprYDecl exprYRef) = effectfulAST' s1 ex
     in (s2, Code (exprXDecl $$ exprYDecl) (jsCall exprXRef exprYRef))

pureAST :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Doc
pureAST = renderCode . snd . pureAST' startCG

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

