{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language TypeOperators #-}

{-# options_ghc -fno-warn-unused-top-binds #-}

module JShark
  ( Expr(..)
  , Value(..)
    -- Evaluation
  , evaluate
  , evaluateNumber
  , pureAST
  , effectfulAST
  , printComputation
  , renderJS
  ) where

-- This uses a higher-order PHOAS approach as described by
-- https://www.reddit.com/r/haskell/comments/85een6/sharing_from_phoas_multiple_interpreters_from_free/dvxhlba

import Data.Functor.Const (Const(..))
import Data.Kind
import Data.Text (Text)
import Numeric (showFFloat)
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

evaluate :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Value u
evaluate e0 = go e0 where
  go :: forall v. Expr Value v -> Value v
  go = \case
    Literal v -> v
    Plus x y -> ValueNumber (unNumber (go x) + unNumber (go y))
    Times x y -> ValueNumber (unNumber (go x) * unNumber (go y))
    Minus x y -> ValueNumber (unNumber (go x) - unNumber (go y))
    Abs x -> ValueNumber (abs (unNumber (go x)))
    Sign x -> ValueNumber (signum (unNumber (go x)))
    Negate x -> ValueNumber (negate (unNumber (go x)))
    FracDiv x y -> ValueNumber (unNumber (go x) / unNumber (go y))
    Var x -> x
    Apply g x -> unFunction (go g) (go x)
    Lambda g -> ValueFunction (go . g)
    Concat x y -> ValueString (unString (go x) <> unString (go y))
    Show x -> ValueString (jsShow (go x))
    And x y -> ValueBool (unBool (go x) && unBool (go y))
    Or x y -> ValueBool (unBool (go x) || unBool (go y))
    Eq x y -> ValueBool (valueEq (go x) (go y))
    NEq x y -> ValueBool (not (valueEq (go x) (go y)))
    GTh x y -> ValueBool (valueCompare (go x) (go y) == GT)
    LTh x y -> ValueBool (valueCompare (go x) (go y) == LT)
    GTEq x y -> ValueBool (valueCompare (go x) (go y) /= LT)
    LTEq x y -> ValueBool (valueCompare (go x) (go y) /= GT)
    Let x g -> go (g (go x))
    If c t e -> if unBool (go c) then go t else go e
    Some x -> ValueOption (Just (go x))
    None -> ValueOption Nothing
    OptionCase opt none' someF -> case go opt of
      ValueOption Nothing -> go none'
      ValueOption (Just x) -> go (someF x)
    Ok x -> ValueResult (Left (go x))
    Err y -> ValueResult (Right (go y))
    ResultCase r okF errF -> case go r of
      ValueResult (Left x) -> go (okF x)
      ValueResult (Right y) -> go (errF y)
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
    ExprIndex xs i -> case go xs of
      ValueArray vs ->
        -- JS array indexing truncates the index toward zero (as part of
        -- ToIntegerOrInfinity) rather than rounding, and returns @undefined@
        -- out of bounds rather than crashing; we can't represent
        -- @undefined@ generically here (there's no 'Value' inhabitant for
        -- an arbitrary universe @u@), so out-of-bounds access is a hard
        -- error in the reference interpreter.
        let idx = truncate (unNumber (go i)) :: Int
         in if idx >= 0 && idx < length vs
              then vs !! idx
              else error "evaluate: array index out of bounds"
    MathUnary name x -> ValueNumber (mathUnaryFn name (unNumber (go x)))
    MathBinary name x y -> ValueNumber (mathBinaryFn name (unNumber (go x)) (unNumber (go y)))
    UnsafeNullable _ ->
      error "evaluate: cannot evaluate UnsafeNullable (an FFI-derived Option)"

fromRightE :: Either [Char] c -> c
fromRightE = either error id

printComputation :: Doc -> IO ()
printComputation (computation) = do
  putStrLn $ P.renderStyle P.style computation

renderJS :: Doc -> String
renderJS = P.renderStyle P.style

renderCode :: Code -> Doc
renderCode (Code a b) = a $$ b

partitionCode :: [Code] -> ([Doc], [Doc])
partitionCode ((Code a b):cs) = let (as,bs) = partitionCode cs in ((a:as),(b:bs))
partitionCode [] = ([], [])

effectfulAST :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Effect f u)
  -> Doc
effectfulAST = renderCode . snd . effectfulAST' 0

effectfulAST' :: forall v. Int -> Effect (Const Int) v -> (Int, Code)
effectfulAST' !n0 = \case
  Lift x -> pureAST' n0 x
  FFI fn args ->
    let foo :: Int -> Rec (Expr (Const Int)) u' -> (Int, [Code])
        foo n'0 (RecCons x xs) = 
          let (n'1, x') = pureAST' n'0 x 
              (n'2, cs) = foo n'1 xs
           in (n'2, x' : cs)
        foo n' RecNil = (n',[])
        (n1, lArgs') = foo n0 args
        (lVars, lArgs) = partitionCode lArgs'
        foreignFunction = P.text fn <> P.parens (P.hcat (P.punctuate ", " lArgs))
     in (n1, Code (P.vcat lVars) foreignFunction)
  ForEach xs f ->
    let (n1, (Code xsDecl xsRef)) = pureAST' n0 xs
        (n2, (Code asDecl asRef)) = effectfulAST' n0 (f (Const n0))
        bodyStmt = if P.isEmpty asRef then asDecl else asDecl $$ (asRef <> P.semi)
        forE = xsRef <> ".forEach" <> (P.parens 
               $ "function" <> P.parens (P.text ('n':show n1))
               <> P.braces (P.nest 2 bodyStmt)) <> P.semi
     in (n2, Code xsDecl forE)
  IfE c t e ->
    -- We always render this as an if/else statement (rather than trying to
    -- special-case a ternary expression) because an effectful branch's
    -- rendered "ref" may be a leftover 'Unit' placeholder rather than a
    -- genuinely-empty Doc, which made an emptiness-based heuristic unsound
    -- (it could produce a ternary with an empty branch, e.g. `c ? x : `).
    -- Using a shared `let`-bound result variable, assigned in both
    -- branches, is correct regardless of whether the branches are 'Unit'
    -- or carry a real value.
    let (n1, Code cDecl cRef) = pureAST' n0 c
        resultVar = 'n' : show n1
        (n2, Code tDecl tRef) = effectfulAST' (n1 + 1) t
        (n3, Code eDecl eRef) = effectfulAST' n2 e
        assign ref = if P.isEmpty ref then mempty else (P.text resultVar <+> "=" <+> ref) <> P.semi
        ifStmt = ("let" <+> P.text resultVar) <> P.semi
          $$ ("if" <+> P.parens cRef <+> P.braces (P.nest 2 (tDecl $$ assign tRef)))
          $$ ("else" <+> P.braces (P.nest 2 (eDecl $$ assign eRef)))
     in (n3, Code (cDecl $$ ifStmt) (P.text resultVar))
  While cond body ->
    let (n1, Code condDecl condRef) = pureAST' n0 cond
        (n2, Code bodyDecl bodyRef) = effectfulAST' n1 body
        bodyStmt = if P.isEmpty bodyRef then bodyDecl else bodyDecl $$ (bodyRef <> P.semi)
        whileStmt = "while" <+> P.parens condRef <+> P.braces (P.nest 2 bodyStmt)
     in (n2, Code (condDecl $$ whileStmt) mempty)
  Bind x f ->
    let (n1, (Code x1Decl x1Ref)) = effectfulAST' n0 x
     in if P.isEmpty x1Ref
          -- x produced no meaningful value (e.g. a 'Unit'-typed effect like
          -- 'noOp' or 'While'), so don't allocate a fresh (and undeclared!)
          -- binding for it; reuse whichever variable was last legitimately
          -- declared, since the placeholder is never actually inspected.
          then
            let (n2, (Code x2Decl x2Ref)) = effectfulAST' n1 (f (Const (n1 - 1)))
             in (n2, Code (x1Decl $$ x2Decl) x2Ref)
          else
            let constX = ("const" <+> P.text ('n':show n1) <+> "=" <+> x1Ref) <> P.semi
                (n2, (Code x2Decl x2Ref)) = effectfulAST' (n1 + 1) (f (Const n1))
             in (n2, Code (x1Decl $$ constX $$ x2Decl) x2Ref)
  UnsafeObject obj -> (n0, Code mempty $ P.text $ T.unpack obj)
  UnsafeObjectGet x string ->
    let (n1, (Code x1Decl x1Ref)) = effectfulAST' n0 x
    in (n1, Code x1Decl $ x1Ref <> "." <> P.text string)
  UnsafeObjectAssign x y ->
    let (n1, (Code x1Decl x1Ref)) = effectfulAST' n0 x
        (n2, (Code y1Decl y1Ref)) = effectfulAST' n1 y
    in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <> " = " <> y1Ref )
  ObjectFFI x ffi ->
    let (n1, (Code x1Decl x1Ref)) = effectfulAST' n0 x
        (n2, (Code ffi1Decl ffi1Ref)) = effectfulAST' n1 ffi
    in (n2, Code (x1Decl $$ ffi1Decl) $ x1Ref <> "." <> ffi1Ref)
  UnEffectful x -> 
    let (n1, (Code a1Decl a1Ref)) = pureAST' n0 x
     in (n1, (Code a1Decl $ a1Ref <> P.parens mempty))
  LambdaE f ->
    let ex = f (Const n0)
        (n1, (Code exprXDecl exprXRef)) = effectfulAST' n0 ex
     in ( n1 + 1
        , Code mempty
            $ "function" 
            <+> P.parens (P.text $ 'n':show (n1))
            <+> P.braces ( (exprXDecl $$ "return" <+> exprXRef) )
        )
  ApplyE fex ex ->
    let (n1, (Code exprXDecl exprXRef)) = effectfulAST' n0 fex
        (n2, (Code exprYDecl exprYRef)) = effectfulAST' n1 ex
     in ( n2+2
        , Code (exprXDecl $$ exprYDecl $$ ("const" <+> (P.text $ 'n':show (n2+1)) <+> "=" <+> exprXRef) <> P.semi)
            (P.text ('n':show (n2+1)) <> P.parens exprYRef)
        )

pureAST :: forall (u :: Universe).
     (forall (f :: Universe -> Type). Expr f u)
  -> Doc
pureAST = renderCode . snd . pureAST' 0

pureAST' :: forall v. Int -> Expr (Const Int) v 
   -> (Int, Code)
pureAST' !n0 = \case
  Literal v -> case v of
    ValueNumber d -> (n0,Code mempty (P.text $ showFFloat Nothing d ""))
    ValueArray xs ->       
      let foo :: Int -> [Value u] -> (Int, [Code])
          foo n'0 (x:xs') = 
            let (n'1, x') = pureAST' n'0 (Literal x)
                (n'2, cs) = foo n'1 xs'
             in (n'2, x' : cs)
          foo n' [] = (n', [])
          (n1, exprs) = foo n0 xs
          (exprDecls, exprRefs) = partitionCode exprs
       in (n1, Code (P.vcat exprDecls) $ P.brackets (P.hcat $ P.punctuate ", " exprRefs))
    ValueString s -> (n0, Code mempty $ P.doubleQuotes (P.text $ T.unpack s))
    ValueFunction _f -> undefined
    ValueUnit -> (n0, mempty)
    ValueOption (Just x) -> pureAST' n0 (Literal x)
    ValueOption Nothing -> (n0, Code mempty "null")
    ValueResult (Left x) ->
      let (n1, Code xDecl xRef) = pureAST' n0 (Literal x)
       in (n1, Code xDecl (P.brackets ("true" <> ", " <> xRef)))
    ValueResult (Right x) ->
      let (n1, Code xDecl xRef) = pureAST' n0 (Literal x)
       in (n1, Code xDecl (P.brackets ("false" <> ", " <> xRef)))
    ValueBool True -> (n0, Code mempty "true")
    ValueBool False -> (n0, Code mempty "false")
  Concat x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "+" <+> y1Ref)
  Plus x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "+" <+> y1Ref)
  Minus x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "-" <+> y1Ref)
  Times x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "*" <+> y1Ref)
  FracDiv x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "/" <+> y1Ref)
  Abs x ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
     in (n1, Code x1Decl $ "Math.abs" <> P.parens x1Ref)
  Sign x ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
     in (n1, Code x1Decl $ "Math.sign" <> P.parens x1Ref)
  Show x ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
     in (n1, Code x1Decl $ "String" <> P.parens x1Ref)
  Negate x ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
     in (n1, Code x1Decl $ "-" <> P.parens x1Ref)
  Lambda f ->
    let ex = f (Const n0)
        (n1, (Code exprXDecl exprXRef)) = pureAST' n0 ex
     in ( n1
        , Code exprXDecl 
            $ "function" 
            <+> P.parens (P.text $ 'n':show n0)
            <+> P.braces ("return" <+> (P.parens exprXRef))
        )
  And x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "&&" <+> y1Ref)
  Or x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "||" <+> y1Ref)
  Eq x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "===" <+> y1Ref)
  NEq x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "!==" <+> y1Ref)
  GTh x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> ">" <+> y1Ref)
  LTh x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "<" <+> y1Ref)
  GTEq x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> ">=" <+> y1Ref)
  LTEq x y ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        (n2, (Code y1Decl y1Ref)) = pureAST' n1 y
     in (n2, Code (x1Decl $$ y1Decl) $ x1Ref <+> "<=" <+> y1Ref)
  Let x g ->
    let (n1, (Code x1Decl x1Ref)) = pureAST' n0 x
        constX = ("const" <+> P.text ('n':show n1) <+> "=" <+> x1Ref) <> P.semi
        (n2, (Code x2Decl x2Ref)) = pureAST' (n1 + 1) (g (Const n1))
     in (n2, Code (x1Decl $$ constX $$ x2Decl) (x2Ref))
  Apply fex ex ->
    let (n1, (Code exprXDecl exprXRef)) = pureAST' n0 fex
        (n2, (Code exprYDecl exprYRef)) = pureAST' n1 ex
     in ( n2+2
        , Code (exprXDecl $$ exprYDecl $$ ("const" <+> (P.text $ 'n':show (n2+1)) <+> "=" <+> exprXRef) <> P.semi)
            (P.text ('n':show (n2+1)) <> P.parens exprYRef)
        )
  Var (Const x) -> (n0, Code mempty $ P.text ('n':show x))
  If c t e ->
    let (n1, Code cDecl cRef) = pureAST' n0 c
        (n2, Code tDecl tRef) = pureAST' n1 t
        (n3, Code eDecl eRef) = pureAST' n2 e
     in (n3, Code (cDecl $$ tDecl $$ eDecl) (P.parens (cRef <+> "?" <+> tRef <+> ":" <+> eRef)))
  Some x -> pureAST' n0 x
  None -> (n0, Code mempty "null")
  OptionCase opt none' someF ->
    let (n1, Code optDecl optRef) = pureAST' n0 opt
        optVar = 'n' : show n1
        constOpt = ("const" <+> P.text optVar <+> "=" <+> optRef) <> P.semi
        (n2, Code noneDecl noneRef) = pureAST' (n1 + 1) none'
        (n3, Code someDecl someRef) = pureAST' n2 (someF (Const n1))
     in ( n3
        , Code (optDecl $$ constOpt $$ noneDecl $$ someDecl)
            (P.parens (P.text optVar <+> "===" <+> "null" <+> "?" <+> noneRef <+> ":" <+> someRef))
        )
  Ok x ->
    let (n1, Code xDecl xRef) = pureAST' n0 x
     in (n1, Code xDecl (P.brackets ("true" <> ", " <> xRef)))
  Err x ->
    let (n1, Code xDecl xRef) = pureAST' n0 x
     in (n1, Code xDecl (P.brackets ("false" <> ", " <> xRef)))
  ResultCase r okF errF ->
    let (n1, Code rDecl rRef) = pureAST' n0 r
        rVar = 'n' : show n1
        constR = ("const" <+> P.text rVar <+> "=" <+> rRef) <> P.semi
        payloadVar = 'n' : show (n1 + 1)
        constPayload = ("const" <+> P.text payloadVar <+> "=" <+> (P.text rVar <> P.brackets "1")) <> P.semi
        (n2, Code okDecl okRef) = pureAST' (n1 + 2) (okF (Const (n1 + 1)))
        (n3, Code errDecl errRef) = pureAST' n2 (errF (Const (n1 + 1)))
     in ( n3
        , Code (rDecl $$ constR $$ constPayload $$ okDecl $$ errDecl)
            (P.parens ((P.text rVar <> P.brackets "0") <+> "?" <+> okRef <+> ":" <+> errRef))
        )
  UnsafeEffectExpr eff -> effectfulAST' n0 eff
  ExprFFI fn args ->
    let foo :: Int -> Rec (Expr (Const Int)) u' -> (Int, [Code])
        foo n'0 (RecCons x xs) =
          let (n'1, x') = pureAST' n'0 x
              (n'2, cs) = foo n'1 xs
           in (n'2, x' : cs)
        foo n' RecNil = (n', [])
        (n1, lArgs') = foo n0 args
        (lVars, lArgs) = partitionCode lArgs'
        call = P.text (T.unpack fn) <> P.parens (P.hcat (P.punctuate ", " lArgs))
     in (n1, Code (P.vcat lVars) call)
  ExprProp recv name ->
    let (n1, Code rDecl rRef) = pureAST' n0 recv
     in (n1, Code rDecl (rRef <> "." <> P.text (T.unpack name)))
  ExprMethod recv name args ->
    let (n1, Code rDecl rRef) = pureAST' n0 recv
        foo :: Int -> Rec (Expr (Const Int)) u' -> (Int, [Code])
        foo n'0 (RecCons x xs) =
          let (n'1, x') = pureAST' n'0 x
              (n'2, cs) = foo n'1 xs
           in (n'2, x' : cs)
        foo n' RecNil = (n', [])
        (n2, lArgs') = foo n1 args
        (lVars, lArgs) = partitionCode lArgs'
        call = rRef <> "." <> P.text (T.unpack name) <> P.parens (P.hcat (P.punctuate ", " lArgs))
     in (n2, Code (rDecl $$ P.vcat lVars) call)
  ExprMethodCallback recv name f ->
    let (n1, Code rDecl rRef) = pureAST' n0 recv
        ex = f (Const n1)
        (n2, Code exDecl exRef) = pureAST' (n1 + 1) ex
        paramName = 'n' : show n1
        callback = "function" <+> P.parens (P.text paramName) <+> P.braces (exDecl $$ "return" <+> exRef)
        call = rRef <> "." <> P.text (T.unpack name) <> P.parens callback
     in (n2, Code rDecl call)
  ExprIndex arr idx ->
    let (n1, Code aDecl aRef) = pureAST' n0 arr
        (n2, Code iDecl iRef) = pureAST' n1 idx
     in (n2, Code (aDecl $$ iDecl) (aRef <> P.brackets iRef))
  MathUnary name x ->
    let (n1, Code xDecl xRef) = pureAST' n0 x
     in (n1, Code xDecl ("Math." <> P.text (T.unpack name) <> P.parens xRef))
  MathBinary name x y ->
    let (n1, Code xDecl xRef) = pureAST' n0 x
        (n2, Code yDecl yRef) = pureAST' n1 y
     in (n2, Code (xDecl $$ yDecl) ("Math." <> P.text (T.unpack name) <> P.parens (xRef <> ", " <> yRef)))
  UnsafeNullable x -> pureAST' n0 x

