{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , OverloadedStrings
#-}
module JShark.Api where

import Data.Text (Text)
import JShark.Types
import JShark.Object
import JShark.Rec (Rec(..), (<:))

data Window
type instance Field Window "location.host" = 'String
type instance Field Window "location.hash" = 'String

window :: Effect f ('Object Window)
window = unsafeObject "window"

host :: EffectSyntax f (Expr f 'String)
host = get @"location.host" window

-- | @window.location.hash@
locationHash :: EffectSyntax f (Expr f 'String)
locationHash = get @"location.hash" window

-- | Empty untyped object literal (@{}@), pinned to @'Object ()@.
emptyObject :: Effect f ('Object ())
emptyObject = unsafeObject "{}"

onClick :: Effect f ('Object obj) -> (f 'Unit -> Effect f a) -> EffectSyntax f ()
onClick el f = toSyntax_ $ unsafeObjectAssign (unsafeObjectGet el "onclick") (LambdaE f)

-- | 'onClick' with an 'EffectSyntax' body (no manual 'fromSyntax'/'toSyntax' noOp).
onClick_ :: Effect f ('Object obj) -> EffectSyntax f (f 'Unit) -> EffectSyntax f ()
onClick_ el body = onClick el $ \_ -> stmts body

consoleLog :: Expr f u -> EffectSyntax f ()
consoleLog u = toSyntax (ffi "console.log" (arg u <: RecNil)) *> pure ()

ffi :: String -> Rec (Arg f) us -> Effect f v
ffi name args = FFI name args

-- | @recv.method(args…)@.
callMethod :: Effect f object -> String -> Rec (Arg f) us -> Effect f u
callMethod = CallMethod

expr :: Expr f u -> Effect f u
expr = Lift

plus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
plus = Plus

apply :: Expr f ('Function u v) -> Expr f u -> Expr f v
apply = Apply

-- | PHOAS variable: Kmett's @var = return@ / @Place@.
var :: f u -> Expr f u
var = Var

lambda ::
     (Expr f u -> Expr f v)
  -> Expr f ('Function u v)
lambda f = Lambda (\x -> f (var x))

lambdaE ::
    (Effect f u -> Effect f v)
  -> Effect f ('Function u v)
lambdaE f = LambdaE (\x -> f (Lift (var x)))

-- | Embed a 'Double' as a number literal.
--
-- Integer literals can use the 'Num' instance on 'Expr' directly
-- (@3 :: Expr f 'Number@). Prefer 'number' when you already have a
-- runtime 'Double' (including non-integers).
number :: Double -> Expr f 'Number
number = Literal . ValueNumber

bool :: Bool -> Expr f 'Bool
bool = Literal . ValueBool

true_, false_ :: Expr f 'Bool
true_ = bool True
false_ = bool False

string :: Text -> Expr f 'String
string = Literal . ValueString

-- | @arr.forEach(function(x){…})@. 'callMethod' + 'LambdaE', not a dedicated AST node.
forEach :: Expr f ('Array u) -> (Expr f u -> Effect f u') -> Effect f 'Unit
forEach arr f = callMethod (expr arr) "forEach" (ArgEffect (LambdaE (\x -> f (var x))) <: RecNil)

-- | 'forEach' with an 'EffectSyntax' body.
forEach_ :: Expr f ('Array u) -> (Expr f u -> EffectSyntax f (f 'Unit)) -> EffectSyntax f (f 'Unit)
forEach_ arr f = toSyntax $ forEach arr (\x -> stmts (f x))

noOp :: Effect f 'Unit
noOp = expr (Literal ValueUnit)

let_ ::
     Expr f u
  -> (Expr f u -> Expr f v)
  -> Expr f v
let_ e f = Let e (\x -> f (var x))

-- Control flow -----------------------------------------------------------

-- | Ternary conditional: @if_ c t e@ compiles to @c ? t : e@.
if_ :: Expr f 'Bool -> Expr f u -> Expr f u -> Expr f u
if_ = If

-- | Effectful conditional. See the note on 'IfE' regarding the condition
-- expression's limitations.
ifE :: Expr f 'Bool -> Effect f u -> Effect f u -> Effect f u
ifE = IfE

-- | Run an effect only when the condition holds, otherwise do nothing.
when_ :: Expr f 'Bool -> Effect f 'Unit -> Effect f 'Unit
when_ c t = IfE c t noOp

-- | Loop while the condition holds. See the note on 'While' regarding the
-- condition expression's limitations.
while_ :: Expr f 'Bool -> Effect f 'Unit -> Effect f 'Unit
while_ = While

-- Option --------------------------------------------------------------

-- | Present value. Literals pack as 'ValueOption'; otherwise a typed
-- 'UnsafeNullable' (not treated as a known Some by the optimizer, so
-- FFI nulls such as 'getItem' keep their @=== null@ check).
some :: Expr f u -> Expr f ('Option u)
some (Literal v) = Literal (ValueOption (Just v))
some x = UnsafeNullable x

-- | Absent value: JS @null@.
none :: Expr f ('Option u)
none = Literal (ValueOption Nothing)

-- | Eliminate an 'Option', analogous to 'maybe'.
optionCase :: Expr f ('Option u) -> Expr f v -> (Expr f u -> Expr f v) -> Expr f v
optionCase opt noneBranch someBranch = OptionCase opt noneBranch (\x -> someBranch (var x))

-- | Untyped global call on the pure tree. Prefer 'ffi' on 'Effect'.
unsafeExprFfi :: Text -> Rec (Expr f) us -> Expr f v
unsafeExprFfi = UnsafeExprFFI

-- | Untyped property access on the pure tree. Prefer 'getProp' / 'CallMethod'.
unsafeExprProp :: Expr f u -> Text -> Expr f v
unsafeExprProp = UnsafeExprProp

-- | Untyped method call on the pure tree. Prefer 'callMethod' for effects.
unsafeExprMethod :: Expr f u -> Text -> Rec (Expr f) us -> Expr f v
unsafeExprMethod = UnsafeExprMethod

-- | Untyped callback method on the pure tree (e.g. @map@). Prefer 'callMethod'.
unsafeExprMethodCallback :: Expr f u -> Text -> (Expr f a -> Expr f b) -> Expr f v
unsafeExprMethodCallback recv name f = UnsafeExprMethodCallback recv name (\x -> f (var x))

exprIndex :: Expr f ('Array u) -> Expr f 'Number -> Expr f u
exprIndex = ExprIndex

mathUnary :: Text -> Expr f 'Number -> Expr f 'Number
mathUnary = MathUnary

mathBinary :: Text -> Expr f 'Number -> Expr f 'Number -> Expr f 'Number
mathBinary = MathBinary

-- | Reinterpret a value that may be a JS null (e.g. the result of
-- @localStorage.getItem@) as an 'Option'. See the note on 'UnsafeNullable'.
unsafeNullable :: Expr f u -> Expr f ('Option u)
unsafeNullable = UnsafeNullable

addEventListener :: Text -> Effect f ('Object obj) -> (f u -> Effect f a) -> EffectSyntax f ()
addEventListener name el handler =
  toSyntax_ $ callMethod el "addEventListener" (arg (string name) <: ArgEffect (LambdaE handler) <: RecNil)

-- | 'addEventListener' with an 'EffectSyntax' body.
addEventListener_ :: Text -> Effect f ('Object obj) -> EffectSyntax f (f 'Unit) -> EffectSyntax f ()
addEventListener_ name el body = addEventListener name el $ \_ -> stmts body

-- Lifting ----------------------------------------------------------------

-- | Wrap a pure expression as an FFI argument. Effectful arguments
-- (object handles, 'LambdaE') use 'ArgEffect' so the two trees stay distinct.
arg :: Expr f u -> Arg f u
arg = ArgExpr

-- | Convert binders (@f u@), expressions, and effects into an 'Effect'
-- suitable for FFI / property access without manual 'Lift'/'Var'.
class ToEffect f u a where
  toEffect :: a -> Effect f u

instance ToEffect f u (Effect f u) where
  toEffect = id

instance ToEffect f u (Expr f u) where
  toEffect = Lift

instance ToEffect f u (f u) where
  toEffect = Lift . Var

-- | Convert binders and expressions into an 'Expr'.
class ToExpr f u a where
  toExpr :: a -> Expr f u

instance ToExpr f u (Expr f u) where
  toExpr = id

instance ToExpr f u (f u) where
  toExpr = Var

-- | Bind an effect once (@const n = e@) and return a reusable handle.
hold :: Effect f u -> EffectSyntax f (Effect f u)
hold e = fmap (expr . Var) (toSyntax e)

-- | 'f' binder for an untyped object as an 'Effect' handle.
obj :: f ('Object ()) -> Effect f ('Object ())
obj = toEffect

-- | 'Expr' untyped object as an 'Effect' handle.
objE :: Expr f ('Object ()) -> Effect f ('Object ())
objE = toEffect

-- | Read a property (@o.name@).
getProp :: Effect f ('Object a) -> String -> EffectSyntax f (Expr f u)
getProp o name = fmap Var $ toSyntax $ unsafeObjectGet o name

-- | Assign a property (@o.name = v@).
setProp :: Effect f ('Object a) -> String -> Expr f u -> EffectSyntax f (f 'Unit)
setProp o name v = toSyntax $ unsafeObjectAssign (unsafeObjectGet o name) (Lift v)

-- | Read a property from an 'Effect', 'Expr', or binder (untyped objects).
getProp' :: forall f o u. ToEffect f ('Object ()) o => o -> String -> EffectSyntax f (Expr f u)
getProp' o name = getProp (toEffect o :: Effect f ('Object ())) name

-- | Assign a property on an 'Effect', 'Expr', or binder (untyped objects).
setProp' :: forall f o u. ToEffect f ('Object ()) o => o -> String -> Expr f u -> EffectSyntax f (f 'Unit)
setProp' o name v = setProp (toEffect o :: Effect f ('Object ())) name v

-- | Turn an 'EffectSyntax' block into a single 'Effect' (for 'when_'/'ifE'/handlers).
stmts :: EffectSyntax f (f 'Unit) -> Effect f 'Unit
stmts = fromSyntax

-- | End an 'EffectSyntax' block with @undefined@ / unit.
done :: EffectSyntax f (f 'Unit)
done = toSyntax noOp

-- | 'when_' with an 'EffectSyntax' body.
whenS :: Expr f 'Bool -> EffectSyntax f (f 'Unit) -> EffectSyntax f (f 'Unit)
whenS c body = toSyntax $ when_ c (stmts body)

-- | 'ifE' with 'EffectSyntax' branches.
ifS :: Expr f 'Bool -> EffectSyntax f (f 'Unit) -> EffectSyntax f (f 'Unit) -> EffectSyntax f (f 'Unit)
ifS c t e = toSyntax $ ifE c (stmts t) (stmts e)

-- | Call a nullary JS function that returns unit (@fn()@).
call0 :: forall f. Expr f ('Function 'Unit 'Unit) -> EffectSyntax f (f 'Unit)
call0 fn = toSyntax (ApplyE (Lift fn) noOp :: Effect f 'Unit)

-- Binary operators (JS expression constructors) --------------------------

infix 4 .==, .!=, .>, .<, .>=, .<=
infixr 3 .&&
infixr 2 .||

(.==), (.!=) :: Expr f a -> Expr f a -> Expr f 'Bool
(.==) = Eq   -- compiles to ===
(.!=) = NEq  -- compiles to !==

(.>), (.<), (.>=), (.<=) :: Expr f a -> Expr f a -> Expr f 'Bool
(.>) = GTh
(.<) = LTh
(.>=) = GTEq
(.<=) = LTEq

(.&&), (.||) :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool
(.&&) = And
(.||) = Or
