{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , KindSignatures
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , OverloadedStrings
#-}

module JShark.Api
  ( -- * Types
    Expr
  , Effect
  , EffectSyntax
  , Universe(..)
  , Value(..)
  , Arg(..)
  , Field
  , Comparable
    -- * Literals
  , number
  , bool
  , true_
  , false_
  , string
  , emptyArray
  , toString
    -- * Variables and lifting
  , var
  , expr
  , yield
  , arg
  , ToEffect(..)
  , ToExpr(..)
    -- * Functions and binding
  , lambda
  , lambda2
  , lambda3
  , lambdaE
  , apply
  , apply2
  , apply3
  , let_
  , letRec
  , bindRec
  , loop0
    -- * Control
  , if_
  , ifE
  , stringCaseE
  , when_
  , while_
  , whenS
  , ifS
  , forEach
  , forEach_
  , try_
  , catch_
  , throw_
    -- * Option
  , some
  , none
  , optionCase
  , optionCaseE
  , whenSomeS
  , whenSomeE
  , unsafeNullable
  , orElse
  , fromOption
    -- * Result
  , ok
  , err
  , resultCase
  , resultCaseE
    -- * FFI
  , ffi
  , callMethod
  , assign
    -- * Objects
  , emptyObject
  , newObject
  , get
  , set
  , HasField(..)
  , getProp
  , setProp
  , getProp'
  , setProp'
    -- * Events / window
  , window
  , host
  , locationHash
  , onClick
  , onClick_
  , addEventListener
  , addEventListener_
    -- * Syntax
  , noOp
  , discard
  , hold
  , stmts
  , done
  , toSyntax
  , toSyntax_
  , fromSyntax
  , call0
    -- * Operators
  , not_
  , typeOf
  , (.==)
  , (.!=)
  , (.>)
  , (.<)
  , (.>=)
  , (.<=)
  , (.&&)
  , (.||)
  , rem_
  , bitAnd
  , bitOr
  , bitXor
  , shl
  , shr
  , ushr
  , parseInt_
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol)
import JShark.Types
import JShark.Object hiding (get, set)
import qualified JShark.Object as Object
import JShark.Rec (Rec(..), (<:))

data Window
type instance Field Window "location.host" = 'String
type instance Field Window "location.hash" = 'String

window :: Effect f ('MutableObject Window)
window = unsafeObject "window"

host :: EffectSyntax f (Expr f 'String)
host = Object.get @"location.host" window

locationHash :: EffectSyntax f (Expr f 'String)
locationHash = Object.get @"location.hash" window

emptyObject :: Effect f ('MutableObject ())
emptyObject = newObject

onClick :: Effect f ('MutableObject obj) -> (f 'Unit -> Effect f a) -> EffectSyntax f ()
onClick el f = toSyntax_ $ unsafeObjectAssign (unsafeObjectGet el "onclick") (LambdaE f)

onClick_ :: Effect f ('MutableObject obj) -> EffectSyntax f (f 'Unit) -> EffectSyntax f ()
onClick_ el body = onClick el $ \_ -> stmts body

ffi :: String -> Rec (Arg f) us -> Effect f v
ffi = FFI

callMethod :: Effect f object -> String -> Rec (Arg f) us -> Effect f u
callMethod = CallMethod

-- | @Object.assign(dst, src)@. In-place copy; @dst@ keeps its identity
-- (needed when a closure already captured @dst@).
assign :: Effect f u -> Effect f u -> EffectSyntax f (f 'Unit)
assign dst src = do
  toSyntax_ $ ffi "Object.assign" (ArgEffect dst <: ArgEffect src <: RecNil)
  done

expr :: Expr f u -> Effect f u
expr = Lift

yield :: Expr f u -> EffectSyntax f (f u)
yield = toSyntax . Lift

apply :: Expr f ('Function u v) -> Expr f u -> Expr f v
apply = Apply

-- | Nested unary application, not a binary JS call.
apply2 :: Expr f ('Function a ('Function b c)) -> Expr f a -> Expr f b -> Expr f c
apply2 f x y = apply (apply f x) y

apply3 :: Expr f ('Function a ('Function b ('Function c d))) -> Expr f a -> Expr f b -> Expr f c -> Expr f d
apply3 f x y z = apply (apply2 f x y) z

var :: f u -> Expr f u
var = Var

lambda :: (Expr f u -> Expr f v) -> Expr f ('Function u v)
lambda f = Lambda (\x -> f (var x))

-- | Nested unary functions. Not a binary JS @function(a, b)@.
lambda2 :: (Expr f a -> Expr f b -> Expr f c) -> Expr f ('Function a ('Function b c))
lambda2 f = lambda (\x -> lambda (\y -> f x y))

-- | Nested unary functions. Not a ternary JS @function(a, b, c)@.
lambda3 :: (Expr f a -> Expr f b -> Expr f c -> Expr f d) -> Expr f ('Function a ('Function b ('Function c d)))
lambda3 f = lambda (\x -> lambda2 (\y z -> f x y z))

lambdaE :: (Effect f u -> Effect f v) -> Effect f ('Function u v)
lambdaE f = LambdaE (\x -> f (Lift (var x)))

letRec :: (Expr f u -> Expr f u) -> (Expr f u -> Expr f v) -> Expr f v
letRec r b = LetRec (\x -> r (var x)) (\x -> b (var x))

bindRec :: (Effect f u -> Effect f u) -> (Effect f u -> Effect f v) -> Effect f v
bindRec r b = BindRec (\x -> r (Lift (var x))) (\x -> b (Lift (var x)))

-- | Recursively bind a zero-argument effectful function, then run the body.
-- @loop0 paint wire@ is @const render = function(){ paint(render); }; wire(render)@.
loop0
  :: (Effect f ('Function 'Unit 'Unit) -> EffectSyntax f (f 'Unit))
  -> (Effect f ('Function 'Unit 'Unit) -> EffectSyntax f (f 'Unit))
  -> EffectSyntax f (f 'Unit)
loop0 rec body =
  toSyntax $
    bindRec
      (\fn -> lambdaE (\_ -> stmts (rec fn)))
      (\fn -> stmts (body fn))

number :: Double -> Expr f 'Number
number = Literal . ValueNumber

bool :: Bool -> Expr f 'Bool
bool = Literal . ValueBool

true_, false_ :: Expr f 'Bool
true_ = bool True
false_ = bool False

string :: Text -> Expr f 'String
string = Literal . ValueString

emptyArray :: Expr f ('Array u)
emptyArray = Literal (ValueArray [])

-- | @String(x)@.
toString :: Expr f u -> Expr f 'String
toString = Show

-- | @arr.method(function(x){…})@ with an 'Effect' callback.
arrayCallback :: String -> Expr f ('Array u) -> (Expr f u -> Effect f v) -> Effect f w
arrayCallback name arr f =
  callMethod (expr arr) name (ArgEffect (LambdaE (\x -> f (var x))) <: RecNil)

forEach :: Expr f ('Array u) -> (Expr f u -> Effect f u') -> Effect f 'Unit
forEach = arrayCallback "forEach"

forEach_ :: Expr f ('Array u) -> (Expr f u -> EffectSyntax f (f 'Unit)) -> EffectSyntax f (f 'Unit)
forEach_ arr f = toSyntax $ forEach arr (\x -> stmts (f x))

noOp :: Effect f 'Unit
noOp = expr (Literal ValueUnit)

let_ :: Expr f u -> (Expr f u -> Expr f v) -> Expr f v
let_ e f = Let e (\x -> f (var x))

if_ :: Expr f 'Bool -> Expr f u -> Expr f u -> Expr f u
if_ = If

-- | Effectful conditional. Lift an 'Expr' test with 'expr'.
ifE :: Effect f 'Bool -> Effect f u -> Effect f u -> Effect f u
ifE = IfE

-- | @switch (s) { case k: …; default: … }@. First matching label wins.
-- Arms do not fall through. Statement arms that are polymorphic ('FFI',
-- 'CallMethod') need 'discard', same as 'ifE'.
stringCaseE :: Expr f 'String -> [(Text, Effect f v)] -> Effect f v -> Effect f v
stringCaseE = StringCaseE

-- | Drop a result, forcing 'Unit'. Lets statement-'if' be 'IfE' of two
-- unit arms (polymorphic 'FFI' / 'CallMethod' are not unit witnesses).
discard :: Effect f u -> Effect f 'Unit
discard e = Bind e (\_ -> noOp)

when_ :: Effect f 'Bool -> Effect f 'Unit -> Effect f 'Unit
when_ c t = IfE c (discard t) noOp

while_ :: Effect f 'Bool -> Effect f 'Unit -> Effect f 'Unit
while_ = While

try_ :: Effect f u -> Effect f u -> Effect f u
try_ a b = Try a (\_ -> b)

catch_ :: Effect f u -> (Expr f 'String -> Effect f u) -> Effect f u
catch_ a k = Try a (\e -> k (var e))

throw_ :: Expr f 'String -> Effect f v
throw_ = Throw

some :: Expr f u -> Expr f ('Option u)
some (Literal v) = Literal (ValueOption (Just v))
some x = UnsafeNullable x

none :: Expr f ('Option u)
none = Literal (ValueOption Nothing)

optionCase :: Expr f ('Option u) -> Expr f v -> (Expr f u -> Expr f v) -> Expr f v
optionCase opt noneBranch someBranch = OptionCase opt noneBranch (\x -> someBranch (var x))

optionCaseE :: Expr f ('Option u) -> Effect f v -> (Expr f u -> Effect f v) -> Effect f v
optionCaseE opt noneBranch someBranch = OptionCaseE opt noneBranch (\x -> someBranch (var x))

unsafeNullable :: Expr f u -> Expr f ('Option u)
unsafeNullable = UnsafeNullable

orElse :: Expr f ('Option u) -> Expr f u -> Expr f u
orElse o d = optionCase o d id

fromOption :: Expr f u -> Expr f ('Option u) -> Expr f u
fromOption = flip orElse

ok :: Expr f a -> Expr f ('Result e a)
ok (Literal v) = Literal (ValueResult (Right v))
ok x = ResultOk x

err :: Expr f e -> Expr f ('Result e a)
err (Literal v) = Literal (ValueResult (Left v))
err x = ResultErr x

resultCase :: Expr f ('Result e a) -> (Expr f e -> Expr f v) -> (Expr f a -> Expr f v) -> Expr f v
resultCase r onErr onOk = ResultCase r (\e -> onErr (var e)) (\a -> onOk (var a))

resultCaseE :: Expr f ('Result e a) -> (Expr f e -> Effect f v) -> (Expr f a -> Effect f v) -> Effect f v
resultCaseE r onErr onOk = ResultCaseE r (\e -> onErr (var e)) (\a -> onOk (var a))

typeOf :: Expr f u -> Expr f 'String
typeOf = TypeOf

not_ :: Expr f 'Bool -> Expr f 'Bool
not_ c = c .== false_

addEventListener :: Text -> Effect f ('MutableObject obj) -> (Expr f u -> Effect f a) -> EffectSyntax f ()
addEventListener name el handler =
  toSyntax_ $ callMethod el "addEventListener" (ArgExpr (string name) <: ArgEffect (LambdaE (\x -> handler (var x))) <: RecNil)

addEventListener_ :: Text -> Effect f ('MutableObject obj) -> EffectSyntax f (f 'Unit) -> EffectSyntax f ()
addEventListener_ name el body = addEventListener name el $ \_ -> stmts body

arg :: Expr f u -> Arg f u
arg = ArgExpr

class ToEffect f u a where
  toEffect :: a -> Effect f u

instance ToEffect f u (Effect f u) where
  toEffect = id

instance ToEffect f u (Expr f u) where
  toEffect = Lift

instance {-# OVERLAPPABLE #-} ToEffect f u (f u) where
  toEffect = Lift . Var

class ToExpr f u a where
  toExpr :: a -> Expr f u

instance ToExpr f u (Expr f u) where
  toExpr = id

instance ToExpr f u (f u) where
  toExpr = Var

hold :: Effect f u -> EffectSyntax f (Effect f u)
hold e = fmap (Lift . Var) (toSyntax e)

-- | Recover the record phantom from an object handle. Closed so
-- 'Effect'/'Expr' win over a bare PHOAS binder @f ('MutableObject r)@.
type family ObjectRow (a :: Type) :: Type where
  ObjectRow (Effect f ('MutableObject r)) = r
  ObjectRow (Expr f ('MutableObject r)) = r
  ObjectRow (f ('MutableObject r)) = r

-- | @o.k@. @OverloadedRecordDot@ uses 'HasField' on 'Effect' and 'Expr':
-- @n <- o.fullName@. PHOAS binders need 'get' or @(Var x).k@. Keys that
-- are not Haskell identifiers still use @get \@k@.
get :: forall k a f. (KnownSymbol k, ToEffect f ('MutableObject (ObjectRow a)) a)
    => a -> EffectSyntax f (Expr f (Field (ObjectRow a) k))
get o = Object.get @k @(ObjectRow a)
  (toEffect o :: Effect f ('MutableObject (ObjectRow a)))

set :: forall k a f. (KnownSymbol k, ToEffect f ('MutableObject (ObjectRow a)) a)
    => a -> Expr f (Field (ObjectRow a) k) -> EffectSyntax f (f 'Unit)
set o v = Object.set @k @(ObjectRow a)
  (toEffect o :: Effect f ('MutableObject (ObjectRow a)))
  v

-- | Untyped @o.k@. Prefer 'get' / 'set' (or record-dot) when the key is a 'Field'.
getProp :: Effect f ('MutableObject a) -> String -> EffectSyntax f (Expr f u)
getProp o name = fmap Var $ toSyntax $ unsafeObjectGet o name

setProp :: Effect f ('MutableObject a) -> String -> Expr f u -> EffectSyntax f (f 'Unit)
setProp o name v = toSyntax $ unsafeObjectAssign (unsafeObjectGet o name) (Lift v)

getProp' :: forall f o u. ToEffect f ('MutableObject ()) o => o -> String -> EffectSyntax f (Expr f u)
getProp' o name = getProp (toEffect o :: Effect f ('MutableObject ())) name

setProp' :: forall f o u. ToEffect f ('MutableObject ()) o => o -> String -> Expr f u -> EffectSyntax f (f 'Unit)
setProp' o name v = setProp (toEffect o :: Effect f ('MutableObject ())) name v

stmts :: EffectSyntax f (f 'Unit) -> Effect f 'Unit
stmts = fromSyntax

done :: EffectSyntax f (f 'Unit)
done = toSyntax noOp

whenS :: Expr f 'Bool -> EffectSyntax f (f 'Unit) -> EffectSyntax f (f 'Unit)
whenS c body = toSyntax $ when_ (expr c) (stmts body)

ifS :: Expr f 'Bool -> EffectSyntax f (f 'Unit) -> EffectSyntax f (f 'Unit) -> EffectSyntax f (f 'Unit)
ifS c t e = toSyntax $ IfE (expr c) (discard (stmts t)) (discard (stmts e))

whenSomeS :: Expr f ('Option u) -> (Expr f u -> EffectSyntax f (f 'Unit)) -> EffectSyntax f (f 'Unit)
whenSomeS opt k = toSyntax $ optionCaseE opt noOp (\x -> stmts (k x))

-- | Bind an optional effect, then run the body when it is present.
whenSomeE :: Effect f ('Option u) -> (Expr f u -> EffectSyntax f (f 'Unit)) -> EffectSyntax f (f 'Unit)
whenSomeE opt k = do
  o <- toSyntax opt
  whenSomeS (var o) k

call0 :: forall a f. ToEffect f ('Function 'Unit 'Unit) a => a -> EffectSyntax f (f 'Unit)
call0 fn = toSyntax (ApplyE (toEffect fn) noOp :: Effect f 'Unit)

infix 4 .==, .!=, .>, .<, .>=, .<=
infixr 3 .&&
infixr 2 .||

(.==), (.!=) :: Expr f a -> Expr f a -> Expr f 'Bool
(.==) = Eq
(.!=) = NEq

(.>), (.<), (.>=), (.<=) :: Comparable a => Expr f a -> Expr f a -> Expr f 'Bool
(.>) = GTh
(.<) = LTh
(.>=) = GTEq
(.<=) = LTEq

(.&&), (.||) :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool
(.&&) = And
(.||) = Or

rem_ :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
rem_ = Rem

bitAnd, bitOr, bitXor, shl, shr, ushr :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
bitAnd = BitAnd
bitOr = BitOr
bitXor = BitXor
shl = Shl
shr = Shr
ushr = UShr

-- | @parseInt(s, radix)@. The radix is required (Crockford appendix A).
parseInt_ :: Expr f 'String -> Expr f 'Number -> Expr f 'Number
parseInt_ = ExprBinary StdParseInt
