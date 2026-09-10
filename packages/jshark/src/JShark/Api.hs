{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | The JShark EDSL surface: literals, operators, functions, control
-- flow, FFI, and the 'EffectSyntax' do-notation bridge.
--
-- == Naming conventions
--
-- - Trailing @_@ avoids a Haskell/Prelude clash ('true_', 'not_', 'rem_')
--   or marks the statement-shaped member of a pair ('push_' vs
--   'Array.push', 'forRange_' vs 'forRange').
-- - An @S@ suffix marks the 'EffectSyntax' (do-notation) variant of an
--   'Effect'-based combinator: 'whenS'\/'ifS' run blocks of syntax,
--   'whenSomeS'\/'whenNoneS' branch on an 'Option' inside a do-block,
--   'addEventListenerS' takes a do-block handler. The un-suffixed names
--   compose 'Effect' values (see "JShark.Api.Types" for the two trees).
-- - Prime variants ('getProp'', 'setProp'') are the row-untyped forms:
--   any property name, unchecked field type.
module JShark.Api
  ( -- * Types
    Expr
  , Effect
  , EffectSyntax
  , Universe (..)
  , Value (..)
  , Arg (..)
  , Field
  , Comparable
  , KnownScalar
  , NumericU
  , structuralEq
  , structuralNEq
  , GroupBy

    -- * Literals
  , number
  , bigInt
  , bool
  , true_
  , false_
  , string
  , uint8Array
  , emptyArray
  , toString

    -- * Byte arrays
  , newByteArray
  , u8Index
  , u8Set
  , u8Fill
  , u8Copy
  , u8Len

    -- * Variables and lifting
  , var
  , expr
  , yield
  , arg
  , argEffect
  , ToEffect (..)
  , ToExpr (..)

    -- * Functions and binding
  , lambda
  , lambdaRow
  , fnLit
  , namedLambda
  , namedLambdaRow
  , ToFn (..)
  , ToLambda (..)
  , lambdaE
  , apply
  , apply2
  , apply3
  , applyNamed2
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
  , forRange
  , forRange_
  , arrayCallback
  , try_
  , catch_
  , throw_

    -- * Option
  , some
  , none
  , optionCase
  , optionCaseE
  , whenSomeS
  , whenNoneS
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
  , ffiExpr
  , callMethod
  , assign

    -- * Objects
  , emptyObject
  , newObject
  , get
  , set
  , HasField (..)
  , getProp
  , setProp
  , getProp'
  , setProp'

    -- * Events / window
  , Event
  , window
  , host
  , locationHash
  , onClick
  , onClick_
  , addEventListener
  , addEventListener_
  , addEventListenerS
  , eventKey
  , eventCode
  , eventRepeat
  , eventPointerId
  , eventClientX
  , eventClientY
  , eventButton
  , eventShiftKey
  , eventOffsetX
  , eventOffsetY

    -- * Syntax
  , noOp
  , discard
  , hold
  , bindExpr
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
  , quot_
  , parseInt_
  , toNumber
  , toBigInt
  , fromBigInt
  , parseBigInt_
  , hvm2Kernel
  , loadHvm2Wasm
  )
where

import Data.Array.Byte (ByteArray)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownSymbol)
import JShark.Api.Caller (callerBinderHint)
import JShark.Api.Params
  ( NamedLambdaRow (..)
  , ParamRec
  , ToFn (..)
  , ToLambda (..)
  , fnLit
  , lambdaRow
  , toFn
  , toLambda
  )
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types
import JShark.Object hiding (get, set)
import qualified JShark.Object as Object

data Window

type instance Field Window "location.host" = 'String

type instance Field Window "location.hash" = 'String

-- | The event object handed to 'addEventListener' callbacks. Read it
-- with the typed accessors ('eventKey', 'eventCode', …); @target@ is
-- typed in "JShark.Dom" ('JShark.Dom.eventTarget').
data Event

type instance Field Event "key" = 'String

type instance Field Event "code" = 'String

type instance Field Event "repeat" = 'Bool

type instance Field Event "pointerId" = 'Number

type instance Field Event "clientX" = 'Number

type instance Field Event "clientY" = 'Number

type instance Field Event "button" = 'Number

type instance Field Event "offsetX" = 'Number

type instance Field Event "offsetY" = 'Number

type instance Field Event "shiftKey" = 'Bool

eventKey ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'String)
eventKey o = Object.get @"key" @Event (toEffect o)

eventCode ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'String)
eventCode o = Object.get @"code" @Event (toEffect o)

eventRepeat ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'Bool)
eventRepeat o = Object.get @"repeat" @Event (toEffect o)

eventPointerId ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'Number)
eventPointerId o = Object.get @"pointerId" @Event (toEffect o)

eventClientX ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'Number)
eventClientX o = Object.get @"clientX" @Event (toEffect o)

eventClientY ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'Number)
eventClientY o = Object.get @"clientY" @Event (toEffect o)

eventButton ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'Number)
eventButton o = Object.get @"button" @Event (toEffect o)

eventShiftKey ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'Bool)
eventShiftKey o = Object.get @"shiftKey" @Event (toEffect o)

eventOffsetX ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'Number)
eventOffsetX o = Object.get @"offsetX" @Event (toEffect o)

eventOffsetY ::
  ToEffect f ('MutableObject Event) o => o -> EffectSyntax f (Expr f 'Number)
eventOffsetY o = Object.get @"offsetY" @Event (toEffect o)

window :: Effect f ('MutableObject Window)
window = unsafeObject "window"

host :: EffectSyntax f (Expr f 'String)
host = Object.get @"location.host" window

locationHash :: EffectSyntax f (Expr f 'String)
locationHash = Object.get @"location.hash" window

emptyObject :: Effect f ('MutableObject ())
emptyObject = newObject

onClick ::
  Effect f ('MutableObject obj) -> (f 'Unit -> Effect f a) -> EffectSyntax f ()
onClick el f = toSyntax_ $ unsafeObjectAssign (unsafeObjectGet el "onclick") (LambdaE f)

onClick_ ::
  Effect f ('MutableObject obj) -> EffectSyntax f (f 'Unit) -> EffectSyntax f ()
onClick_ el body = onClick el $ \_ -> stmts body

-- | Raw JS call. Codegen appends @(...)@ for the argument list; with
--   'RecNil' that is a trailing @()@ (e.g. @performance.now@ →
--   @performance.now()@). Parenthesized callees (IIFEs) stay 'FFICall'.
ffi :: Text -> Rec (Arg f) us -> Effect f v
ffi s = FFI (classifyFFI s)

-- | Raw JS expression. With 'RecNil', codegen emits the string as-is (no
--   trailing @()@). Use for comparisons, @typeof@, property reads, etc.
ffiExpr :: Text -> Rec (Arg f) us -> Effect f v
ffiExpr s = FFI (FFIExpr s)

-- | Classify a string for 'ffi'. Unparenthesized arrows become 'FFILambda';
--   everything else (including parenthesized IIFEs) becomes 'FFICall'.
classifyFFI :: Text -> FFIForm
classifyFFI s
  | T.head s == '(' = FFICall s
  | isUnparenthesizedArrow s = FFILambda s
  | otherwise = FFICall s

isUnparenthesizedArrow :: Text -> Bool
isUnparenthesizedArrow s =
  case T.findIndex (== '=') s of
    Just i -> T.length s > i + 1 && T.index s (i + 1) == '>'
    Nothing -> False

-- | Call @object.method(args...)@ — the receiver is an
-- Effect handle (e.g. a DOM element), the method a free-text name.
callMethod :: Effect f object -> Text -> Rec (Arg f) us -> Effect f u
callMethod o n = CallMethod o n

-- | @Object.assign(dst, src)@. In-place copy; @dst@ keeps its identity
-- (needed when a closure already captured @dst@).
assign :: Effect f u -> Effect f u -> EffectSyntax f (f 'Unit)
assign dst src = do
  toSyntax_ $ ffi "Object.assign" (ArgEffect dst <: ArgEffect src <: RecNil)
  done

-- | Lift an Expr into effect position: @x + 1@ stays pure,
-- @expr (x + 1)@ can appear where an Effect is expected.
expr :: Expr f u -> Effect f u
expr = Lift

yield :: Expr f u -> EffectSyntax f (f u)
yield = toSyntax . Lift

apply :: Expr f ('Function u v) -> Expr f u -> Expr f v
apply = Apply

-- | Curried application (@f(x)(y)@ in JS). Prefer 'applyNamed2' for hoisted
-- two-arg helpers.
apply2 ::
  Expr f ('Function a ('Function b c)) -> Expr f a -> Expr f b -> Expr f c
apply2 f x y = apply (apply f x) y

apply3 ::
  Expr f ('Function a ('Function b ('Function c d)))
  -> Expr f a
  -> Expr f b
  -> Expr f c
  -> Expr f d
apply3 f x y z = apply (apply2 f x y) z

-- | Uncurried call for hoisted two-arg helpers (@f(x, y)@, not @f(x)(y)@).
--
-- Use with helpers from 'namedLambdaRow' only; 'apply2' emits curried JS.
applyNamed2 ::
  Expr f ('Function a ('Function b r))
  -> Expr f a
  -> Expr f b
  -> Expr f r
applyNamed2 f x y = expr3 FixCall2 f x y

-- | Re-embed a reified value (an EffectSyntax bind result) as an
-- expression: @x <- toSyntax e; ... (var x + 1)@.
var :: f u -> Expr f u
var = Var

-- | Anonymous curried lambda (@function(x){…}@).
lambda :: (Expr f u -> Expr f v) -> Expr f ('Function u v)
lambda f = Lambda noLamInfo (\x -> f (var x))

-- | Named unary lambda; codegen hoists a shared @$name@ helper.
namedLambda :: Text -> (Expr f u -> Expr f v) -> Expr f ('Function u v)
namedLambda name f = Lambda (LamInfo (Just name) Nothing) (\x -> f (var x))

-- | Named binary lambda over a 'ParamRec' row.
--
-- Emits one uncurried JS @function(a, b)@. Call sites must use
-- 'applyNamed2', not 'apply2'. Rows are binary-only ('NamedLambdaRow').
namedLambdaRow ::
  NamedLambdaRow row r fn =>
  Text
  -> (ParamRec f row -> Expr f r)
  -> Expr f fn
namedLambdaRow = namedLambdaFromRow

-- | An Effect-valued function value: the body may bind and
-- sequence effects; the last value becomes the @return@.
lambdaE :: (Effect f u -> Effect f v) -> Effect f ('Function u v)
lambdaE f = LambdaE (\x -> f (Lift (var x)))

-- | Recursive @let@. The right-hand side must be productive — a 'lambda',
-- or a value that does not force the binder. 'JShark.evaluate' ties the knot,
-- so a strict self-reference (@letRec (\\x -> x + 1)@) diverges.
letRec :: (Expr f u -> Expr f u) -> (Expr f u -> Expr f v) -> Expr f v
letRec r b = LetRec (\x -> r (var x)) (\x -> b (var x))

bindRec ::
  (Effect f u -> Effect f u) -> (Effect f u -> Effect f v) -> Effect f v
bindRec r b = BindRec (\x -> r (Lift (var x))) (\x -> b (Lift (var x)))

-- | Recursively bind a zero-argument effectful function, then run the body.
-- @loop0 paint wire@ is @const render = function(){ paint(render); }; wire(render)@.
loop0 ::
  (Effect f ('Function 'Unit 'Unit) -> EffectSyntax f (f 'Unit))
  -> (Effect f ('Function 'Unit 'Unit) -> EffectSyntax f (f 'Unit))
  -> EffectSyntax f (f 'Unit)
loop0 rec body =
  toSyntax $
    bindRec
      (\f -> lambdaE (\_ -> stmts (rec f)))
      (\f -> stmts (body f))

-- | A JS number literal (IEEE double).
number :: Double -> Expr f 'Number
number = Literal . ValueNumber
{-# INLINE number #-}

-- | Exact integer literal. Codegen emits @Nn@ (negatives parenthesized).
bigInt :: Integer -> Expr f 'BigInt
bigInt = Literal . ValueBigInt

-- | A JS boolean literal.
bool :: Bool -> Expr f 'Bool
bool = Literal . ValueBool
{-# INLINE bool #-}

true_, false_ :: Expr f 'Bool
true_ = bool True
false_ = bool False
{-# INLINE true_ #-}
{-# INLINE false_ #-}

-- | A JS string literal.
string :: Text -> Expr f 'String
string = Literal . ValueString
{-# INLINE string #-}

-- | @new Uint8Array([…])@ from a host 'ByteArray'. All-zero buffers codegen
-- as @new Uint8Array(n)@; non-zero literals keep the element list.
uint8Array :: ByteArray -> Expr f 'Uint8Array
uint8Array = Literal . ValueUint8Array

-- | @new Uint8Array(n)@ — @n@ zeroed bytes.
--
-- 'uint8Array' is for bytes the host already has; this is for a buffer whose
-- size is known but whose contents are not, which is what a JS API filling a
-- buffer wants. Allocation has identity: bind it; two occurrences would be
-- two arrays. JS can write the object.
newByteArray ::
  Expr f 'Number -> Effect f 'Uint8Array
newByteArray n =
  FFI (FFILambda "n => new Uint8Array(n)") (arg n <: RecNil)

u8Index :: Expr f 'Uint8Array -> Expr f 'Number -> Expr f 'Number
u8Index = U8Index

u8Set ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f 'Unit
u8Set = U8Set

u8Fill :: Expr f 'Uint8Array -> Expr f 'Number -> Effect f 'Unit
u8Fill = U8Fill

-- | @dst.set(src)@ — copy one @Uint8Array@ into another of the same length.
u8Copy :: Expr f 'Uint8Array -> Expr f 'Uint8Array -> Effect f 'Unit
u8Copy dst src =
  FFI
    (FFILambda "(d,s)=>{d.set(s);}")
    (arg dst <: arg src <: RecNil)

u8Len :: Expr f 'Uint8Array -> Expr f 'Number
u8Len = expr1 FixU8Len

-- | @for (let i = start; i < end; i++) body@ — the loop index is
-- a number; the body is an Effect.
forRange ::
  Expr f 'Number
  -> Expr f 'Number
  -> (Expr f 'Number -> Effect f 'Unit)
  -> Effect f 'Unit
forRange start end body = ForRange start end (\i -> body (var i))

forRange_ ::
  Expr f 'Number
  -> Expr f 'Number
  -> (Expr f 'Number -> EffectSyntax f (f 'Unit))
  -> EffectSyntax f (f 'Unit)
forRange_ start end f = toSyntax $ forRange start end (\x -> stmts (f x))

emptyArray :: Expr f ('Array u)
emptyArray = Literal (ValueArray [])

toString :: Expr f u -> Expr f 'String
toString = Show

-- | @arr.method(function(x){…})@ with an 'Effect' callback.
arrayCallback ::
  Text -> Expr f ('Array u) -> (Expr f u -> Effect f v) -> Effect f w
arrayCallback name arr f =
  callMethod (expr arr) name (ArgEffect (LambdaE (\x -> f (var x))) <: RecNil)

-- | @arr.forEach(x => body)@ — iterate an array for effect; the
-- callback is inlined.
forEach :: Expr f ('Array u) -> (Expr f u -> Effect f u') -> Effect f 'Unit
forEach = arrayCallback "forEach"

forEach_ ::
  Expr f ('Array u)
  -> (Expr f u -> EffectSyntax f (f 'Unit))
  -> EffectSyntax f (f 'Unit)
forEach_ arr f = toSyntax $ forEach arr (\x -> stmts (f x))

noOp :: Effect f 'Unit
noOp = expr (Literal ValueUnit)

-- | Const-bind inside a pure expression. Single-use lets inline;
-- multi-use lets stay named, and the binder keeps the Haskell name of
-- the enclosing function under readableConfig.
let_ :: HasCallStack => Expr f u -> (Expr f u -> Expr f v) -> Expr f v
let_ (Literal v) f = f (Literal v)
let_ (Var x) f = f (Var x)
let_ e f = Let (callerBinderHint) e (\x -> f (var x))
{-# NOINLINE let_ #-}

-- | The conditional operator @c ? t : e@ on pure values.
if_ :: Expr f 'Bool -> Expr f u -> Expr f u -> Expr f u
if_ (Literal (ValueBool True)) t _ = t
if_ (Literal (ValueBool False)) _ e = e
if_ c t e = If c t e
{-# INLINE [1] if_ #-}

-- | Effectful conditional. Lift an 'Expr' test with 'expr'.
ifE :: Effect f 'Bool -> Effect f u -> Effect f u -> Effect f u
ifE = IfE

-- | @switch (s) { case k: …; default: … }@. First matching label wins.
-- Arms do not fall through. Statement arms that are polymorphic ('FFI',
-- 'CallMethod') need 'discard', same as 'ifE'.
stringCaseE ::
  Expr f 'String -> [(Text, Effect f v)] -> Effect f v -> Effect f v
stringCaseE = StringCaseE

-- | Drop a result, forcing 'Unit'. Lets statement-'if' be 'IfE' of two
-- unit arms (polymorphic 'FFI' / 'CallMethod' are not unit witnesses).
discard :: Effect f u -> Effect f 'Unit
discard e = ThenE e noOp

-- | Run an effect only when the (effectful) condition holds —
-- @if (c) { body }@.
when_ :: Effect f 'Bool -> Effect f 'Unit -> Effect f 'Unit
when_ c t = IfE c (discard t) noOp

-- | @while (c) { body }@ with effectful condition and body.
while_ :: Effect f 'Bool -> Effect f 'Unit -> Effect f 'Unit
while_ = While

-- | @try { a } catch (e) { b }@ — both arms share the result type;
-- the caught value stays unnamed.
try_ :: Effect f u -> Effect f u -> Effect f u
try_ a b = Try a (\_ -> b)

catch_ :: Effect f u -> (Expr f 'String -> Effect f u) -> Effect f u
catch_ a k = Try a (\e -> k (var e))

-- | @throw msg@ — never returns; the result type is free.
throw_ :: Expr f 'String -> Effect f v
throw_ = Throw

-- | Wrap a value: JS @null@ means missing, so @some x@ is just
-- @x@ and none is @null@ (see fromOption).
some :: Expr f u -> Expr f ('Option u)
some (Literal v) = Literal (ValueOption (Just v))
some x = UnsafeNullable x

-- | The missing option: JS @null@.
none :: Expr f ('Option u)
none = Literal (ValueOption Nothing)

-- | Branch on an Option in expression position:
-- @o === null ? n : some@-style ternary.
optionCase ::
  Expr f ('Option u) -> Expr f v -> (Expr f u -> Expr f v) -> Expr f v
optionCase opt noneBranch someBranch = OptionCase opt noneBranch (\x -> someBranch (var x))

optionCaseE ::
  Expr f ('Option u) -> Effect f v -> (Expr f u -> Effect f v) -> Effect f v
optionCaseE opt noneBranch someBranch = OptionCaseE opt noneBranch (\x -> someBranch (var x))

unsafeNullable :: Expr f u -> Expr f ('Option u)
unsafeNullable = UnsafeNullable

-- | @o ?? d@ — the option if present, the default otherwise.
orElse :: Expr f ('Option u) -> Expr f u -> Expr f u
orElse o d = optionCase o d id

fromOption :: Expr f u -> Expr f ('Option u) -> Expr f u
fromOption = flip orElse

-- | The success side of a Result: @{ok: true, value: a}@.
ok :: Expr f a -> Expr f ('Result e a)
ok (Literal v) = Literal (ValueResult (Right v))
ok x = ResultOk x

-- | The failure side of a Result: @{ok: false, value: e}@.
err :: Expr f e -> Expr f ('Result e a)
err (Literal v) = Literal (ValueResult (Left v))
err x = ResultErr x

-- | Branch on a Result in expression position, reading @.ok@
-- and unwrapping @.value@.
resultCase ::
  Expr f ('Result e a)
  -> (Expr f e -> Expr f v)
  -> (Expr f a -> Expr f v)
  -> Expr f v
resultCase r onErr onOk = ResultCase r (\e -> onErr (var e)) (\a -> onOk (var a))

resultCaseE ::
  Expr f ('Result e a)
  -> (Expr f e -> Effect f v)
  -> (Expr f a -> Effect f v)
  -> Effect f v
resultCaseE r onErr onOk = ResultCaseE r (\e -> onErr (var e)) (\a -> onOk (var a))

typeOf :: Expr f u -> Expr f 'String
typeOf = TypeOf

not_ :: Expr f 'Bool -> Expr f 'Bool
not_ c = c .== false_

addEventListener ::
  Text
  -> Effect f ('MutableObject obj)
  -> (Expr f ('MutableObject Event) -> Effect f a)
  -> EffectSyntax f ()
addEventListener name el handler =
  toSyntax_ $
    callMethod
      el
      "addEventListener"
      (ArgExpr (string name) <: ArgEffect (LambdaE (\x -> handler (var x))) <: RecNil)

-- | 'addEventListener' with the handler written directly in
-- 'EffectSyntax' (no @stmts@ wrap needed).
addEventListenerS ::
  Text
  -> Effect f ('MutableObject obj)
  -> (Expr f ('MutableObject Event) -> EffectSyntax f (f 'Unit))
  -> EffectSyntax f ()
addEventListenerS name el handler =
  addEventListener name el (stmts . handler)

addEventListener_ ::
  Text
  -> Effect f ('MutableObject obj)
  -> EffectSyntax f (f 'Unit)
  -> EffectSyntax f ()
addEventListener_ name el body = addEventListener name el $ \_ -> stmts body

arg :: Expr f u -> Arg f u
arg = ArgExpr

-- | Lift an effectful computation into an FFI argument position
-- (rendered as an inline callback).
argEffect :: Effect f u -> Arg f u
argEffect = ArgEffect

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
hold = fmap Lift . bindExpr

-- | Recover the record phantom from an object handle. Closed so
-- 'Effect'/'Expr' win over a bare PHOAS binder @f ('MutableObject r)@.
type family ObjectRow (a :: Type) :: Type where
  ObjectRow (Effect f ('MutableObject r)) = r
  ObjectRow (Expr f ('MutableObject r)) = r
  ObjectRow (f ('MutableObject r)) = r

-- | @o.k@. @OverloadedRecordDot@ uses 'HasField' on 'Effect' and 'Expr':
-- @n <- o.fullName@. PHOAS binders need 'get' or @(Var x).k@. Keys that
-- are not Haskell identifiers still use @get \@k@.
get ::
  forall k a f.
  (KnownSymbol k, ToEffect f ('MutableObject (ObjectRow a)) a) =>
  a -> EffectSyntax f (Expr f (Field (ObjectRow a) k))
get o =
  Object.get @k @(ObjectRow a)
    (toEffect o :: Effect f ('MutableObject (ObjectRow a)))

set ::
  forall k a f.
  (KnownSymbol k, ToEffect f ('MutableObject (ObjectRow a)) a) =>
  a -> Expr f (Field (ObjectRow a) k) -> EffectSyntax f (f 'Unit)
set o v =
  Object.set @k @(ObjectRow a)
    (toEffect o :: Effect f ('MutableObject (ObjectRow a)))
    v

-- | Untyped @o.k@. Prefer 'get' / 'set' (or record-dot) when the key is a 'Field'.
getProp :: Effect f ('MutableObject a) -> String -> EffectSyntax f (Expr f u)
getProp o name = bindExpr $ unsafeObjectGet o name

setProp ::
  Effect f ('MutableObject a) -> String -> Expr f u -> EffectSyntax f (f 'Unit)
setProp o name v = toSyntax $ unsafeObjectAssign (unsafeObjectGet o name) (Lift v)

getProp' ::
  forall f o u.
  ToEffect f ('MutableObject ()) o => o -> String -> EffectSyntax f (Expr f u)
getProp' o name = getProp (toEffect o :: Effect f ('MutableObject ())) name

setProp' ::
  forall f o u.
  ToEffect f ('MutableObject ()) o =>
  o -> String -> Expr f u -> EffectSyntax f (f 'Unit)
setProp' o name v = setProp (toEffect o :: Effect f ('MutableObject ())) name v

stmts :: EffectSyntax f (f 'Unit) -> Effect f 'Unit
stmts = fromSyntax

done :: EffectSyntax f (f 'Unit)
done = toSyntax noOp

whenS :: Expr f 'Bool -> EffectSyntax f (f 'Unit) -> EffectSyntax f (f 'Unit)
whenS (Literal (ValueBool True)) body = body
whenS (Literal (ValueBool False)) _ = done
-- Same as 'ifS': do not route through 'when_' / 'discard'. 'discard'
-- under a constant-folded 'IfE' can drop impure FFI preludes.
whenS c body = toSyntax $ IfE (expr c) (stmts body) noOp
{-# INLINE [1] whenS #-}

ifS ::
  Expr f 'Bool
  -> EffectSyntax f (f 'Unit)
  -> EffectSyntax f (f 'Unit)
  -> EffectSyntax f (f 'Unit)
-- Branches are already 'Effect' 'Unit' via 'stmts'; do not 'discard' here.
-- 'discard(stmts …)' under a constant-folded 'IfE' arm can drop impure FFI
-- preludes (see 'stepGrid' region copy).
ifS (Literal (ValueBool True)) t _ = t
ifS (Literal (ValueBool False)) _ e = e
ifS c t e = toSyntax $ IfE (expr c) (stmts t) (stmts e)
{-# INLINE [1] ifS #-}

whenSomeS ::
  Expr f ('Option u)
  -> (Expr f u -> EffectSyntax f (f 'Unit))
  -> EffectSyntax f (f 'Unit)
whenSomeS opt k = toSyntax $ optionCaseE opt noOp (\x -> stmts (k x))

-- | Run the body when the option is 'none' (the 'whenSomeS' complement).
whenNoneS ::
  Expr f ('Option u)
  -> EffectSyntax f (f 'Unit)
  -> EffectSyntax f (f 'Unit)
whenNoneS opt k = toSyntax $ optionCaseE opt (stmts k) (\_ -> noOp)

-- | Bind an optional effect, then run the body when it is present.
whenSomeE ::
  Effect f ('Option u)
  -> (Expr f u -> EffectSyntax f (f 'Unit))
  -> EffectSyntax f (f 'Unit)
whenSomeE opt k = do
  o <- bindExpr opt
  whenSomeS o k

call0 ::
  forall a f.
  ToEffect f ('Function 'Unit 'Unit) a => a -> EffectSyntax f (f 'Unit)
call0 f = toSyntax (ApplyE (toEffect f) noOp :: Effect f 'Unit)

infix 4 .==, .!=, .>, .<, .>=, .<=

infixr 3 .&&

infixr 2 .||

(.==), (.!=) :: KnownScalar a => Expr f a -> Expr f a -> Expr f 'Bool
(.==) = mkEq
(.!=) = mkNEq
{-# INLINE [1] (.==) #-}
{-# INLINE [1] (.!=) #-}

(.>)
  , (.<)
  , (.>=)
  , (.<=) ::
    Comparable a => Expr f a -> Expr f a -> Expr f 'Bool
(.>) = mkGTh
(.<) = mkLTh
(.>=) = mkGTEq
(.<=) = mkLTEq
{-# INLINE [1] (.>) #-}
{-# INLINE [1] (.<) #-}
{-# INLINE [1] (.>=) #-}
{-# INLINE [1] (.<=) #-}

(.&&), (.||) :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool
(.&&) = andE
(.||) = orE
{-# INLINE (.&&) #-}
{-# INLINE (.||) #-}

ushr :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
ushr = ushrE
{-# INLINE [1] ushr #-}

-- | BigInt truncating division (JS @/@). Number uses 'Fractional' @/@.
quot_ :: Expr f 'BigInt -> Expr f 'BigInt -> Expr f 'BigInt
quot_ x y = Std (Kernel (KBig BQuot x y))

-- | @parseInt(s, radix)@. The radix is required (Crockford appendix A).
parseInt_ :: Expr f 'String -> Expr f 'Number -> Expr f 'Number
parseInt_ s r = expr2 FixParseInt s r

-- | JS @Number(x)@ coercion on strings (unlike 'parseInt_', no radix,
-- accepts decimals; yields NaN on garbage).
toNumber :: Expr f 'String -> EffectSyntax f (Expr f 'Number)
toNumber x = fmap var (toSyntax (ffi "Number" (arg x <: RecNil)))

-- | @BigInt(n)@. Throws when @n@ is not an integer Number.
toBigInt :: Expr f 'Number -> Expr f 'BigInt
toBigInt = expr1 FixToBigInt

-- | @Number(n)@. Large values may lose precision.
fromBigInt :: Expr f 'BigInt -> Expr f 'Number
fromBigInt = expr1 FixFromBigInt

-- | @BigInt(s)@. Accepts an optional sign and @0x@ / @0b@ / @0o@ prefixes.
parseBigInt_ :: Expr f 'String -> Expr f 'BigInt
parseBigInt_ = expr1 FixParseBigInt

-- | Mark a closed pure kernel for HVM2 compilation ('Hvm2Kernel' in JS AST).
hvm2Kernel :: Text -> ClosedExpr u -> Expr f u
hvm2Kernel name k = Hvm2Kernel name k

-- | Fetch and instantiate the HVM2 WASM module at @url@, setting
-- @globalThis.__jsharkHvm2.exports@ for 'hvm2Kernel' call sites.
-- Returns a Promise (awaited by the Bun runner and in async JS hosts).
loadHvm2Wasm :: Expr f 'String -> EffectSyntax f ()
loadHvm2Wasm url = toSyntax_ $ ffi hvm2LoadWasmFFI (arg url <: RecNil)

hvm2LoadWasmFFI :: Text
hvm2LoadWasmFFI =
  "url=>(async()=>{"
    <> "if(!globalThis.WebAssembly)throw new Error(\"WebAssembly unavailable\");"
    <> "const r=await fetch(url);"
    <> "if(!r.ok)throw new Error(\"HVM2 wasm fetch failed: \"+url);"
    <> "const b=await r.arrayBuffer();"
    <> "const{instance:i}=await WebAssembly.instantiate(b,{});"
    <> "globalThis.__jsharkHvm2={exports:i.exports}"
    <> "})()"
