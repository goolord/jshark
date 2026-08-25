{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
  , seedLiveCells
  , seedSoupRegion
  , u8Index
  , u8Set
  , u8Fill
  , u8FillRegion
  , u8Copy
  , u8CopyRegion
  , u8Len
  , fillRgbaImageData
  , rgbaPixelSet
  , rgbaFillRect
  , paintGridCells

    -- * Variables and lifting
  , var
  , expr
  , yield
  , arg
  , ToEffect (..)
  , ToExpr (..)

    -- * Functions and binding
  , lambda
  , lambdaRow
  , fnLit
  , ToFn (..)
  , ToLambda (..)
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
  , HasField (..)
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
  , toBigInt
  , fromBigInt
  , parseBigInt_
  )
where

import Data.Array.Byte (ByteArray)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import GHC.TypeLits (KnownSymbol)
import JShark.Object hiding (get, set)
import qualified JShark.Object as Object
import JShark.Params
  ( ToFn (..)
  , ToLambda (..)
  , fnLit
  , lambdaRow
  , toFn
  , toLambda
  )
import JShark.Rec (Rec (..), (<:))
import JShark.Types

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

onClick ::
  Effect f ('MutableObject obj) -> (f 'Unit -> Effect f a) -> EffectSyntax f ()
onClick el f = toSyntax_ $ unsafeObjectAssign (unsafeObjectGet el "onclick") (LambdaE f)

onClick_ ::
  Effect f ('MutableObject obj) -> EffectSyntax f (f 'Unit) -> EffectSyntax f ()
onClick_ el body = onClick el $ \_ -> stmts body

ffi :: String -> Rec (Arg f) us -> Effect f v
ffi s = FFI (classifyFFI s)

classifyFFI :: String -> FFIForm
classifyFFI s@('(' : _) = FFICall (T.pack s)
classifyFFI s
  | isUnparenthesizedArrow s = FFILambda (T.pack s)
  | otherwise = FFICall (T.pack s)

isUnparenthesizedArrow :: String -> Bool
isUnparenthesizedArrow s =
  case break (== '=') s of
    (_, '=' : '>' : _) -> True
    _ -> False

callMethod :: Effect f object -> String -> Rec (Arg f) us -> Effect f u
callMethod o n = CallMethod o (T.pack n)

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

var :: f u -> Expr f u
var = Var

lambda :: (Expr f u -> Expr f v) -> Expr f ('Function u v)
lambda f = Lambda (\x -> f (var x))

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

number :: Double -> Expr f 'Number
number = Literal . ValueNumber

-- | Exact integer literal. Codegen emits @Nn@ (negatives parenthesized).
bigInt :: Integer -> Expr f 'BigInt
bigInt = Literal . ValueBigInt

bool :: Bool -> Expr f 'Bool
bool = Literal . ValueBool

true_, false_ :: Expr f 'Bool
true_ = bool True
false_ = bool False

string :: Text -> Expr f 'String
string = Literal . ValueString

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

-- | Must stay in sync with 'Types' soup seed constants in the Life example.
soupLcgMult, soupLcgInc, soupLcgModulus :: Int
soupLcgMult = 1103515245
soupLcgInc = 12345
soupLcgModulus = 0x7fffffff

soupDensityLit :: Double
soupDensityLit = 0.20

soupSeedJs :: Text
soupSeedJs =
  T.concat
    [ "(a,x0,y0,w,h,gw,rng0)=>{let rng=BigInt(rng0|0);for(let y=y0|0;y<y0+h;y++)for(let x=x0|0;x<x0+w;x++){rng=(BigInt("
    , T.pack (show soupLcgMult)
    , ")*rng+BigInt("
    , T.pack (show soupLcgInc)
    , "))%BigInt("
    , T.pack (show soupLcgModulus)
    , ");if(Number(rng)/"
    , T.pack (show soupLcgModulus)
    , "<"
    , T.pack (show soupDensityLit)
    , ")a[y*gw+x]=1;}}"
    ]

-- | Stamp live cells into zeroed @alive@ / @species@ buffers. Each pair is
-- @(linearIndex, speciesId)@; @alive[index]@ is set to @1@.
seedLiveCells ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> [(Int, Word8)]
  -> Effect f 'Unit
seedLiveCells alive species cells =
  FFI
    ( FFILambda
        "(a,s,p)=>{for(let k=0;k<p.length;k++){const t=p[k];a[t[0]]=1;s[t[0]]=t[1];}}"
    )
    ( arg alive
        <: arg species
        <: arg (indexSpeciesPairs cells)
        <: RecNil
    )

-- | Fill every RGBA pixel in an @ImageData.data@ buffer.
fillRgbaImageData ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f 'Unit
fillRgbaImageData pixels r g b a =
  FFI
    ( FFILambda
        "(p,r,g,b,a)=>{for(let i=0;i<p.length;i+=4){p[i]=r;p[i+1]=g;p[i+2]=b;p[i+3]=a;}}"
    )
    (arg pixels <: arg r <: arg g <: arg b <: arg a <: RecNil)

-- | Random soup in a rectangular region. Matches 'Patterns.seedCell' LCG (@20%@
-- live, species untouched — caller should stamp catalog ids afterward).
seedSoupRegion ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f 'Unit
seedSoupRegion alive seedOx seedOy seedW seedH gridW rng0 =
  FFI
    (FFILambda soupSeedJs)
    ( arg alive
        <: arg seedOx
        <: arg seedOy
        <: arg seedW
        <: arg seedH
        <: arg gridW
        <: arg rng0
        <: RecNil
    )

indexSpeciesPairs :: [(Int, Word8)] -> Expr f ('Array ('Array 'Number))
indexSpeciesPairs pairs =
  Literal $
    ValueArray
      [ ValueArray [ValueNumber (fromIntegral i), ValueNumber (fromIntegral w)]
      | (i, w) <- pairs
      ]

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

-- | Copy a half-open bbox @\[x0,x1)×\[y0,y1)@ row-wise (@y * gridW + x@).
u8CopyRegion ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f 'Unit
u8CopyRegion dst src gridW x0 y0 x1 y1 =
  FFI
    ( FFILambda
        "(d,s,w,x0,y0,x1,y1)=>{for(let y=y0|0;y<(y1|0);y++){const row=y*(w|0)|0;d.set(s.subarray(row+(x0|0),row+(x1|0)),row+(x0|0));}}"
    )
    ( arg dst
        <: arg src
        <: arg gridW
        <: arg x0
        <: arg y0
        <: arg x1
        <: arg y1
        <: RecNil
    )

-- | Write one premultiplied-ready RGBA pixel (@0xAABBGGRR@) into @ImageData.data@.
rgbaPixelSet ::
  Expr f 'Uint8Array -> Expr f 'Number -> Expr f 'Number -> Effect f 'Unit
rgbaPixelSet pixels idx color =
  FFI
    ( FFILambda
        "(p,i,c)=>{const o=(i<<2)|0;p[o]=c&255;p[o+1]=(c>>8)&255;p[o+2]=(c>>16)&255;p[o+3]=(c>>>24)&255;}"
    )
    (arg pixels <: arg idx <: arg color <: RecNil)

-- | Fill a solid @sw×sh@ block clipped to the canvas buffer.
rgbaFillRect ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f 'Unit
rgbaFillRect pixels canvasW canvasH x y sw sh color =
  FFI
    ( FFILambda
        "(p,w,h,x,y,s,t,c)=>{const x0=Math.max(0,0|x),y0=Math.max(0,0|y);const x1=Math.min(w,(0|x)+(0|s)),y1=Math.min(h,(0|y)+(0|t));for(let yy=y0;yy<y1;yy++){const row=yy*w|0;for(let xx=x0;xx<x1;xx++){const o=(row+xx)<<2;p[o]=c&255;p[o+1]=(c>>8)&255;p[o+2]=(c>>16)&255;p[o+3]=(c>>>24)&255;}}}"
    )
    ( arg pixels
        <: arg canvasW
        <: arg canvasH
        <: arg x
        <: arg y
        <: arg sw
        <: arg sh
        <: arg color
        <: RecNil
    )

-- | Paint live/changed grid cells into an RGBA canvas buffer in one JS pass.
-- Writes dirty-rect fields onto @out@ (@dirtyCx0@ … @dirtyPainted@) for blitting.
paintGridCells ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f ('Array 'Number)
  -> Expr f ('Array 'Number)
  -> Expr f 'Bool
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f u
  -> Effect f 'Unit
paintGridCells
  pixels
  cw
  ch
  paletteRgba
  alive
  species
  w
  scale
  panX
  panY
  bg
  liveList
  changedList
  fullRedraw
  visX0
  visX1
  visY0
  visY1
  out =
  FFI
    ( FFILambda
        ( "(p,cw,ch,pal,alive,species,w,scale,panX,panY,bg,live,changed,full,vx0,vx1,vy0,vy1,out)=>{"
            <> "let cx0=1e9,cy0=1e9,cx1=-1,cy1=-1,painted=false;"
            <> "const bgR=bg&255,bgG=(bg>>8)&255,bgB=(bg>>16)&255,bgA=bg>>>24;"
            <> "const bump=(a,b,c,d)=>{if(a<cx0)cx0=a;if(b<cy0)cy0=b;if(c>cx1)cx1=c;if(d>cy1)cy1=d;};"
            <> "const paint=(gi)=>{"
            <> "const x=gi%w,y=(gi/w)|0;"
            <> "if(x<vx0||x>=vx1||y<vy0||y>=vy1)return;"
            <> "const sx0=Math.floor(x*scale+panX),sy0=Math.floor(y*scale+panY);"
            <> "const sx1=Math.ceil((x+1)*scale+panX),sy1=Math.ceil((y+1)*scale+panY);"
            <> "const cellW=sx1-sx0,cellH=sy1-sy0;"
            <> "if(cellW<=0||cellH<=0||sx1<=0||sy1<=0||sx0>=cw||sy0>=ch)return;"
            <> "const sp=species[gi],base=sp<<2;"
            <> "const live=alive[gi]&1;"
            <> "const r=live?pal[base]:bgR,g=live?pal[base+1]:bgG,b=live?pal[base+2]:bgB,a=live?255:bgA;"
            <> "const dx0=Math.max(0,sx0),dy0=Math.max(0,sy0);"
            <> "const dx1=Math.min(cw,sx1),dy1=Math.min(ch,sy1);"
            <> "for(let yy=dy0;yy<dy1;yy++){const row=yy*cw|0;for(let xx=dx0;xx<dx1;xx++){const o=(row+xx)<<2;p[o]=r;p[o+1]=g;p[o+2]=b;p[o+3]=a;}}"
            <> "bump(dx0,dy0,dx1,dy1);painted=true;"
            <> "};"
            <> "if(full){for(let i=0;i<p.length;i+=4){p[i]=bgR;p[i+1]=bgG;p[i+2]=bgB;p[i+3]=bgA;}"
            <> "for(let k=0,n=live.length|0;k<n;k++)paint(live[k]);"
            <> "out.dirtyCx0=0;out.dirtyCy0=0;out.dirtyCx1=cw;out.dirtyCy1=ch;out.dirtyFull=true;out.dirtyPainted=true;return;}"
            <> "for(let k=0,n=changed.length|0;k<n;k++)paint(changed[k]);"
            <> "if(!painted){out.dirtyCx0=0;out.dirtyCy0=0;out.dirtyCx1=0;out.dirtyCy1=0;out.dirtyPainted=false;out.dirtyFull=false;return;}"
            <> "out.dirtyCx0=cx0;out.dirtyCy0=cy0;out.dirtyCx1=cx1;out.dirtyCy1=cy1;out.dirtyFull=false;out.dirtyPainted=painted;"
            <> "}"
        )
    )
    ( arg pixels
        <: arg cw
        <: arg ch
        <: arg paletteRgba
        <: arg alive
        <: arg species
        <: arg w
        <: arg scale
        <: arg panX
        <: arg panY
        <: arg bg
        <: arg liveList
        <: arg changedList
        <: arg fullRedraw
        <: arg visX0
        <: arg visX1
        <: arg visY0
        <: arg visY1
        <: ArgEffect out
        <: RecNil
    )

-- | Zero a rectangular region of a row-major @Uint8Array@ (@y * gridW + x@).
-- Half-open intervals: @x0 <= x < x1@, @y0 <= y < y1@.
u8FillRegion ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f 'Unit
u8FillRegion buf gridW x0 y0 x1 y1 val =
  forRange y0 y1 $ \y ->
    forRange x0 x1 $ \x ->
      u8Set buf (y * gridW + x) val

u8Len :: Expr f 'Uint8Array -> Expr f 'Number
u8Len = expr1 FixU8Len

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

-- | @String(x)@.
toString :: Expr f u -> Expr f 'String
toString = Show

-- | @arr.method(function(x){…})@ with an 'Effect' callback.
arrayCallback ::
  String -> Expr f ('Array u) -> (Expr f u -> Effect f v) -> Effect f w
arrayCallback name arr f =
  callMethod (expr arr) name (ArgEffect (LambdaE (\x -> f (var x))) <: RecNil)

forEach :: Expr f ('Array u) -> (Expr f u -> Effect f u') -> Effect f 'Unit
forEach = arrayCallback "forEach"

forEach_ ::
  Expr f ('Array u)
  -> (Expr f u -> EffectSyntax f (f 'Unit))
  -> EffectSyntax f (f 'Unit)
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
stringCaseE ::
  Expr f 'String -> [(Text, Effect f v)] -> Effect f v -> Effect f v
stringCaseE = StringCaseE

-- | Drop a result, forcing 'Unit'. Lets statement-'if' be 'IfE' of two
-- unit arms (polymorphic 'FFI' / 'CallMethod' are not unit witnesses).
discard :: Effect f u -> Effect f 'Unit
discard e = ThenE e noOp

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

optionCase ::
  Expr f ('Option u) -> Expr f v -> (Expr f u -> Expr f v) -> Expr f v
optionCase opt noneBranch someBranch = OptionCase opt noneBranch (\x -> someBranch (var x))

optionCaseE ::
  Expr f ('Option u) -> Effect f v -> (Expr f u -> Effect f v) -> Effect f v
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
  -> (Expr f u -> Effect f a)
  -> EffectSyntax f ()
addEventListener name el handler =
  toSyntax_ $
    callMethod
      el
      "addEventListener"
      (ArgExpr (string name) <: ArgEffect (LambdaE (\x -> handler (var x))) <: RecNil)

addEventListener_ ::
  Text
  -> Effect f ('MutableObject obj)
  -> EffectSyntax f (f 'Unit)
  -> EffectSyntax f ()
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
whenS c body = toSyntax $ when_ (expr c) (stmts body)

ifS ::
  Expr f 'Bool
  -> EffectSyntax f (f 'Unit)
  -> EffectSyntax f (f 'Unit)
  -> EffectSyntax f (f 'Unit)
-- Branches are already 'Effect' 'Unit' via 'stmts'; do not 'discard' here.
-- 'discard(stmts …)' under a constant-folded 'IfE' arm can drop impure FFI
-- preludes (see 'stepGrid' region copy).
ifS c t e = toSyntax $ IfE (expr c) (stmts t) (stmts e)

whenSomeS ::
  Expr f ('Option u)
  -> (Expr f u -> EffectSyntax f (f 'Unit))
  -> EffectSyntax f (f 'Unit)
whenSomeS opt k = toSyntax $ optionCaseE opt noOp (\x -> stmts (k x))

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

(.>)
  , (.<)
  , (.>=)
  , (.<=) ::
    Comparable a => Expr f a -> Expr f a -> Expr f 'Bool
(.>) = mkGTh
(.<) = mkLTh
(.>=) = mkGTEq
(.<=) = mkLTEq

(.&&), (.||) :: Expr f 'Bool -> Expr f 'Bool -> Expr f 'Bool
(.&&) = And
(.||) = Or

ushr :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
ushr = UShr

-- | BigInt truncating division (JS @/@). Number uses 'Fractional' @/@.
quot_ :: Expr f 'BigInt -> Expr f 'BigInt -> Expr f 'BigInt
quot_ x y = Std (Kernel (KBig BQuot x y))

-- | @parseInt(s, radix)@. The radix is required (Crockford appendix A).
parseInt_ :: Expr f 'String -> Expr f 'Number -> Expr f 'Number
parseInt_ s r = expr2 FixParseInt s r

-- | @BigInt(n)@. Throws when @n@ is not an integer Number.
toBigInt :: Expr f 'Number -> Expr f 'BigInt
toBigInt = expr1 FixToBigInt

-- | @Number(n)@. Large values may lose precision.
fromBigInt :: Expr f 'BigInt -> Expr f 'Number
fromBigInt = expr1 FixFromBigInt

-- | @BigInt(s)@. Accepts an optional sign and @0x@ / @0b@ / @0o@ prefixes.
parseBigInt_ :: Expr f 'String -> Expr f 'BigInt
parseBigInt_ = expr1 FixParseBigInt
