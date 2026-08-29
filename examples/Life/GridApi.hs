{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Life grid/canvas byte-buffer helpers (soup seed, region copy/fill, RGBA paint).
-- Kept out of public 'JShark.Api' — only the Life example uses these FFI bundles.
module GridApi
  ( seedLiveCells
  , seedSoupRegion
  , fillRgbaImageData
  , rgbaPixelSet
  , rgbaFillRect
  , paintGridCells
  , paintGridCellsJs
  , u8CopyRegion
  , u8FillRegion
  , forRange2
  , forRange2_
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import JShark.Api
  ( arg
  , forRange
  , stmts
  , toSyntax
  , u8Set
  )
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types
  ( Arg (..)
  , Effect (..)
  , EffectSyntax
  , Expr (..)
  , FFIForm (FFILambda)
  , Universe (..)
  , Value (..)
  )

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

-- | Embedded JS for 'paintGridCells' (exported for regression tests).
paintGridCellsJs :: Text
paintGridCellsJs =
  "(p,cw,ch,pal,alive,species,w,scale,panX,panY,bg,live,changed,full,vx0,vx1,vy0,vy1,out)=>{"
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
    <> "if(full){"
    <> "const gx0=Math.max(0,vx0|0),gx1=Math.min(w,vx1|0);"
    <> "const gh=(p.length>>2)/w|0;"
    <> "const gy0=Math.max(0,vy0|0),gy1=Math.min(gh,vy1|0);"
    <> "for(let y=gy0;y<gy1;y++){const row=y*w|0;for(let x=gx0;x<gx1;x++){"
    <> "const o=(row+x)<<2;p[o]=bgR;p[o+1]=bgG;p[o+2]=bgB;p[o+3]=bgA;}}"
    <> "for(let k=0,n=live.length|0;k<n;k++)paint(live[k]);"
    <> "out.dirtyCx0=0;out.dirtyCy0=0;out.dirtyCx1=cw;out.dirtyCy1=ch;out.dirtyFull=true;out.dirtyPainted=true;return;}"
    <> "for(let k=0,n=changed.length|0;k<n;k++)paint(changed[k]);"
    <> "if(!painted){out.dirtyCx0=0;out.dirtyCy0=0;out.dirtyCx1=0;out.dirtyCy1=0;out.dirtyPainted=false;out.dirtyFull=false;return;}"
    <> "out.dirtyCx0=cx0;out.dirtyCy0=cy0;out.dirtyCx1=cx1;out.dirtyCy1=cy1;out.dirtyFull=false;out.dirtyPainted=painted;"
    <> "}"

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
      (FFILambda paintGridCellsJs)
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

-- | Nested half-open @\[y0,y1) x \[x0,x1)@ loop.
forRange2 ::
  Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> (Expr f 'Number -> Expr f 'Number -> Effect f 'Unit)
  -> Effect f 'Unit
forRange2 y0 y1 x0 x1 body =
  forRange y0 y1 $ \y ->
    forRange x0 x1 $ \x ->
      body y x

forRange2_ ::
  Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> (Expr f 'Number -> Expr f 'Number -> EffectSyntax f (f 'Unit))
  -> EffectSyntax f (f 'Unit)
forRange2_ y0 y1 x0 x1 f = toSyntax $ forRange2 y0 y1 x0 x1 (\y x -> stmts (f y x))
