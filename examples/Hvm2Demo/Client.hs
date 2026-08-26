{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Mandelbrot zoom canvas driven by pure 'Kernels' and HVM2 WASM.
module Client (mainJS) where

import GHC.Generics (Generic)
import JShark.Api
import qualified JShark.Canvas as Canvas
import qualified JShark.Dom as Dom
import JShark.Generic (MutableObjectOf, newRecord)
import qualified JShark.Math as Math
import JShark.Promise (Promise, promiseCatch, promiseThen)
import JShark.Rec (Rec (..), (<:))
import qualified JShark.Timers as Timers
import Kernels
  ( blockPx
  , canvasH
  , canvasW
  , initialCenterIm
  , initialCenterRe
  , initialScale
  , mandelJsSource
  , maxIter
  , minScale
  , zoomRate
  , zoomReferenceMs
  )
import Page (benchId, boardId, modeJsId, modeWasmId, statusId)

data LabState = LabState
  { labFrame :: Double
  , labWasm :: Bool
  , labTickMs :: Double
  , labCenterRe :: Double
  , labCenterIm :: Double
  , labScale :: Double
  , labPrevMs :: Double
  , labFps :: Double
  }
  deriving Generic

wasmLoaderFFI :: String
wasmLoaderFFI =
  "url=>(async()=>{"
    ++ "if(!globalThis.WebAssembly)throw new Error(\"WebAssembly unavailable\");"
    ++ "const r=await fetch(url);"
    ++ "if(!r.ok)throw new Error(\"HVM2 wasm fetch failed: \"+url);"
    ++ "const b=await r.arrayBuffer();"
    ++ "const{instance:i}=await WebAssembly.instantiate(b,{});"
    ++ "globalThis.__jsharkHvm2={exports:i.exports}"
    ++ "})()"

initMandelJsFFI :: String
initMandelJsFFI =
  "()=>{globalThis.__jsharkMandelJs=" ++ mandelJsSource ++ "}"

mainJS :: forall f. EffectSyntax f (f 'Unit)
mainJS = do
  canvas <- Dom.lookupId (string boardId)
  status <- Dom.lookupId (string statusId)
  modeWasm <- Dom.lookupId (string modeWasmId)
  modeJs <- Dom.lookupId (string modeJsId)
  benchBtn <- Dom.lookupId (string benchId)
  ctxOpt <- Canvas.getContext2d canvas
  whenSomeE ctxOpt $ \ctx -> do
    ctxH <- hold (expr ctx)
    st <- hold (newRecord @LabState)
    set @"labFrame" st 0
    set @"labWasm" st false_
    set @"labTickMs" st (number (-1))
    set @"labCenterRe" st (number initialCenterRe)
    set @"labCenterIm" st (number initialCenterIm)
    set @"labScale" st (number initialScale)
    set @"labPrevMs" st (number (-1))
    set @"labFps" st 0
    _ <- toSyntax_ $ ffi initMandelJsFFI (RecNil)
    wasmUrl <- getProp canvas "dataset.wasm"
    let
      loadP =
        ffi wasmLoaderFFI (arg wasmUrl <: RecNil) ::
          Effect f ('MutableObject (Promise 'Unit))
    promiseCatch loadP $ \_ ->
      stmts $ do
        set @"labWasm" st false_
        setStatus status (string "WASM unavailable, JS reference only")
        boot ctxH status modeWasm modeJs benchBtn st
    promiseThen loadP $ \_ ->
      stmts $ do
        set @"labWasm" st true_
        setStatus status (string "WASM ready")
        boot ctxH status modeWasm modeJs benchBtn st
    done

boot ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f (MutableObjectOf LabState)
  -> EffectSyntax f (f 'Unit)
boot ctxH status modeWasm modeJs benchBtn st = do
  wireControls modeWasm modeJs benchBtn st status
  Timers.foreverTick $ \now -> do
    tickFrame st now
    paint ctxH st status
  done

wireControls ::
  Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f (MutableObjectOf LabState)
  -> Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
wireControls modeWasm modeJs benchBtn st status = do
  addEventListener "click" modeWasm $ \_ ->
    stmts $ do
      set @"labWasm" st true_
      setActive modeWasm modeJs
      done
  addEventListener "click" modeJs $ \_ ->
    stmts $ do
      set @"labWasm" st false_
      setActive modeJs modeWasm
      done
  addEventListener "click" benchBtn $ \_ ->
    stmts $ do
      runBench st status
      done
  done

setActive ::
  Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
setActive on off = do
  toSyntax_ $
    ffi
      "(on,off)=>{on.classList.add('active');off.classList.remove('active')}"
      (ArgEffect on <: ArgEffect off <: RecNil)
  done

tickFrame ::
  Effect f (MutableObjectOf LabState)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
tickFrame st now = do
  prev <- st.labPrevMs
  prevFps <- st.labFps
  f <- st.labFrame
  scale <- st.labScale
  let
    delta = Math.max (now - prev) (number 0)
    instant =
      if_ (prev .>= number 0 .&& delta .> number 0) (number 1000 / delta) (number 0)
    smoothed =
      if_ (prev .>= number 0) (prevFps * number 0.85 + instant * number 0.15) instant
    dt = Math.min delta (number 100)
  zoomFactor <- bindExpr $ zoomScaleFactor dt
  let
    shrunk = scale * zoomFactor
    nextScale =
      if_ (shrunk .< number minScale) (number initialScale) shrunk
  set @"labFps" st smoothed
  set @"labPrevMs" st now
  set @"labFrame" st (f + 1)
  set @"labScale" st nextScale
  set @"labTickMs" st now

-- | @zoomRate@ per @zoomReferenceMs@, raised to elapsed ms (fps-independent).
zoomScaleFactor :: Expr f 'Number -> Effect f 'Number
zoomScaleFactor deltaMs =
  ffi
    "(rate,dt,ref)=>Math.pow(rate,dt/ref)"
    ( arg (number zoomRate)
        <: arg deltaMs
        <: arg (number zoomReferenceMs)
        <: RecNil
    )

paint ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Effect f (MutableObjectOf LabState)
  -> Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
paint ctx st status = do
  wasmOn <- st.labWasm
  centerRe <- st.labCenterRe
  centerIm <- st.labCenterIm
  scale <- st.labScale
  let
    w = number (fromIntegral canvasW)
    h = number (fromIntegral canvasH)
    blk = number (fromIntegral blockPx)
    blocksX = Math.floor (w / blk)
    blocksY = Math.floor (h / blk)
  set @"fillStyle" ctx (string "#07080f")
  Canvas.fillRect ctx 0 0 w h
  grid <-
    bindExpr $
      sampleGrid wasmOn centerRe centerIm scale w h blk blocksX blocksY
  blitGrid ctx grid blocksX blocksY blk w h
  fpsLine status st wasmOn centerRe centerIm scale
  done

blitGrid ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Expr f ('Array 'Number)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
blitGrid ctx grid blocksX blocksY blk w h = do
  toSyntax_ $
    ffi
      ( "(ctx,grid,bxN,byN,blk,w,h)=>{"
          <> "const max="
          <> show maxIter
          <> ";"
          <> "const img=ctx.createImageData(w,h);"
          <> "const d=img.data;"
          <> "const bg=0x090912;"
          <> "const rgb=i=>{"
          <> "if(i>=max)return bg;"
          <> "const t=i/(max-1);"
          <> "const hue=(285-t*250)|0;"
          <> "const s=1,l=0.62+t*0.28;"
          <> "const c=(1-Math.abs(2*l-1)*s)*255;"
          <> "const x=c*(1-Math.abs((hue/60)%2-1));"
          <> "let r=0,g=0,b=0;"
          <> "if(hue<60){r=c;g=x;}else if(hue<120){r=x;g=c;}else if(hue<180){g=c;b=x;}"
          <> "else if(hue<240){g=x;b=c;}else if(hue<300){r=x;b=c;}else{r=c;b=x;}"
          <> "const m=(l-0.5*s)*255;"
          <> "return((r+m)<<16)|((g+m)<<8)|((b+m)|0);"
          <> "};"
          <> "for(let by=0;by<byN;by++){"
          <> "for(let bx=0;bx<bxN;bx++){"
          <> "const px0=bx*blk,py0=by*blk;"
          <> "const c=rgb(grid[by*bxN+bx]);"
          <> "for(let dy=0;dy<blk;dy++){"
          <> "for(let dx=0;dx<blk;dx++){"
          <> "const x=px0+dx,y=py0+dy,p=(y*w+x)*4;"
          <> "d[p]=c&255;d[p+1]=(c>>8)&255;d[p+2]=(c>>16)&255;d[p+3]=255;"
          <> "}}}"
          <> "}"
          <> "ctx.putImageData(img,0,0);"
          <> "}"
      )
      ( ArgEffect ctx
          <: arg grid
          <: arg blocksX
          <: arg blocksY
          <: arg blk
          <: arg w
          <: arg h
          <: RecNil
      )
  done

sampleGrid ::
  Expr f 'Bool
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f ('Array 'Number)
sampleGrid wasmOn centerRe centerIm scale w h blk blocksX blocksY =
  ffi
    ( "(wasmOn,centerRe,centerIm,scale,w,h,blk,bxN,byN)=>{"
        <> "const out=new Array(bxN*byN);"
        <> "const half=blk/2;"
        <> "const buf=new ArrayBuffer(8);"
        <> "const f64=new Float64Array(buf);"
        <> "const i64=new BigInt64Array(buf);"
        <> "const pack=x=>{f64[0]=+x;return i64[0];};"
        <> "const wasm64=globalThis.__jsharkHvm2?.exports?.mandel_f64;"
        <> "const wasm=globalThis.__jsharkHvm2?.exports?.mandel;"
        <> "const js=globalThis.__jsharkMandelJs;"
        <> "const useWasm64=wasmOn&&typeof wasm64==='function';"
        <> "const useWasmI64=wasmOn&&!useWasm64&&typeof wasm==='function';"
        <> "let k=0;"
        <> "for(let by=0;by<byN;by++){"
        <> "for(let bx=0;bx<bxN;bx++){"
        <> "const px=bx*blk+half,py=by*blk+half;"
        <> "const invW=1/w,invH=1/h,halfW=w*0.5,halfH=h*0.5;"
        <> "const cr=centerRe+(px-halfW)*scale*invW;"
        <> "const ci=centerIm+(py-halfH)*scale*invH;"
        <> "if(useWasm64){"
        <> "out[k++]=wasm64(cr,ci);"
        <> "}else if(useWasmI64){"
        <> "const r=wasm(pack(cr),pack(ci));"
        <> "out[k++]=typeof r==='bigint'?Number(r):r;"
        <> "}else{"
        <> "out[k++]=js(cr,ci);"
        <> "}"
        <> "}"
        <> "}"
        <> "return out;"
        <> "}"
    )
    ( arg wasmOn
        <: arg centerRe
        <: arg centerIm
        <: arg scale
        <: arg w
        <: arg h
        <: arg blk
        <: arg blocksX
        <: arg blocksY
        <: RecNil
    )

fpsLine ::
  Effect f ('MutableObject Dom.DomElement)
  -> Effect f (MutableObjectOf LabState)
  -> Expr f 'Bool
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
fpsLine status st wasmOn centerRe centerIm scale = do
  frameN <- st.labFrame
  fps <- st.labFps
  let
    mode =
      if_ wasmOn (string "WASM native") (string "JS ref")
    fpsTxt = toString (Math.round fps)
  setStatus
    status
    ( fpsTxt
        <> string " fps · "
        <> mode
        <> string " · frame "
        <> toString frameN
        <> string " · scale "
        <> toString scale
        <> string " · c="
        <> toString centerRe
        <> string "+"
        <> toString centerIm
        <> string "i"
    )

setStatus ::
  Effect f ('MutableObject Dom.DomElement)
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
setStatus el msg = do
  _ <- Dom.setTextContent el msg
  done

runBench ::
  Effect f (MutableObjectOf LabState)
  -> Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
runBench st status = do
  wasmOn <- st.labWasm
  centerRe <- st.labCenterRe
  centerIm <- st.labCenterIm
  ms <-
    bindExpr $
      benchMandel wasmOn centerRe centerIm (number (-0.5)) (number 0)
  setStatus
    status
    ( string "bench 50k× mandel → "
        <> toString ms
        <> string " ms ("
        <> if_ wasmOn (string "WASM native") (string "JS ref")
        <> string ")"
    )

benchMandel ::
  Expr f 'Bool
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f 'Number
benchMandel wasmOn cr ci cr2 ci2 =
  ffi
    ( "(wasmOn,cr,ci,cr2,ci2)=>{"
        <> "const t0=performance.now();"
        <> "const buf=new ArrayBuffer(8);"
        <> "const f64=new Float64Array(buf);"
        <> "const i64=new BigInt64Array(buf);"
        <> "const pack=x=>{f64[0]=+x;return i64[0];};"
        <> "const step=globalThis.__jsharkMandelJs;"
        <> "const wasm64=globalThis.__jsharkHvm2?.exports?.mandel_f64;"
        <> "const wasm=globalThis.__jsharkHvm2?.exports?.mandel;"
        <> "if(wasmOn&&typeof wasm64==='function'){"
        <> "for(let i=0;i<50000;i++)wasm64(cr+cr2*0.001*i,ci+ci2*0.001*i);"
        <> "}else if(wasmOn){"
        <> "const f=wasm;"
        <> "if(typeof f!=='function')return -1;"
        <> "for(let i=0;i<50000;i++)f(pack(cr+cr2*0.001*i),pack(ci+ci2*0.001*i));"
        <> "}else{"
        <> "if(typeof step!=='function')return -1;"
        <> "for(let i=0;i<50000;i++)step(cr+cr2*0.001*i,ci+ci2*0.001*i);"
        <> "}"
        <> "return performance.now()-t0;"
        <> "}"
    )
    ( arg wasmOn
        <: arg cr
        <: arg ci
        <: arg cr2
        <: arg ci2
        <: RecNil
    )
