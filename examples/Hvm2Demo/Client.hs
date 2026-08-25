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
  , mandelEscapes
  , mandelKernel
  , maxIter
  , zoomRate
  )
import Page (benchId, boardId, modeJsId, modeWasmId, statusId)

data LabState = LabState
  { labFrame :: Double
  , labWasm :: Bool
  , labReady :: Bool
  , labTickMs :: Double
  , labCenterRe :: Double
  , labCenterIm :: Double
  , labScale :: Double
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
    set @"labReady" st false_
    set @"labTickMs" st (number (-1))
    set @"labCenterRe" st (number initialCenterRe)
    set @"labCenterIm" st (number initialCenterIm)
    set @"labScale" st (number initialScale)
    wasmUrl <- getProp canvas "dataset.wasm"
    let
      loadP =
        ffi wasmLoaderFFI (arg wasmUrl <: RecNil)
          :: Effect f ('MutableObject (Promise 'Unit))
    promiseCatch loadP $ \_ ->
      stmts $ do
        set @"labWasm" st false_
        setStatus status (string "WASM unavailable, JS reference only")
        boot ctxH status modeWasm modeJs benchBtn st
    promiseThen loadP $ \_ ->
      stmts $ do
        set @"labReady" st true_
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
  Timers.foreverFrame $ \now -> do
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
  Effect f (MutableObjectOf LabState) -> Expr f 'Number -> EffectSyntax f (f 'Unit)
tickFrame st now = do
  f <- st.labFrame
  scale <- st.labScale
  set @"labFrame" st (f + 1)
  set @"labScale" st (scale * number zoomRate)
  set @"labTickMs" st now

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
    half = blk / number 2
    blocksX = Math.floor (w / blk)
    blocksY = Math.floor (h / blk)
  set @"fillStyle" ctx (string "#07080f")
  Canvas.fillRect ctx 0 0 w h
  forRange_ 0 blocksY $ \by ->
    forRange_ 0 blocksX $ \bx -> do
      let
        px = bx * blk + half
        py = by * blk + half
        iters =
          mandelAt px py w h centerRe centerIm scale wasmOn
      css <- iterColor iters
      set @"fillStyle" ctx css
      Canvas.fillRect ctx (px - half) (py - half) blk blk
  fpsLine status st wasmOn centerRe centerIm scale
  done

mandelAt ::
  Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Bool
  -> Expr f 'Number
mandelAt px py w h centerRe centerIm scale wasmOn =
  let
    cr = centerRe + (px - w / number 2) * scale / w
    ci = centerIm + (py - h / number 2) * scale / h
    wasmIters =
      apply
        ( lambda
            ( \_ ->
                apply (apply (hvm2Kernel "mandel" mandelKernel) cr) ci
            )
        )
        (number 0)
  in
    if_ wasmOn wasmIters (mandelEscapes cr ci)

iterColor :: Expr f 'Number -> EffectSyntax f (Expr f 'String)
iterColor iters =
  bindExpr $
    ffi
      ( "(i=>{const t=i/"
          <> show maxIter
          <> ";const h=(240-t*220)|0;const s=100;const l=(18+t*62)|0;"
          <> "return 'hsl('+h+','+s+'%,'+l+'%)';})"
      )
      (arg iters <: RecNil)

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
  frameMs <- st.labTickMs
  let
    mode = if_ wasmOn (string "WASM") (string "JS")
  setStatus
    status
    ( mode
        <> string " · mandelbrot · frame "
        <> toString frameN
        <> string " · scale "
        <> toString scale
        <> string " · c="
        <> toString centerRe
        <> string "+"
        <> toString centerIm
        <> string "i · t="
        <> toString frameMs
        <> string "ms"
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
    ( string "bench 2000× mandel → "
        <> toString ms
        <> string " ms ("
        <> if_ wasmOn (string "WASM") (string "JS")
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
        <> "if(wasmOn){"
        <> "const f=globalThis.__jsharkHvm2?.exports?.mandel;"
        <> "if(typeof f!=='function')return -1;"
        <> "for(let i=0;i<2000;i++)f(pack(cr+cr2*0.001*i),pack(ci+ci2*0.001*i));"
        <> "}else{"
        <> "const max=64;"
        <> "const step=(c,d)=>{"
        <> "let n=0,zr=0,zi=0;"
        <> "while(n<max&&zr*zr+zi*zi<4){"
        <> "const nzr=zr*zr-zi*zi+c,nzi=2*zr*zi+d;"
        <> "zr=nzr;zi=nzi;n++;"
        <> "}"
        <> "return n;"
        <> "};"
        <> "for(let i=0;i<2000;i++)step(cr+cr2*0.001*i,ci+ci2*0.001*i);"
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
