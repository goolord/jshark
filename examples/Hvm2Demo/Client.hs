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

import qualified Data.Text as T
import GHC.Generics (Generic)
import JShark.Api
import JShark.Api.Generic (MutableObjectOf, newRecord)
import JShark.Api.Rec (Rec (..), (<:))
import qualified JShark.Array as Array
import qualified JShark.Canvas as Canvas
import qualified JShark.Dom as Dom
import qualified JShark.Math as Math
import JShark.Promise (promiseCatch, promiseThen)
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
import Page
  ( benchId
  , boardId
  , metricBackendId
  , metricCenterId
  , metricFpsId
  , metricFrameMsId
  , metricFrameNumId
  , metricKernelId
  , metricScaleId
  , modeHvm2Id
  , modeJsId
  , modeWasmId
  , pauseId
  , resSelectId
  , statusId
  )

-- | @labMode@: 0 = JS reference, 1 = WASM SIMD grid, 2 = HVM2 net reduction.
data LabState = LabState
  { labFrame :: Double
  , labMode :: Double
  , labWasmReady :: Bool
  , labWasmFailed :: Bool
  , labTickMs :: Double
  , labCenterRe :: Double
  , labCenterIm :: Double
  , labScale :: Double
  , labPrevMs :: Double
  , labFps :: Double
  , labFrameMs :: Double
  , labKernelMs :: Double
  , labWidth :: Double
  , labHeight :: Double
  , labPaused :: Bool
  }
  deriving Generic

wasmLoaderFFI :: String
wasmLoaderFFI =
  "(url,workerUrl)=>{"
    ++ "const load=globalThis.__jsharkHvm2Load;"
    ++ "if(typeof load!=='function'){"
    ++ "return Promise.reject(new Error('HVM2 loader missing (hvm2-wasm.js)'));"
    ++ "}"
    ++ "return load(url,workerUrl);"
    ++ "}"

initMandelJsFFI :: String
initMandelJsFFI =
  "()=>{globalThis.__jsharkMandelJs=" ++ mandelJsSource ++ "}"

gridBackendLabelFFI :: String
gridBackendLabelFFI =
  "(mode)=>{"
    ++ "const b=globalThis.__jsharkGridBackend||'js';"
    ++ "const m=mode|0;"
    ++ "const h=globalThis.__jsharkHvm2;"
    ++ "let s;"
    ++ "if(m===0){"
    ++ "s=b==='js'?'JS ref':'JS ref (unexpected '+b+')';"
    ++ "}else if(m===1){"
    ++ "if(b==='wasm-simd')s='WASM SIMD';"
    ++ "else if(b==='wasm-scalar')s='WASM requested · scalar fallback';"
    ++ "else if(b==='wasm-simd-fail')s='WASM requested · grid failed';"
    ++ "else s='WASM requested · JS fallback';"
    ++ "}else{"
    ++ "if(b==='hvm2')s='HVM2 net (f24)';"
    ++ "else if(b==='hvm2-fail:no-export')s='export missing';"
    ++ "else if(b==='hvm2-fail:trap')s='wasm trap (OOB)';"
    ++ "else if(b==='hvm2-fail:zero'){"
    ++ "const lk=globalThis.__jsharkHvm2LastK??0;"
    ++ "if(lk===-11)s='boot failed (book)';"
    ++ "else if(lk===-12)s='boot failed (net)';"
    ++ "else if(lk===-15)s='wasm trap (OOB)';"
    ++ "else if(lk===-2)s='def jshark_grid missing';"
    ++ "else if(lk===-13)s='normalize stuck';"
    ++ "else if(lk===-14)s='normalize budget';"
    ++ "else if(lk>0&&lk<999999)s='grid incomplete ('+lk+' cells)';"
    ++ "else s='grid returned 0';"
    ++ "}"
    ++ "else if(b==='wasm-scalar')s='wasm scalar fallback';"
    ++ "else s='JS fallback';"
    ++ "}"
    ++ "if(h?.loadNote)s+=' · '+h.loadNote;"
    ++ "if(h?.blocking&&b==='hvm2')s+=' · '+(h.threads|0)+' threads · UI blocked in normalize';"
    ++ "return s;"
    ++ "}"

gridBackendFallbackFFI :: String
gridBackendFallbackFFI =
  "(mode)=>{"
    ++ "const b=globalThis.__jsharkGridBackend||'js';"
    ++ "const m=mode|0;"
    ++ "if(m===2)return b!=='hvm2';"
    ++ "if(m===1)return b!=='wasm-simd';"
    ++ "return b!=='js';"
    ++ "}"

updateMetricsFFI :: String
updateMetricsFFI =
  "(k,f,fps,fn,sc,re,im,bk,err,warn)=>{"
    ++ "const fix=(id,v)=>{const e=document.getElementById(id);if(e)e.textContent=v;};"
    ++ "fix('"
    ++ T.unpack metricKernelId
    ++ "',Number(k).toFixed(1));"
    ++ "fix('"
    ++ T.unpack metricFrameMsId
    ++ "',Number(f).toFixed(1));"
    ++ "fix('"
    ++ T.unpack metricFpsId
    ++ "',String(Math.round(fps)).padStart(3,'\\u00a0'));"
    ++ "fix('"
    ++ T.unpack metricFrameNumId
    ++ "',String(Math.trunc(fn)).padStart(5,'\\u00a0'));"
    ++ "fix('"
    ++ T.unpack metricScaleId
    ++ "',Number(sc).toExponential(3));"
    ++ "fix('"
    ++ T.unpack metricCenterId
    ++ "',Number(re).toFixed(6)+'+'+Number(im).toFixed(6)+'i');"
    ++ "const be=document.getElementById('"
    ++ T.unpack metricBackendId
    ++ "');"
    ++ "if(be){"
    ++ "be.textContent=bk;"
    ++ "be.classList.toggle('error',!!err);"
    ++ "be.classList.toggle('warn',!!warn&&!err);"
    ++ "}"
    ++ "}"

mainJS :: forall f. EffectSyntax f (f 'Unit)
mainJS = do
  canvas <- Dom.lookupId (string boardId)
  status <- Dom.lookupId (string statusId)
  modeWasm <- Dom.lookupId (string modeWasmId)
  modeHvm2 <- Dom.lookupId (string modeHvm2Id)
  modeJs <- Dom.lookupId (string modeJsId)
  benchBtn <- Dom.lookupId (string benchId)
  resSelect <- Dom.lookupId (string resSelectId)
  pauseBtn <- Dom.lookupId (string pauseId)
  ctxOpt <- Canvas.getContext2dDesync canvas
  whenSomeE ctxOpt $ \ctx -> do
    ctxH <- hold (expr ctx)
    st <- hold (newRecord @LabState)
    set @"labFrame" st 0
    set @"labMode" st 0
    set @"labWasmReady" st false_
    set @"labWasmFailed" st false_
    set @"labTickMs" st (number (-1))
    set @"labCenterRe" st (number initialCenterRe)
    set @"labCenterIm" st (number initialCenterIm)
    set @"labScale" st (number initialScale)
    set @"labPrevMs" st (number (-1))
    set @"labFps" st 0
    set @"labFrameMs" st 0
    set @"labKernelMs" st 0
    set @"labWidth" st (number (fromIntegral canvasW))
    set @"labHeight" st (number (fromIntegral canvasH))
    set @"labPaused" st false_
    _ <- toSyntax_ $ ffi initMandelJsFFI (RecNil)
    wasmUrl <- getProp canvas "dataset.wasm"
    workerUrl <- getProp canvas "dataset.worker"
    boot ctxH status modeWasm modeHvm2 modeJs benchBtn canvas resSelect pauseBtn st
    activateMode modeWasm modeHvm2 modeJs 0
    loadP <- hold $ ffi wasmLoaderFFI (arg wasmUrl <: arg workerUrl <: RecNil)
    promiseCatch loadP $ \_ ->
      stmts $ do
        set @"labWasmReady" st false_
        set @"labWasmFailed" st true_
        done
    promiseThen loadP $ \_ ->
      stmts $ do
        set @"labMode" st 1
        set @"labWasmReady" st true_
        set @"labWasmFailed" st false_
        activateMode modeWasm modeHvm2 modeJs 1
        done
    done

boot ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f (MutableObjectOf LabState)
  -> EffectSyntax f (f 'Unit)
boot ctxH status modeWasm modeHvm2 modeJs benchBtn canvas resSelect pauseBtn st = do
  wireControls
    modeWasm
    modeHvm2
    modeJs
    benchBtn
    canvas
    resSelect
    pauseBtn
    st
    status
  Timers.foreverTick $ \now -> do
    paused <- st.labPaused
    sourceOpen <-
      bindExpr $
        ffiExpr "!!document.querySelector('.js-source[open]')" RecNil
    -- Optimizer strips whenS/ifS in this timeout callback. HVM2 must
    -- not re-enter normalize here; sampleGrid is armed from the click.
    whenS (not_ paused .&& not_ sourceOpen) $ do
      tickFrame st now
      paint ctxH st status
  done

wireControls ::
  Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f (MutableObjectOf LabState)
  -> Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
wireControls modeWasm modeHvm2 modeJs benchBtn canvas resSelect pauseBtn st status = do
  -- Direct FFI (not whenS/ifS): the optimizer strips conditional bodies
  -- inside event callbacks. Modes 1 and 2 both need the wasm module.
  let
    selectMode m =
      ffi
        ( "(st,wasm,hvm2,js)=>{"
            <> "const m="
            <> show (m :: Int)
            <> ";"
            <> "if(m>0&&!st.labWasmReady)return;"
            <> "st.labMode=m;"
            <> "if(m===2)globalThis.__jsharkHvm2Armed=1;"
            <> "[wasm,hvm2,js].forEach(b=>b.classList.remove('active'));"
            <> "if(m===0)js.classList.add('active');"
            <> "else if(m===1)wasm.classList.add('active');"
            <> "else hvm2.classList.add('active');"
            <> "}"
        )
        ( ArgEffect st
            <: ArgEffect modeWasm
            <: ArgEffect modeHvm2
            <: ArgEffect modeJs
            <: RecNil
        )
  addEventListener "click" modeWasm $ \_ ->
    stmts $ do
      toSyntax_ $ selectMode 1
      done
  addEventListener "click" modeHvm2 $ \_ ->
    stmts $ do
      toSyntax_ $ selectMode 2
      done
  addEventListener "click" modeJs $ \_ ->
    stmts $ do
      toSyntax_ $ selectMode 0
      done
  addEventListener "click" benchBtn $ \_ ->
    stmts $ do
      runBench st status
      done
  addEventListener "change" resSelect $ \_ ->
    stmts $ do
      v <- Dom.getValue resSelect
      toSyntax_ $
        ffi
          ( "(st,canvas,v)=>{"
              <> "const p=v.split('x').map(Number);"
              <> "const w=p[0],h=p[1];"
              <> "if(!w||!h)return;"
              <> "canvas.width=w;"
              <> "canvas.height=h;"
              <> "st.labWidth=w;"
              <> "st.labHeight=h;"
              <> "st.labFrame=0;"
              <> "}"
          )
          ( ArgEffect st
              <: ArgEffect canvas
              <: arg v
              <: RecNil
          )
      done
  addEventListener "click" pauseBtn $ \_ ->
    stmts $ do
      toSyntax_ $
        ffi
          ( "(st,btn)=>{"
              <> "st.labPaused=!st.labPaused;"
              <> "if(st.labPaused){"
              <> "btn.textContent='resume';"
              <> "btn.classList.add('active');"
              <> "}else{"
              <> "btn.textContent='pause';"
              <> "btn.classList.remove('active');"
              <> "st.labPrevMs=performance.now();"
              <> "}"
              <> "}"
          )
          (ArgEffect st <: ArgEffect pauseBtn <: RecNil)
      done
  done

activateMode ::
  Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Int
  -> EffectSyntax f (f 'Unit)
activateMode modeWasm modeHvm2 modeJs which = do
  toSyntax_ $
    ffi
      ( "(wasm,hvm2,js,which)=>{"
          <> "[wasm,hvm2,js].forEach(b=>b.classList.remove('active'));"
          <> "if(which===0)js.classList.add('active');"
          <> "else if(which===1)wasm.classList.add('active');"
          <> "else hvm2.classList.add('active');"
          <> "}"
      )
      ( ArgEffect modeWasm
          <: ArgEffect modeHvm2
          <: ArgEffect modeJs
          <: arg (number (fromIntegral which))
          <: RecNil
      )
  done

tickFrame ::
  Effect f (MutableObjectOf LabState)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
tickFrame st now = do
  prev <- st.labPrevMs
  let
    delta = Math.max (now - prev) (number 0)
    dt = Math.min delta (number 100)
  zoomFactor <- bindExpr $ zoomScaleFactor dt
  -- FFI (not whenS): hold the camera in HVM2 so a stripped tick
  -- callback cannot zoom while a cached snapshot is on screen.
  toSyntax_ $
    ffi
      ( "(st,now,zf,minS,initS)=>{"
          <> "if((st.labMode|0)===2)return;"
          <> "const shrunk=st.labScale*zf;"
          <> "st.labPrevMs=now;"
          <> "st.labFrame=(st.labFrame||0)+1;"
          <> "st.labScale=shrunk<minS?initS:shrunk;"
          <> "st.labTickMs=now;"
          <> "}"
      )
      ( ArgEffect st
          <: arg now
          <: arg zoomFactor
          <: arg (number minScale)
          <: arg (number initialScale)
          <: RecNil
      )
  done

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
  frameT0 <- bindExpr $ ffi "performance.now" RecNil
  mode <- st.labMode
  centerRe <- st.labCenterRe
  centerIm <- st.labCenterIm
  scale <- st.labScale
  w <- st.labWidth
  h <- st.labHeight
  let
    blk = number (fromIntegral blockPx)
    blocksX = Math.floor (w / blk)
    blocksY = Math.floor (h / blk)
  set @"fillStyle" ctx (string "#07080f")
  Canvas.fillRect ctx 0 0 w h
  kernelT0 <- bindExpr $ ffi "performance.now" RecNil
  grid <-
    bindExpr $
      sampleGrid mode centerRe centerIm scale w h blk blocksX blocksY
  kernelT1 <- bindExpr $ ffi "performance.now" RecNil
  blitGrid ctx grid blocksX blocksY blk w h
  frameT1 <- bindExpr $ ffi "performance.now" RecNil
  let
    kernelMs = kernelT1 - kernelT0
    frameMs = frameT1 - frameT0
  set @"labKernelMs" st kernelMs
  set @"labFrameMs" st frameMs
  updateFrameFps st frameMs
  fpsLine status st mode centerRe centerIm scale
  done

updateFrameFps ::
  Effect f (MutableObjectOf LabState)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
updateFrameFps st frameMs = do
  prevFps <- st.labFps
  let
    instant =
      if_ (frameMs .> number 0) (number 1000 / frameMs) (number 0)
    smoothed = prevFps * number 0.85 + instant * number 0.15
  set @"labFps" st smoothed
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

-- | @mode@: 0 = JS loop, 1 = WASM SIMD grid, 2 = HVM2 net reduction of the
-- Bend-compiled @jshark_grid@ def. SIMD-grid miss falls back to the JS
-- loop; per-pixel wasm calls lose to a warmed JS JIT on this kernel.
sampleGrid ::
  Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f ('Array 'Number)
sampleGrid mode centerRe centerIm scale w h blk blocksX blocksY =
  ffi
    ( "(mode,centerRe,centerIm,scale,w,h,blk,bxN,byN)=>{"
        <> "const setBackend=b=>{globalThis.__jsharkGridBackend=b;};"
        <> "const ex=globalThis.__jsharkHvm2?.exports;"
        <> "const mem=ex?.memory;"
        <> "let hvm2Fail=null;"
        <> "if(mode===2&&mem){"
        <> "const cache=globalThis.__jsharkHvm2Cache;"
        <> "const n=bxN*byN;"
        <> "if(!globalThis.__jsharkHvm2Armed&&cache&&cache.length===n){"
        <> "setBackend('hvm2');return cache;"
        <> "}"
        <> "const hg=ex?.mandel_hvm2_grid;"
        <> "if(typeof hg!=='function'){hvm2Fail='no-export';}"
        <> "else{"
        <> "try{"
        <> "globalThis.__jsharkHvm2Armed=0;"
        <> "const ptr=hg(centerRe,centerIm,scale,w,h,blk,bxN,byN);"
        <> "if(typeof ex?.jshark_hvm2_last_k==='function'){"
        <> "globalThis.__jsharkHvm2LastK=ex.jshark_hvm2_last_k();"
        <> "}"
        <> "if(ptr){"
        <> "const g=Int32Array.from(new Int32Array(mem.buffer,ptr,n));"
        <> "globalThis.__jsharkHvm2Cache=g;"
        <> "setBackend('hvm2');return g;"
        <> "}"
        <> "hvm2Fail='zero';"
        <> "}catch(_){"
        <> "globalThis.__jsharkHvm2LastK=-15;"
        <> "hvm2Fail='trap';"
        <> "}"
        <> "}"
        <> "}"
        <> "const gridFn=ex?.mandel_grid;"
        <> "if((mode===1)&&typeof gridFn==='function'&&mem){"
        <> "const ptr=gridFn(centerRe,centerIm,scale,w,h,blk,bxN,byN);"
        <> "if(ptr){"
        <> "setBackend('wasm-simd');"
        <> "return new Int32Array(mem.buffer,ptr,bxN*byN);"
        <> "}"
        <> "if(mode===1)setBackend('wasm-simd-fail');"
        <> "}"
        <> "const out=new Array(bxN*byN);"
        <> "const half=blk/2;"
        <> "const js=globalThis.__jsharkMandelJs;"
        <> "if(hvm2Fail){"
        <> "setBackend(hvm2Fail==='no-export'?'hvm2-fail:no-export':"
        <> "hvm2Fail==='trap'?'hvm2-fail:trap':'hvm2-fail:zero');"
        <> "}else{setBackend('js');}"
        <> "let k=0;"
        <> "for(let by=0;by<byN;by++){"
        <> "for(let bx=0;bx<bxN;bx++){"
        <> "const px=bx*blk+half,py=by*blk+half;"
        <> "const invW=1/w,invH=1/h,halfW=w*0.5,halfH=h*0.5;"
        <> "const cr=centerRe+(px-halfW)*scale*invW;"
        <> "const ci=centerIm+(py-halfH)*scale*invH;"
        <> "out[k++]=js(cr,ci);"
        <> "}"
        <> "}"
        <> "return out;"
        <> "}"
    )
    ( arg mode
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
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
fpsLine status st modeN centerRe centerIm scale = do
  frameN <- st.labFrame
  fps <- st.labFps
  frameMs <- st.labFrameMs
  kernelMs <- st.labKernelMs
  failed <- get @"labWasmFailed" st
  backendLabel <-
    bindExpr $ ffi gridBackendLabelFFI (arg modeN <: RecNil)
  fallback <-
    bindExpr $ ffi gridBackendFallbackFFI (arg modeN <: RecNil)
  let
    hvm2Err = modeN .== number 2 .&& fallback
    wasmWarn = modeN .== number 1 .&& fallback
    isErr = failed .|| hvm2Err
  toSyntax_ $
    ffi
      updateMetricsFFI
      ( arg kernelMs
          <: arg frameMs
          <: arg fps
          <: arg frameN
          <: arg scale
          <: arg centerRe
          <: arg centerIm
          <: arg backendLabel
          <: arg isErr
          <: arg (wasmWarn .&& not_ isErr)
          <: RecNil
      )
  setStatusDisplay
    status
    false_
    false_
    ( if_
        failed
        (string "WASM unavailable — JS reference")
        (string "")
    )

setStatusDisplay ::
  Effect f ('MutableObject Dom.DomElement)
  -> Expr f 'Bool
  -> Expr f 'Bool
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
setStatusDisplay el isErr isWarn msg = do
  toSyntax_ $
    ffi
      ( "(el,err,warn,msg)=>{"
          <> "el.textContent=msg;"
          <> "el.classList.toggle('error',!!err);"
          <> "el.classList.toggle('warn',!!warn&&!err);"
          <> "}"
      )
      (ArgEffect el <: arg isErr <: arg isWarn <: arg msg <: RecNil)
  done

-- | Three-lane bench on the identical frame workload: JS reference loop,
-- WASM SIMD grid (averaged over 'benchReps'), and one full HVM2 net
-- reduction of the Bend book. All reported as per-frame milliseconds.
runBench ::
  Effect f (MutableObjectOf LabState)
  -> Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
runBench st status = do
  centerRe <- st.labCenterRe
  centerIm <- st.labCenterIm
  scale <- st.labScale
  w <- st.labWidth
  h <- st.labHeight
  wasPaused <- st.labPaused
  set @"labPaused" st true_
  let
    blk = number (fromIntegral blockPx)
    blocksX = Math.floor (w / blk)
    blocksY = Math.floor (h / blk)
  res <- bindExpr $ benchCompare centerRe centerIm scale w h
  set @"labPaused" st wasPaused
  let
    jsMs = Array.index res 0
    wasmMs = Array.index res 1
    speedup = Array.index res 2
    hvm2Ms = Array.index res 3
    wasmOk = wasmMs .>= number 0
    hvm2Ok = hvm2Ms .>= number 0
    gridTxt =
      string "per "
        <> toString blocksX
        <> string "×"
        <> toString blocksY
        <> string " frame: "
    wasmTxt =
      if_
        wasmOk
        ( string " · WASM SIMD "
            <> toString wasmMs
            <> string " ms ("
            <> toString speedup
            <> string "× vs JS)"
        )
        (string " · WASM unavailable")
    hvm2Txt =
      if_
        hvm2Ok
        ( string " · HVM2 net "
            <> toString hvm2Ms
            <> string " ms"
        )
        (string " · HVM2 unavailable")
  setStatusDisplay
    status
    (not_ wasmOk)
    false_
    ( string "bench "
        <> gridTxt
        <> string "JS "
        <> toString jsMs
        <> string " ms"
        <> wasmTxt
        <> hvm2Txt
    )
  done

benchReps :: Int
benchReps = 20

benchCompare ::
  Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f ('Array 'Number)
benchCompare cr ci scale w h =
  ffi
    ( "(cr,ci,scale,w,h)=>{"
        <> "const blk="
        <> show blockPx
        <> ",reps="
        <> show benchReps
        <> ";"
        <> "const bxN=Math.floor(w/blk),byN=Math.floor(h/blk);"
        <> "const js=globalThis.__jsharkMandelJs;"
        <> "const ex=globalThis.__jsharkHvm2?.exports;"
        <> "const grid=ex?.mandel_grid;"
        <> "const mem=ex?.memory;"
        <> "const half=blk/2,invW=1/w,invH=1/h,halfW=w*0.5,halfH=h*0.5;"
        <> "const r1=x=>Math.round(x*10)/10;"
        <> "let sink=0;"
        <> "const t0=performance.now();"
        <> "for(let r=0;r<reps;r++){"
        <> "for(let by=0;by<byN;by++){for(let bx=0;bx<bxN;bx++){"
        <> "const cr2=cr+(bx*blk+half-halfW)*scale*invW;"
        <> "const ci2=ci+(by*blk+half-halfH)*scale*invH;"
        <> "sink+=js(cr2,ci2);"
        <> "}}}"
        <> "const jsMs=performance.now()-t0;"
        <> "let wasmMs=-1;"
        <> "if(typeof grid==='function'&&mem){"
        <> "const t1=performance.now();"
        <> "let ptr=0;"
        <> "for(let r=0;r<reps;r++)ptr=grid(cr,ci,scale,w,h,blk,bxN,byN);"
        <> "wasmMs=performance.now()-t1;"
        <> "if(ptr)sink+=new Int32Array(mem.buffer,ptr,1)[0];"
        <> "}"
        <> "let hvmMs=-1;"
        <> "const hg=ex?.mandel_hvm2_grid;"
        <> "if(typeof hg==='function'&&mem){"
        <> "const t2=performance.now();"
        <> "try{"
        <> "const p=hg(cr,ci,scale,w,h,blk,bxN,byN);"
        <> "if(p){hvmMs=performance.now()-t2;"
        <> "sink+=new Int32Array(mem.buffer,p,1)[0];}"
        <> "}catch(_){}"
        <> "}"
        <> "globalThis.__jsharkBenchSink=sink;"
        <> "const sp=wasmMs>0?r1(jsMs/wasmMs):0;"
        <> "return[r1(jsMs/reps),wasmMs<0?-1:r1(wasmMs/reps),sp,"
        <> "hvmMs<0?-1:r1(hvmMs)];"
        <> "}"
    )
    ( arg cr
        <: arg ci
        <: arg scale
        <: arg w
        <: arg h
        <: RecNil
    )
