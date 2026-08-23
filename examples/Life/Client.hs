{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Client (mainJS) where

import Discover (initRegistry)
import Engine
import GHC.Generics (Generic)
import Grid (createImageData)
import JShark.Api
import qualified JShark.Canvas as Canvas
import qualified JShark.Dom as Dom
import JShark.Generic (MutableObjectOf)
import qualified JShark.Generic as G
import qualified JShark.Math as Math
import JShark.Rec (Rec (..), (<:))
import qualified JShark.Timers as Timers
import Types (LifeState, boardId, canvasH, canvasW, cellPx, ink)

data Fps = Fps
  { lastMs :: Double
  , fps :: Double
  }
  deriving Generic

mainJS :: forall f. EffectSyntax f (f 'Unit)
mainJS = do
  canvas <- Dom.lookupId (string boardId)
  ctxOpt <- Canvas.getContext2d canvas
  whenSomeE ctxOpt $ \ctx -> boot canvas ctx

boot ::
  Effect f ('MutableObject Dom.DomElement)
  -> Expr f ('MutableObject Canvas.Context2D)
  -> EffectSyntax f (f 'Unit)
boot canvas ctx = do
  ctxH <- hold (expr ctx)
  _ <- Canvas.setCanvasWidth canvas (number canvasW)
  _ <- Canvas.setCanvasHeight canvas (number canvasH)
  toSyntax_
    $ discard
    $ ffi
      "((canvas, ctx, w, h) => { canvas.style.width = w + 'px'; canvas.style.height = h + 'px'; ctx.imageSmoothingEnabled = false; })"
      ( ArgEffect canvas
          <: ArgEffect ctxH
          <: arg (number canvasW)
          <: arg (number canvasH)
          <: RecNil
      )
  state <- initLife ctxH
  registry <- initRegistry
  img <- bindExpr =<< createImageData ctxH (number canvasW) (number canvasH)
  meter <- hold (G.toObject (Fps (-1) 0))
  wire canvas state
  Timers.foreverFrame $ \now -> do
    tickFps meter now
    paused <- state.paused
    whenS (not_ paused) (stepLife state registry)
    renderLife ctxH img state
    paintHud ctxH state meter

wire ::
  Effect f ('MutableObject Dom.DomElement)
  -> Effect f (MutableObjectOf LifeState)
  -> EffectSyntax f (f 'Unit)
wire canvas state = do
  addEventListener "keydown" canvas $ \(e :: Expr f ('MutableObject ())) ->
    stmts $ do
      code <- getProp' e "code"
      toSyntax $
        stringCaseE
          code
          [
            ( "Space"
            , discard $
                stmts $ do
                  toSyntax_ $ callMethod (expr e) "preventDefault" RecNil
                  togglePause state
                  done
            )
          ]
          noOp
  addEventListener "click" canvas $ \(e :: Expr f ('MutableObject ())) ->
    stmts $ do
      cx <- getProp' e "clientX"
      cy <- getProp' e "clientY"
      rect <- hold $ callMethod canvas "getBoundingClientRect" RecNil
      left <- getProp rect "left"
      top <- getProp rect "top"
      let
        gx = Math.floor ((cx - left) / number (fromIntegral cellPx))
        gy = Math.floor ((cy - top) / number (fromIntegral cellPx))
      flipCell state gx gy
  done

paintHud ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Effect f (MutableObjectOf LifeState)
  -> Effect f (MutableObjectOf Fps)
  -> EffectSyntax f (f 'Unit)
paintHud ctx state meter = do
  gen <- state.gen
  pop <- state.pop
  paused <- state.paused
  recent <- state.recentDiscover
  fpsN <- meter.fps
  set @"font" ctx (string "15px Georgia")
  fill ctx (string ink)
  _ <-
    Canvas.fillText
      ctx
      (string "Gen: " <> toString gen <> string "  Cells: " <> toString pop)
      8
      18
  set @"textAlign" ctx (string "center")
  _ <-
    Canvas.fillText ctx (string "FPS: " <> toString fpsN) (number (canvasW / 2)) 18
  set @"textAlign" ctx (string "right")
  ifS
    paused
    (Canvas.fillText ctx (string "paused") (number (canvasW - 8)) 18)
    (Canvas.fillText ctx (string "running") (number (canvasW - 8)) 18)
  set @"textAlign" ctx (string "left")
  set @"font" ctx (string "13px Georgia")
  _ <-
    Canvas.fillText
      ctx
      (string "Discovered: " <> recent)
      8
      (number canvasH - 6)
  done

tickFps ::
  Effect f (MutableObjectOf Fps) -> Expr f 'Number -> EffectSyntax f (f 'Unit)
tickFps meter now = do
  prev <- meter.lastMs
  let
    dt = now - prev
  whenS (prev .>= 0 .&& dt .<= 250) $
    set @"fps" meter (Math.round (number 1000 / Math.max 1 dt))
  set @"lastMs" meter now

fill ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
fill ctx col = set @"fillStyle" ctx col
