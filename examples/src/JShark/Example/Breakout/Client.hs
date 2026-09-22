{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Breakout client: MDN canvas draw loop, Haskell 'Game' row.
module JShark.Example.Breakout.Client (mainJS) where

import GHC.Generics (Generic)
import JShark.Api
import JShark.Api.Generic (MutableObjectOf, SumOf)
import qualified JShark.Api.Generic as G
import JShark.Api.Rec (Rec (..), (<:))
import qualified JShark.Canvas as Canvas
import qualified JShark.Dom as Dom
import JShark.Example.Breakout.Types
import qualified JShark.Math as Math
import qualified JShark.Timers as Timers

-- | One-shot latch so a frame breaks at most one brick.
data Once = Once {fired :: Bool} deriving Generic

-- | Instantaneous frame rate from rAF timestamps. Not game state.
-- @lastMs = -1@ means no sample yet (not @0@, which is a valid rAF time).
data Fps = Fps {lastMs, fps, frameMs :: Double} deriving Generic

type Ctx f = Effect f ('MutableObject Canvas.Context2D)

type St f = Effect f (MutableObjectOf Game)

type Stmt f = EffectSyntax f (f 'Unit)

mainJS :: Stmt f
mainJS = do
  canvas <- Dom.lookupId (string boardId)
  ctxOpt <- Canvas.getContext2dDesync canvas
  whenSomeE ctxOpt $ \ctx -> do
    ctxH <- hold (expr ctx)
    Canvas.setCanvasWidth canvas (number canvasW)
    Canvas.setCanvasHeight canvas (number canvasH)
    state <- hold (G.toObject startGame)
    meter <- hold (G.toObject (Fps (-1) 0 0))
    wire canvas state
    Timers.foreverFrame $ \_ -> do
      t0 <- bindExpr $ ffi "performance.now" RecNil
      step state
      paint ctxH state meter
      t1 <- bindExpr $ ffi "performance.now" RecNil
      updateFrameMeter meter (t1 - t0)

wire :: Effect f ('MutableObject Dom.DomElement) -> St f -> Stmt f
wire canvas state = do
  addEventListenerS "keydown" window $ \e -> do
    code <- eventCode e
    let
      restart = do
        toSyntax_ $ callMethod (expr e) "preventDefault" RecNil
        tryRestart state
    toSyntax
      $ stringCaseE code [("Space", discard (stmts restart))]
      $ stmts (bindArrows state code true_)
  addEventListenerS "keyup" window $ \e -> do
    code <- eventCode e
    bindArrows state code false_
  addEventListenerS "mousemove" canvas $ \e -> do
    cx <- eventClientX e
    rect <- hold $ callMethod canvas "getBoundingClientRect" RecNil
    left <- getProp rect "left"
    whenPlay state $ do
      pad <- state.paddle
      set @"px" pad (clampPaddle ((cx - left) - number (paddleW / 2)))
  done

bindArrows :: St f -> Expr f 'String -> Expr f 'Bool -> Stmt f
bindArrows state code held =
  toSyntax $
    stringCaseE
      code
      [ ("ArrowRight", discard (stmts $ set @"rightOn" state held))
      , ("ArrowLeft", discard (stmts $ set @"leftOn" state held))
      ]
      noOp

-- | In-place overwrite so the rAF closure keeps the same object identity.
tryRestart :: St f -> Stmt f
tryRestart state =
  unlessPlay state $ hold (G.toObject startGame) >>= assign state

step :: St f -> Stmt f
step state = whenPlay state $ do
  movePaddle state
  collideBricks state
  bounce state
  advanceBall state

movePaddle :: St f -> Stmt f
movePaddle state = do
  pad <- state.paddle
  px0 <- pad.px
  goR <- state.rightOn
  goL <- state.leftOn
  whenS (goR .&& px0 .< number paddleMaxX) $
    set @"px" pad (px0 + number paddleSpeed) *> done
  px1 <- pad.px
  whenS (goL .&& px1 .> 0) $ set @"px" pad (px1 - number paddleSpeed) *> done

advanceBall :: St f -> Stmt f
advanceBall state = do
  b <- state.ball
  bx0 <- b.x
  by0 <- b.y
  ddx <- b.dx
  ddy <- b.dy
  set @"x" b (bx0 + ddx)
  set @"y" b (by0 + ddy)

collideBricks :: St f -> Stmt f
collideBricks state = do
  b <- state.ball
  bx0 <- b.x
  by0 <- b.y
  field <- state.bricks
  latch <- hold (G.toObject (Once False))
  forEach_ field $ \br -> do
    spent <- latch.fired
    on <- br.alive
    whenS (not_ spent .&& on) $ do
      rx <- br.bx
      ry <- br.by
      let
        inY = by0 .> ry .&& by0 .< (ry + number brickH)
      whenS (bx0 .> rx .&& bx0 .< (rx + number brickW) .&& inY) $ do
        set @"alive" br false_
        set @"fired" latch true_
        ddy <- b.dy
        set @"dy" b (negate ddy)
        sc <- state.score
        set @"score" state (sc + 1)
        sc1 <- state.score
        whenS (sc1 .== number (fromIntegral brickCount)) $ setPhase state Win

-- | Walls and ceiling reflect; the floor bounces off the paddle or costs a
-- life.
bounce :: St f -> Stmt f
bounce state = do
  b <- state.ball
  pad <- state.paddle
  bx0 <- b.x
  by0 <- b.y
  ddx <- b.dx
  ddy <- b.dy
  px0 <- pad.px
  let
    r = number ballR
    nx = bx0 + ddx
    ny = by0 + ddy
    halfPaddle = number (paddleW / 2)
    kick = (bx0 - (px0 + halfPaddle)) / halfPaddle * 3
  whenS (nx .> (number canvasW - r) .|| nx .< r) $
    set @"dx" b (negate ddx) *> done
  whenS (ny .< r) $ set @"dy" b (negate ddy) *> done
  whenS (ny .>= r .&& ny .> (number canvasH - r)) $
    ifS
      (bx0 .> px0 .&& bx0 .< (px0 + number paddleW))
      ( do
          set @"dx" b kick
          ddy1 <- b.dy
          set @"dy" b (negate (abs ddy1))
      )
      ( do
          lv <- state.lives
          set @"lives" state (lv - 1)
          lv1 <- state.lives
          ifS (lv1 .<= 0) (setPhase state Lose) (resetBall state)
      )
  done

resetBall :: St f -> Stmt f
resetBall state = do
  b <- toSyntax (G.toObject startBall)
  set @"ball" state (var b)
  p <- toSyntax (G.toObject startPaddle)
  set @"paddle" state (var p)

paint :: Ctx f -> St f -> Effect f (MutableObjectOf Fps) -> Stmt f
paint ctx state meter = do
  fill ctx (string boardFill)
  Canvas.fillRect ctx 0 0 (number canvasW) (number canvasH)
  drawBricks ctx state
  drawBall ctx state
  drawPaddle ctx state
  drawHud ctx state meter
  unlessPlay state $ drawBanner ctx state

drawBricks :: Ctx f -> St f -> Stmt f
drawBricks ctx state = do
  field <- state.bricks
  forEach_ field $ \br -> do
    on <- br.alive
    whenS on $ do
      rx <- br.bx
      ry <- br.by
      col <- br.color
      fill ctx col
      Canvas.fillRect ctx rx ry (number brickW) (number brickH)
      done

drawBall :: Ctx f -> St f -> Stmt f
drawBall ctx state = do
  b <- state.ball
  bx0 <- b.x
  by0 <- b.y
  Canvas.beginPath ctx
  Canvas.arc ctx bx0 by0 (number ballR) 0 (pi * 2)
  fill ctx (string ballFill)
  Canvas.fill ctx
  Canvas.closePath ctx
  done

drawPaddle :: Ctx f -> St f -> Stmt f
drawPaddle ctx state = do
  pad <- state.paddle
  px0 <- pad.px
  fill ctx (string ink)
  let
    h = number paddleH
  Canvas.fillRect ctx px0 (number (canvasH - paddleH)) (number paddleW) h
  done

drawHud :: Ctx f -> St f -> Effect f (MutableObjectOf Fps) -> Stmt f
drawHud ctx state meter = do
  sc <- state.score
  lv <- state.lives
  n <- meter.fps
  ms <- meter.frameMs
  scoreTxt <-
    bindExpr $
      ffi "(sc)=>('Score '+String(sc).padStart(4,'\\u00a0'))" (arg sc <: RecNil)
  fpsTxt <-
    bindExpr $
      ffi
        ( "(fps,ms)=>"
            <> "'FPS '+String(Math.round(fps)).padStart(3,'\\u00a0')"
            <> "+' ('+String(Math.round(ms)).padStart(4,'\\u00a0')+'ms)'"
        )
        (arg n <: arg ms <: RecNil)
  livesTxt <-
    bindExpr $
      ffi "(lv)=>('Lives '+String(lv).padStart(2,'\\u00a0'))" (arg lv <: RecNil)
  set @"font" ctx (string "16px ui-monospace, monospace")
  fill ctx (string ink)
  Canvas.fillText ctx scoreTxt 8 20
  align ctx "center"
  Canvas.fillText ctx fpsTxt (number (canvasW / 2)) 20
  align ctx "right"
  Canvas.fillText ctx livesTxt (number (canvasW - 8)) 20
  align ctx "left"
  done

drawBanner :: Ctx f -> St f -> Stmt f
drawBanner ctx state = do
  ph <- phaseSum state
  set @"font" ctx (string "28px Georgia")
  fill ctx (string bannerFill)
  align ctx "center"
  toSyntax
    $ G.caseSum ph
    $ G.on @"Play" (\_ -> noOp)
    $ G.on @"Win" (\_ -> stmts $ bannerText ctx (string "You win"))
    $ G.Case_ (\_ -> stmts $ bannerText ctx (string "Game over"))
  align ctx "left"

bannerText :: Ctx f -> Expr f 'String -> Stmt f
bannerText ctx msg = do
  let
    cx = number (canvasW / 2)
    cy = canvasH / 2
  Canvas.fillText ctx msg cx (number cy)
  set @"font" ctx (string "14px Georgia")
  Canvas.fillText ctx (string "Space to play again") cx (number (cy + 28))
  done

updateFrameMeter :: Effect f (MutableObjectOf Fps) -> Expr f 'Number -> Stmt f
updateFrameMeter meter elapsedMs = do
  prevFps <- meter.fps
  let
    instant = if_ (elapsedMs .> number 0) (number 1000 / elapsedMs) (number 0)
    smoothed = prevFps * number 0.85 + instant * number 0.15
  set @"frameMs" meter elapsedMs
  set @"fps" meter (Math.round smoothed)
  done

clampPaddle :: Expr f 'Number -> Expr f 'Number
clampPaddle = Math.max 0 . Math.min (number paddleMaxX)

fill :: Ctx f -> Expr f 'String -> Stmt f
fill = set @"fillStyle"

align :: Ctx f -> Expr f 'String -> Stmt f
align = set @"textAlign"

phaseSum :: St f -> EffectSyntax f (Effect f (SumOf Phase))
phaseSum state = fmap toEffect state.phase

onPhase :: St f -> Effect f 'Unit -> Effect f 'Unit -> Stmt f
onPhase state play miss = do
  ph <- phaseSum state
  toSyntax $ G.caseSum ph $ G.on @"Play" (\_ -> play) $ G.Case_ (\_ -> miss)

whenPlay, unlessPlay :: St f -> Stmt f -> Stmt f
whenPlay state body = onPhase state (stmts body) noOp
unlessPlay state body = onPhase state noOp (stmts body)

setPhase :: St f -> Phase -> Stmt f
setPhase state p = do
  s <- toSyntax (G.toSum p)
  set @"phase" state (var s)
