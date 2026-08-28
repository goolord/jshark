{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Breakout client: MDN canvas draw loop, Haskell 'Game' row.
module Client (mainJS) where

import GHC.Generics (Generic)
import JShark.Api
import JShark.Api.Generic (MutableObjectOf, SumOf)
import qualified JShark.Api.Generic as G
import JShark.Api.Rec (Rec (..), (<:))
import qualified JShark.Canvas as Canvas
import qualified JShark.Dom as Dom
import qualified JShark.Math as Math
import qualified JShark.Timers as Timers
import Types
  ( Ball
  , Game
  , Phase (..)
  , ballFill
  , ballR
  , bannerFill
  , boardFill
  , boardId
  , brickCount
  , brickH
  , brickW
  , canvasH
  , canvasW
  , ink
  , paddleH
  , paddleMaxX
  , paddleSpeed
  , paddleW
  , startBall
  , startGame
  , startPaddle
  )

-- | One-shot latch so a frame breaks at most one brick.
data Once = Once
  { fired :: Bool
  }
  deriving Generic

-- | Instantaneous frame rate from rAF timestamps. Not game state.
-- @lastMs = -1@ means no sample yet (not @0@, which is a valid rAF time).
data Fps = Fps
  { lastMs :: Double
  , fps :: Double
  , frameMs :: Double
  }
  deriving Generic

mainJS :: forall f. EffectSyntax f (f 'Unit)
mainJS = do
  canvas <- Dom.lookupId (string boardId)
  ctxOpt <- Canvas.getContext2dDesync canvas
  whenSomeE ctxOpt $ \ctx -> boot canvas ctx

boot ::
  Effect f ('MutableObject Dom.DomElement)
  -> Expr f ('MutableObject Canvas.Context2D)
  -> EffectSyntax f (f 'Unit)
boot canvas ctx = do
  ctxH <- hold (expr ctx)
  _ <- Canvas.setCanvasWidth canvas (number canvasW)
  _ <- Canvas.setCanvasHeight canvas (number canvasH)
  state <- hold (G.toObject startGame)
  meter <- hold (G.toObject (Fps (-1) 0 0))
  wire canvas state
  Timers.foreverFrame $ \_ -> do
    t0 <- bindExpr $ ffi "performance.now" RecNil
    step state
    paint ctxH state meter
    t1 <- bindExpr $ ffi "performance.now" RecNil
    updateFrameMeter meter (t1 - t0)

wire ::
  Effect f ('MutableObject Dom.DomElement)
  -> Effect f (MutableObjectOf Game)
  -> EffectSyntax f (f 'Unit)
wire canvas state = do
  addEventListener "keydown" window $ \(e :: Expr f ('MutableObject ())) ->
    stmts $ do
      code <- getProp' e "code"
      toSyntax $
        stringCaseE
          code
          [
            ( "Space"
            , discard
                ( stmts $ do
                    toSyntax_ $ callMethod (expr e) "preventDefault" RecNil
                    tryRestart state
                )
            )
          ]
          (stmts $ bindArrows state code true_)
  addEventListener "keyup" window $ \(e :: Expr f ('MutableObject ())) ->
    stmts $ do
      code <- getProp' e "code"
      bindArrows state code false_
  addEventListener "mousemove" canvas $ \(e :: Expr f ('MutableObject ())) ->
    stmts $ do
      cx <- getProp' e "clientX"
      rect <- hold $ callMethod canvas "getBoundingClientRect" RecNil
      left <- getProp rect "left"
      whenPlay state $ do
        pad <- state.paddle
        set @"px" pad (clampPaddle ((cx - left) - number (paddleW / 2)))
  done

bindArrows ::
  Effect f (MutableObjectOf Game)
  -> Expr f 'String
  -> Expr f 'Bool
  -> EffectSyntax f (f 'Unit)
bindArrows state code held =
  toSyntax $
    stringCaseE
      code
      [ ("ArrowRight", discard (stmts $ set @"rightOn" state held))
      , ("ArrowLeft", discard (stmts $ set @"leftOn" state held))
      ]
      noOp

-- | In-place overwrite so the rAF closure keeps the same object identity.
tryRestart :: Effect f (MutableObjectOf Game) -> EffectSyntax f (f 'Unit)
tryRestart state =
  unlessPlay state $ do
    fresh <- hold (G.toObject startGame)
    assign state fresh

step :: Effect f (MutableObjectOf Game) -> EffectSyntax f (f 'Unit)
step state =
  whenPlay state $ do
    movePaddle state
    collideBricks state
    bounce state
    advanceBall state

movePaddle :: Effect f (MutableObjectOf Game) -> EffectSyntax f (f 'Unit)
movePaddle state = do
  pad <- state.paddle
  px0 <- pad.px
  goR <- state.rightOn
  goL <- state.leftOn
  whenS (goR .&& px0 .< number paddleMaxX) $
    do
      set @"px" pad (px0 + number paddleSpeed)
      done
  px1 <- pad.px
  whenS (goL .&& px1 .> 0) $
    do
      set @"px" pad (px1 - number paddleSpeed)
      done

advanceBall :: Effect f (MutableObjectOf Game) -> EffectSyntax f (f 'Unit)
advanceBall state = do
  b <- state.ball
  bx0 <- b.x
  by0 <- b.y
  ddx <- b.dx
  ddy <- b.dy
  set @"x" b (bx0 + ddx)
  set @"y" b (by0 + ddy)

collideBricks :: Effect f (MutableObjectOf Game) -> EffectSyntax f (f 'Unit)
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
      whenS (hitsRect bx0 by0 rx ry (number brickW) (number brickH)) $ do
        set @"alive" br false_
        set @"fired" latch true_
        ddy <- b.dy
        set @"dy" b (negate ddy)
        sc <- state.score
        set @"score" state (sc + 1)
        sc1 <- state.score
        whenS (sc1 .== number (fromIntegral brickCount)) $
          setPhase state Win

bounce :: Effect f (MutableObjectOf Game) -> EffectSyntax f (f 'Unit)
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
    w = number canvasW
    h = number canvasH
    nx = bx0 + ddx
    ny = by0 + ddy
  bounceWalls b ddx r w nx
  bounceFloor state b bx0 ddy px0 r h ny

bounceWalls ::
  Expr f (MutableObjectOf Ball)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
bounceWalls b ddx r w nx =
  whenS (nx .> (w - r) .|| nx .< r) $
    do
      set @"dx" b (negate ddx)
      done

bounceFloor ::
  Effect f (MutableObjectOf Game)
  -> Expr f (MutableObjectOf Ball)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
bounceFloor state b bx0 ddy px0 r h ny =
  do
    whenS (ny .< r) $
      do
        set @"dy" b (negate ddy)
        done
    whenS (ny .>= r .&& ny .> (h - r)) $
      ifS
        (overlapsPaddle bx0 px0)
        ( do
            set @"dx" b (paddleKick bx0 px0)
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

resetBall :: Effect f (MutableObjectOf Game) -> EffectSyntax f (f 'Unit)
resetBall state = do
  b <- toSyntax (G.toObject startBall)
  set @"ball" state (var b)
  p <- toSyntax (G.toObject startPaddle)
  set @"paddle" state (var p)

paint ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Effect f (MutableObjectOf Game)
  -> Effect f (MutableObjectOf Fps)
  -> EffectSyntax f (f 'Unit)
paint ctx state meter = do
  fill ctx (string boardFill)
  _ <- Canvas.fillRect ctx 0 0 (number canvasW) (number canvasH)
  sequence_
    [ drawBricks ctx state
    , drawBall ctx state
    , drawPaddle ctx state
    , drawHud ctx state meter
    ]
  unlessPlay state $ drawBanner ctx state

drawBricks ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Effect f (MutableObjectOf Game)
  -> EffectSyntax f (f 'Unit)
drawBricks ctx state = do
  field <- state.bricks
  forEach_ field $ \br -> do
    on <- br.alive
    whenS on $ do
      rx <- br.bx
      ry <- br.by
      col <- br.color
      fill ctx col
      _ <- Canvas.fillRect ctx rx ry (number brickW) (number brickH)
      done

drawBall ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Effect f (MutableObjectOf Game)
  -> EffectSyntax f (f 'Unit)
drawBall ctx state = do
  b <- state.ball
  bx0 <- b.x
  by0 <- b.y
  _ <- Canvas.beginPath ctx
  _ <- Canvas.arc ctx bx0 by0 (number ballR) 0 (pi * 2)
  fill ctx (string ballFill)
  _ <- Canvas.fill ctx
  _ <- Canvas.closePath ctx
  done

drawPaddle ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Effect f (MutableObjectOf Game)
  -> EffectSyntax f (f 'Unit)
drawPaddle ctx state = do
  pad <- state.paddle
  px0 <- pad.px
  fill ctx (string ink)
  _ <-
    Canvas.fillRect
      ctx
      px0
      (number (canvasH - paddleH))
      (number paddleW)
      (number paddleH)
  done

drawHud ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Effect f (MutableObjectOf Game)
  -> Effect f (MutableObjectOf Fps)
  -> EffectSyntax f (f 'Unit)
drawHud ctx state meter = do
  sc <- state.score
  lv <- state.lives
  n <- meter.fps
  ms <- meter.frameMs
  scoreTxt <-
    bindExpr $
      ffi
        "(sc)=>('Score '+String(sc).padStart(4,'\\u00a0'))"
        (arg sc <: RecNil)
  fpsTxt <-
    bindExpr $
      ffi
        ( "(fps,ms)=>"
            ++ "'FPS '+String(Math.round(fps)).padStart(3,'\\u00a0')"
            ++ "+' ('+String(Math.round(ms)).padStart(4,'\\u00a0')+'ms)'"
        )
        (arg n <: arg ms <: RecNil)
  livesTxt <-
    bindExpr $
      ffi
        "(lv)=>('Lives '+String(lv).padStart(2,'\\u00a0'))"
        (arg lv <: RecNil)
  set @"font" ctx (string "16px ui-monospace, monospace")
  fill ctx (string ink)
  _ <- Canvas.fillText ctx scoreTxt 8 20
  set @"textAlign" ctx (string "center")
  _ <- Canvas.fillText ctx fpsTxt (number (canvasW / 2)) 20
  set @"textAlign" ctx (string "right")
  _ <- Canvas.fillText ctx livesTxt (number (canvasW - 8)) 20
  set @"textAlign" ctx (string "left")
  done

drawBanner ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Effect f (MutableObjectOf Game)
  -> EffectSyntax f (f 'Unit)
drawBanner ctx state = do
  ph <- phaseSum state
  set @"font" ctx (string "28px Georgia")
  fill ctx (string bannerFill)
  set @"textAlign" ctx (string "center")
  toSyntax
    $ G.caseSum ph
    $ G.on @"Play" (\_ -> noOp)
    $ G.on @"Win" (\_ -> stmts $ bannerText ctx (string "You win"))
    $ G.Case_ (\_ -> stmts $ bannerText ctx (string "Game over"))
  set @"textAlign" ctx (string "left")

bannerText ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
bannerText ctx msg = do
  _ <- Canvas.fillText ctx msg (number (canvasW / 2)) (number (canvasH / 2))
  set @"font" ctx (string "14px Georgia")
  _ <-
    Canvas.fillText
      ctx
      (string "Space to play again")
      (number (canvasW / 2))
      (number (canvasH / 2 + 28))
  done

updateFrameMeter ::
  Effect f (MutableObjectOf Fps) -> Expr f 'Number -> EffectSyntax f (f 'Unit)
updateFrameMeter meter elapsedMs = do
  prevFps <- meter.fps
  let
    instant =
      if_ (elapsedMs .> number 0) (number 1000 / elapsedMs) (number 0)
    smoothed = prevFps * number 0.85 + instant * number 0.15
  set @"frameMs" meter elapsedMs
  set @"fps" meter (Math.round smoothed)
  done

-- Helpers -----------------------------------------------------------------

clampPaddle :: Expr f 'Number -> Expr f 'Number
clampPaddle = Math.max 0 . Math.min (number paddleMaxX)

hitsRect ::
  Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Bool
hitsRect cx cy rx ry w h =
  cx .> rx .&& cx .< (rx + w) .&& cy .> ry .&& cy .< (ry + h)

overlapsPaddle :: Expr f 'Number -> Expr f 'Number -> Expr f 'Bool
overlapsPaddle ballX paddleX =
  ballX .> paddleX .&& ballX .< (paddleX + number paddleW)

paddleKick :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
paddleKick ballX paddleX =
  let
    mid = paddleX + number (paddleW / 2)
    hit = (ballX - mid) / number (paddleW / 2)
   in
    hit * 3

fill ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
fill ctx col = set @"fillStyle" ctx col

phaseSum ::
  Effect f (MutableObjectOf Game) -> EffectSyntax f (Effect f (SumOf Phase))
phaseSum state = fmap toEffect (state.phase)

onPhase ::
  Effect f (MutableObjectOf Game)
  -> Effect f 'Unit
  -> Effect f 'Unit
  -> EffectSyntax f (f 'Unit)
onPhase state play miss = do
  ph <- phaseSum state
  toSyntax
    $ G.caseSum ph
    $ G.on @"Play" (\_ -> play)
    $ G.Case_ (\_ -> miss)

whenPlay ::
  Effect f (MutableObjectOf Game)
  -> EffectSyntax f (f 'Unit)
  -> EffectSyntax f (f 'Unit)
whenPlay state body = onPhase state (stmts body) noOp

unlessPlay ::
  Effect f (MutableObjectOf Game)
  -> EffectSyntax f (f 'Unit)
  -> EffectSyntax f (f 'Unit)
unlessPlay state body = onPhase state noOp (stmts body)

setPhase :: Effect f (MutableObjectOf Game) -> Phase -> EffectSyntax f (f 'Unit)
setPhase state p = do
  s <- toSyntax (G.toSum p)
  set @"phase" state (var s)
