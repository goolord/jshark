{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveGeneric
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
#-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Breakout client: MDN canvas draw loop, Haskell 'Game' row.
module Client (mainJS) where

import GHC.Generics (Generic)
import qualified JShark.Canvas as Canvas
import qualified JShark.Dom as Dom
import qualified JShark.Generic as G
import qualified JShark.Math as Math
import qualified JShark.Timers as Timers
import JShark.Api
import JShark.Generic (ObjectOf, SumOf)
import JShark.Rec (Rec(..), (<:))
import JShark.Types
import Types
  ( Game
  , Phase(..)
  , ballFill
  , ballR
  , bannerFill
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
data Once = Once { fired :: Bool }
  deriving (Generic)

mainJS :: forall f. EffectSyntax f (f 'Unit)
mainJS = do
  canvas <- Dom.lookupId (string boardId)
  opt <- Canvas.getContext2d canvas
  toSyntax $
    Bind opt $ \o ->
      optionCaseE (var o) noOp $ \ctx0 ->
        stmts $ boot canvas (Lift ctx0)

boot ::
     Effect f ('Object Dom.DomElement)
  -> Effect f ('Object Canvas.Context2D)
  -> EffectSyntax f (f 'Unit)
boot canvas ctx = do
  ctxH <- hold ctx
  _ <- Canvas.setCanvasWidth canvas (number canvasW)
  _ <- Canvas.setCanvasHeight canvas (number canvasH)
  g0 <- toSyntax (G.toObject startGame)
  state <- hold (Lift (Var g0))
  wire canvas state
  toSyntax $
    bindRec
      (\frame ->
         LambdaE $ \(_ :: f 'Unit) ->
           stmts $ do
             step state
             paint ctxH state
             _ <- Timers.requestAnimationFrame $ \_ -> stmts $ call0 frame
             done)
      (\frame -> stmts $ call0 frame)

wire ::
     Effect f ('Object Dom.DomElement)
  -> Effect f (ObjectOf Game)
  -> EffectSyntax f (f 'Unit)
wire canvas state = do
  addEventListener "keydown" window $ \(e :: f ('Object ())) ->
    stmts $ do
      code <- getProp (Lift (Var e)) "code"
      bindArrows state code true_
      ifS (code .== "Space") (do
        toSyntax $ callMethod (Lift (Var e)) "preventDefault" RecNil
        tryRestart state) done
  addEventListener "keyup" window $ \(e :: f ('Object ())) ->
    stmts $ do
      code <- getProp (Lift (Var e)) "code"
      bindArrows state code false_
  addEventListener "mousemove" canvas $ \(e :: f ('Object ())) ->
    stmts $ do
      cx <- getProp (Lift (Var e)) "clientX"
      rect <- hold $ callMethod canvas "getBoundingClientRect" RecNil
      left <- getProp rect "left"
      whenPlay state $ do
        pad <- get @"paddle" state
        set @"px" pad (clampPaddle ((cx - left) - number (paddleW / 2)))
  done

bindArrows ::
     Effect f (ObjectOf Game)
  -> Expr f 'String
  -> Expr f 'Bool
  -> EffectSyntax f (f 'Unit)
bindArrows state code held = do
  ifS (code .== "ArrowRight") (set @"rightOn" state held) done
  ifS (code .== "ArrowLeft") (set @"leftOn" state held) done

tryRestart :: Effect f (ObjectOf Game) -> EffectSyntax f (f 'Unit)
tryRestart state =
  unlessPlay state $ do
    g0 <- toSyntax (G.toObject startGame)
    copyGame state (Lift (Var g0))

-- | In-place overwrite so the rAF closure keeps the same object identity.
-- 'Object.assign' copies every enumerable field; new 'Game' keys come along.
copyGame :: Effect f (ObjectOf Game) -> Effect f (ObjectOf Game) -> EffectSyntax f (f 'Unit)
copyGame dst src = do
  toSyntax_ $ ffi "Object.assign" (ArgEffect dst <: ArgEffect src <: RecNil)
  done

step :: Effect f (ObjectOf Game) -> EffectSyntax f (f 'Unit)
step state =
  whenPlay state $ do
    movePaddle state
    collideBricks state
    bounce state
    advanceBall state

movePaddle :: Effect f (ObjectOf Game) -> EffectSyntax f (f 'Unit)
movePaddle state = do
  pad <- get @"paddle" state
  px0 <- get @"px" pad
  goR <- get @"rightOn" state
  goL <- get @"leftOn" state
  ifS (goR .&& px0 .< number paddleMaxX) (set @"px" pad (px0 + number paddleSpeed)) done
  px1 <- get @"px" pad
  ifS (goL .&& px1 .> 0) (set @"px" pad (px1 - number paddleSpeed)) done

advanceBall :: Effect f (ObjectOf Game) -> EffectSyntax f (f 'Unit)
advanceBall state = do
  b <- get @"ball" state
  bx0 <- get @"x" b
  by0 <- get @"y" b
  ddx <- get @"dx" b
  ddy <- get @"dy" b
  set @"x" b (bx0 + ddx)
  set @"y" b (by0 + ddy)

collideBricks :: Effect f (ObjectOf Game) -> EffectSyntax f (f 'Unit)
collideBricks state = do
  b <- get @"ball" state
  bx0 <- get @"x" b
  by0 <- get @"y" b
  field <- get @"bricks" state
  latch <- toSyntax (G.toObject (Once False))
  forEach_ field $ \br -> do
    spent <- get @"fired" latch
    on <- get @"alive" br
    whenS (not_ spent .&& on) $ do
      rx <- get @"bx" br
      ry <- get @"by" br
      whenS (hitsRect bx0 by0 rx ry (number brickW) (number brickH)) $ do
        set @"alive" br false_
        set @"fired" latch true_
        ddy <- get @"dy" b
        set @"dy" b (negate ddy)
        sc <- get @"score" state
        set @"score" state (sc + 1)
        sc1 <- get @"score" state
        whenS (sc1 .== number (fromIntegral brickCount)) $
          setPhase state Win

bounce :: Effect f (ObjectOf Game) -> EffectSyntax f (f 'Unit)
bounce state = do
  b <- get @"ball" state
  pad <- get @"paddle" state
  bx0 <- get @"x" b
  by0 <- get @"y" b
  ddx <- get @"dx" b
  ddy <- get @"dy" b
  px0 <- get @"px" pad
  let r = number ballR
      w = number canvasW
      h = number canvasH
      nx = bx0 + ddx
      ny = by0 + ddy
  whenS (nx .> (w - r) .|| nx .< r) $ set @"dx" b (negate ddx)
  ifS
    (ny .< r)
    (set @"dy" b (negate ddy))
    (whenS (ny .> (h - r)) $
       ifS
         (overlapsPaddle bx0 px0)
         (do
            set @"dx" b (paddleKick bx0 px0)
            ddy1 <- get @"dy" b
            set @"dy" b (negate (abs ddy1)))
         (do
            lv <- get @"lives" state
            set @"lives" state (lv - 1)
            lv1 <- get @"lives" state
            ifS (lv1 .<= 0) (setPhase state Lose) (resetBall state)))

resetBall :: Effect f (ObjectOf Game) -> EffectSyntax f (f 'Unit)
resetBall state = do
  b <- toSyntax (G.toObject startBall)
  set @"ball" state (Var b)
  p <- toSyntax (G.toObject startPaddle)
  set @"paddle" state (Var p)

paint ::
     Effect f ('Object Canvas.Context2D)
  -> Effect f (ObjectOf Game)
  -> EffectSyntax f (f 'Unit)
paint ctx state = do
  _ <- Canvas.clearRect ctx 0 0 (number canvasW) (number canvasH)
  sequence_
    [ drawBricks ctx state
    , drawBall ctx state
    , drawPaddle ctx state
    , drawHud ctx state
    ]
  unlessPlay state $ drawBanner ctx state

drawBricks ::
     Effect f ('Object Canvas.Context2D)
  -> Effect f (ObjectOf Game)
  -> EffectSyntax f (f 'Unit)
drawBricks ctx state = do
  field <- get @"bricks" state
  forEach_ field $ \br -> do
    on <- get @"alive" br
    whenS on $ do
      rx <- get @"bx" br
      ry <- get @"by" br
      col <- get @"color" br
      fill ctx col
      _ <- Canvas.fillRect ctx rx ry (number brickW) (number brickH)
      done

drawBall ::
     Effect f ('Object Canvas.Context2D)
  -> Effect f (ObjectOf Game)
  -> EffectSyntax f (f 'Unit)
drawBall ctx state = do
  b <- get @"ball" state
  bx0 <- get @"x" b
  by0 <- get @"y" b
  _ <- Canvas.beginPath ctx
  _ <- Canvas.arc ctx bx0 by0 (number ballR) 0 (Math.pi * 2)
  fill ctx (string ballFill)
  _ <- Canvas.fill ctx
  _ <- Canvas.closePath ctx
  done

drawPaddle ::
     Effect f ('Object Canvas.Context2D)
  -> Effect f (ObjectOf Game)
  -> EffectSyntax f (f 'Unit)
drawPaddle ctx state = do
  pad <- get @"paddle" state
  px0 <- get @"px" pad
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
     Effect f ('Object Canvas.Context2D)
  -> Effect f (ObjectOf Game)
  -> EffectSyntax f (f 'Unit)
drawHud ctx state = do
  sc <- get @"score" state
  lv <- get @"lives" state
  set @"font" ctx (string "16px Georgia")
  fill ctx (string ink)
  _ <- Canvas.fillText ctx (string "Score: " <> Show sc) 8 20
  _ <- Canvas.fillText ctx (string "Lives: " <> Show lv) (number (canvasW - 80)) 20
  done

drawBanner ::
     Effect f ('Object Canvas.Context2D)
  -> Effect f (ObjectOf Game)
  -> EffectSyntax f (f 'Unit)
drawBanner ctx state = do
  ph <- phaseSum state
  set @"font" ctx (string "28px Georgia")
  fill ctx (string bannerFill)
  set @"textAlign" ctx (string "center")
  toSyntax $
    G.whenTag @"Win" ph
      (\_ -> stmts $ bannerText ctx (string "You win"))
      (stmts $ bannerText ctx (string "Game over"))
  set @"textAlign" ctx (string "left")

bannerText ::
     Effect f ('Object Canvas.Context2D)
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

-- Helpers -----------------------------------------------------------------

clampPaddle :: Expr f 'Number -> Expr f 'Number
clampPaddle = Math.max_ 0 . Math.min_ (number paddleMaxX)

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
  let mid = paddleX + number (paddleW / 2)
      hit = (ballX - mid) / number (paddleW / 2)
   in hit * 3

fill :: Effect f ('Object Canvas.Context2D) -> Expr f 'String -> EffectSyntax f (f 'Unit)
fill ctx col = set @"fillStyle" ctx col

phaseSum :: Effect f (ObjectOf Game) -> EffectSyntax f (Effect f (SumOf Phase))
phaseSum state = fmap toEffect (get @"phase" state)

whenPlay :: Effect f (ObjectOf Game) -> EffectSyntax f (f 'Unit) -> EffectSyntax f (f 'Unit)
whenPlay state body = do
  ph <- phaseSum state
  toSyntax $ G.whenTag @"Play" ph (\_ -> stmts body) noOp

unlessPlay :: Effect f (ObjectOf Game) -> EffectSyntax f (f 'Unit) -> EffectSyntax f (f 'Unit)
unlessPlay state body = do
  ph <- phaseSum state
  toSyntax $ G.whenTag @"Play" ph (\_ -> noOp) (stmts body)

setPhase :: Effect f (ObjectOf Game) -> Phase -> EffectSyntax f (f 'Unit)
setPhase state p = do
  s <- toSyntax (G.toSum p)
  set @"phase" state (Var s)
