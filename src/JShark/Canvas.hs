{-# LANGUAGE
    DataKinds
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
#-}
-- | Canvas 2D. Get a context from a canvas element, then draw.
-- Styles are 'Field's ('fillStyle', 'strokeStyle', 'lineWidth', …).
module JShark.Canvas
  ( Context2D
  , TextMetrics
  , getContext2d
  , canvasWidth
  , canvasHeight
  , setCanvasWidth
  , setCanvasHeight
  , fillRect
  , strokeRect
  , clearRect
  , beginPath
  , closePath
  , moveTo
  , lineTo
  , arc
  , fill
  , stroke
  , fillText
  , strokeText
  , measureText
  , save
  , restore
  , translate
  , rotate
  , scale
  ) where

import JShark.Api
import JShark.Dom (DomElement)
import JShark.Rec ((<:), Rec(..))
import JShark.Types

-- | @CanvasRenderingContext2D@.
data Context2D

-- | CSS color strings. Gradients / patterns are not in the 'Field' yet.
type instance Field Context2D "fillStyle" = 'String
type instance Field Context2D "strokeStyle" = 'String
type instance Field Context2D "lineWidth" = 'Number
type instance Field Context2D "font" = 'String
type instance Field Context2D "textAlign" = 'String
type instance Field Context2D "globalAlpha" = 'Number

-- | @TextMetrics@ from 'measureText'.
data TextMetrics

type instance Field TextMetrics "width" = 'Number

-- | @el.getContext("2d")@. 'none' when the element is not a canvas.
-- Held so reuse does not re-run 'getContext'.
getContext2d ::
     Effect f ('Object DomElement)
  -> EffectSyntax f (Effect f ('Option ('Object Context2D)))
getContext2d el =
  hold $
    Bind
      (callMethod el "getContext" (arg (string "2d") <: RecNil))
      (\x -> Lift (unsafeNullable (Var x)))

-- | @HTMLCanvasElement.width@ / @height@ (drawing buffer, not CSS).
-- Assigning either resets the bitmap.
canvasWidth :: Effect f ('Object DomElement) -> EffectSyntax f (Expr f 'Number)
canvasWidth el = getProp el "width"

canvasHeight :: Effect f ('Object DomElement) -> EffectSyntax f (Expr f 'Number)
canvasHeight el = getProp el "height"

setCanvasWidth :: Effect f ('Object DomElement) -> Expr f 'Number -> EffectSyntax f (f 'Unit)
setCanvasWidth el n = setProp el "width" n

setCanvasHeight :: Effect f ('Object DomElement) -> Expr f 'Number -> EffectSyntax f (f 'Unit)
setCanvasHeight el n = setProp el "height" n

fillRect, strokeRect, clearRect ::
     Effect f ('Object Context2D)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
fillRect ctx x y w h =
  toSyntax $ callMethod ctx "fillRect" (arg x <: arg y <: arg w <: arg h <: RecNil)
strokeRect ctx x y w h =
  toSyntax $ callMethod ctx "strokeRect" (arg x <: arg y <: arg w <: arg h <: RecNil)
clearRect ctx x y w h =
  toSyntax $ callMethod ctx "clearRect" (arg x <: arg y <: arg w <: arg h <: RecNil)

beginPath, closePath, fill, stroke, save, restore ::
     Effect f ('Object Context2D)
  -> EffectSyntax f (f 'Unit)
beginPath ctx = toSyntax $ callMethod ctx "beginPath" RecNil
closePath ctx = toSyntax $ callMethod ctx "closePath" RecNil
fill ctx = toSyntax $ callMethod ctx "fill" RecNil
stroke ctx = toSyntax $ callMethod ctx "stroke" RecNil
save ctx = toSyntax $ callMethod ctx "save" RecNil
restore ctx = toSyntax $ callMethod ctx "restore" RecNil

moveTo, lineTo ::
     Effect f ('Object Context2D)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
moveTo ctx x y = toSyntax $ callMethod ctx "moveTo" (arg x <: arg y <: RecNil)
lineTo ctx x y = toSyntax $ callMethod ctx "lineTo" (arg x <: arg y <: RecNil)

-- | @ctx.arc(x, y, r, start, end)@. Clockwise.
arc ::
     Effect f ('Object Context2D)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
arc ctx x y r start end =
  toSyntax $
    callMethod ctx "arc" (arg x <: arg y <: arg r <: arg start <: arg end <: RecNil)

fillText, strokeText ::
     Effect f ('Object Context2D)
  -> Expr f 'String
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
fillText ctx t x y =
  toSyntax $ callMethod ctx "fillText" (arg t <: arg x <: arg y <: RecNil)
strokeText ctx t x y =
  toSyntax $ callMethod ctx "strokeText" (arg t <: arg x <: arg y <: RecNil)

measureText ::
     Effect f ('Object Context2D)
  -> Expr f 'String
  -> EffectSyntax f (Effect f ('Object TextMetrics))
measureText ctx t = hold $ callMethod ctx "measureText" (arg t <: RecNil)

translate ::
     Effect f ('Object Context2D)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
translate ctx x y =
  toSyntax $ callMethod ctx "translate" (arg x <: arg y <: RecNil)

rotate :: Effect f ('Object Context2D) -> Expr f 'Number -> EffectSyntax f (f 'Unit)
rotate ctx a = toSyntax $ callMethod ctx "rotate" (arg a <: RecNil)

scale ::
     Effect f ('Object Context2D)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
scale ctx x y = toSyntax $ callMethod ctx "scale" (arg x <: arg y <: RecNil)
