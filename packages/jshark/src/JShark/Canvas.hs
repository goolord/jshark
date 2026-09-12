{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Canvas 2D. Get a context from a canvas element, then draw.
-- Styles are 'Field's ('fillStyle', 'strokeStyle', 'lineWidth', …).
module JShark.Canvas
  ( Context2D
  , ImageData
  , TextMetrics
  , getContext2d
  , getContext2dDesync
  , canvasWidth
  , canvasHeight
  , setCanvasWidth
  , setCanvasHeight
  , fillRect
  , rect
  , strokeRect
  , clearRect
  , createImageData
  , putImageData
  , putImageDataRegion
  , imageDataBytes
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
  )
where

import Data.Text (Text)
import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types
import JShark.Dom (DomElement)

-- | Opaque 2D drawing context; style is set through its 'Field's.
data Context2D

-- | CSS color strings. Gradients / patterns are not in the 'Field' yet.
type instance Field Context2D "fillStyle" = 'String

type instance Field Context2D "strokeStyle" = 'String

type instance Field Context2D "lineWidth" = 'Number

type instance Field Context2D "font" = 'String

type instance Field Context2D "textAlign" = 'String

type instance Field Context2D "globalAlpha" = 'Number

-- | Opaque result of 'measureText'; read its @width@ 'Field'.
data TextMetrics

-- | Opaque pixel buffer; read its bytes with 'imageDataBytes'.
data ImageData

type instance Field TextMetrics "width" = 'Number

-- | @el.getContext("2d")@. 'none' when the element is not a canvas.
-- Held so reuse does not re-run 'getContext'.
getContext2d ::
  Effect f ('MutableObject DomElement)
  -> EffectSyntax f (Effect f ('Option ('MutableObject Context2D)))
getContext2d el = getContext2dWith el false_

-- | Like 'getContext2d' with @desynchronized: true@ (and @alpha: false@).
-- Decouples canvas writes from the compositor so frame timing reflects
-- compute cost instead of display refresh.
getContext2dDesync ::
  Effect f ('MutableObject DomElement)
  -> EffectSyntax f (Effect f ('Option ('MutableObject Context2D)))
getContext2dDesync el = getContext2dWith el true_

getContext2dWith ::
  Effect f ('MutableObject DomElement)
  -> Expr f 'Bool
  -> EffectSyntax f (Effect f ('Option ('MutableObject Context2D)))
getContext2dWith el desync =
  hold $
    Bind
      Nothing
      ( ffi
          ( "(el,d)=>el.getContext('2d',"
              <> "{desynchronized:!!d,alpha:false,willReadFrequently:false}"
              <> ")"
          )
          (ArgEffect el <: arg desync <: RecNil)
      )
      (\x -> Lift (unsafeNullable (Var x)))

-- | @HTMLCanvasElement.width@ / @height@ (drawing buffer, not CSS).
-- Assigning either resets the bitmap.
canvasWidth ::
  Effect f ('MutableObject DomElement) -> EffectSyntax f (Expr f 'Number)
canvasWidth el = getProp el "width"

-- | @HTMLCanvasElement.height@. Assigning resets the bitmap.
canvasHeight ::
  Effect f ('MutableObject DomElement) -> EffectSyntax f (Expr f 'Number)
canvasHeight el = getProp el "height"

-- | @el.width = n@. Resets the bitmap.
setCanvasWidth ::
  Effect f ('MutableObject DomElement)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
setCanvasWidth el n = setProp el "width" n

-- | @el.height = n@. Resets the bitmap.
setCanvasHeight ::
  Effect f ('MutableObject DomElement)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
setCanvasHeight el n = setProp el "height" n

ctxCall ::
  Effect f ('MutableObject Context2D)
  -> Text
  -> Rec (Arg f) us
  -> EffectSyntax f (f 'Unit)
ctxCall ctx name args = toSyntax $ callMethod ctx name args

ctxCall0 ::
  Effect f ('MutableObject Context2D) -> Text -> EffectSyntax f (f 'Unit)
ctxCall0 ctx name = ctxCall ctx name RecNil

call2 ::
  Effect f ('MutableObject Context2D)
  -> Text
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
call2 ctx name x y = ctxCall ctx name (arg x <: arg y <: RecNil)

call3 ::
  Effect f ('MutableObject Context2D)
  -> Text
  -> Expr f a
  -> Expr f b
  -> Expr f c
  -> EffectSyntax f (f 'Unit)
call3 ctx name a b c = ctxCall ctx name (arg a <: arg b <: arg c <: RecNil)

call4 ::
  Effect f ('MutableObject Context2D)
  -> Text
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
call4 ctx name x y w h =
  ctxCall ctx name (arg x <: arg y <: arg w <: arg h <: RecNil)

-- | Rectangles at @(x, y, w, h)@: @fillRect@ fills, @strokeRect@ outlines,
-- @clearRect@ erases; @rect@ appends the rectangle to the current path.
fillRect
  , rect
  , strokeRect
  , clearRect ::
    Effect f ('MutableObject Context2D)
    -> Expr f 'Number
    -> Expr f 'Number
    -> Expr f 'Number
    -> Expr f 'Number
    -> EffectSyntax f (f 'Unit)
fillRect ctx = call4 ctx "fillRect"
rect ctx = call4 ctx "rect"
strokeRect ctx = call4 ctx "strokeRect"
clearRect ctx = call4 ctx "clearRect"

-- | @ctx.createImageData(w, h)@ — a transparent pixel buffer.
createImageData ::
  Effect f ('MutableObject Context2D)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Effect f ('MutableObject ImageData))
createImageData ctx w h =
  hold $ callMethod ctx "createImageData" (arg w <: arg h <: RecNil)

-- | @ctx.putImageData(img, x, y)@ — blit the whole image.
putImageData ::
  Effect f ('MutableObject Context2D)
  -> Expr f ('MutableObject ImageData)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
putImageData ctx img x y = do
  toSyntax_
    $ discard
    $ callMethod
      ctx
      "putImageData"
      (arg img <: arg x <: arg y <: RecNil)
  done

-- | @putImageData(img, dx, dy, sx, sy, sw, sh)@ — blit a dirty sub-rectangle.
putImageDataRegion ::
  Effect f ('MutableObject Context2D)
  -> Expr f ('MutableObject ImageData)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
putImageDataRegion ctx img dx dy sx sy sw sh = do
  toSyntax_
    $ discard
    $ callMethod
      ctx
      "putImageData"
      ( arg img
          <: arg dx
          <: arg dy
          <: arg sx
          <: arg sy
          <: arg sw
          <: arg sh
          <: RecNil
      )
  done

-- | @img.data@ — the @Uint8ClampedArray@ bytes (@RGBA@ per pixel).
imageDataBytes ::
  Expr f ('MutableObject ImageData)
  -> EffectSyntax f (Expr f 'Uint8ClampedArray)
imageDataBytes img = getProp (expr img) "data"

-- | Path and state primitives: @beginPath@ / @closePath@ delimit a path,
-- @fill@ / @stroke@ paint it, and @save@ / @restore@ push and pop the
-- drawing state.
beginPath
  , closePath
  , fill
  , stroke
  , save
  , restore ::
    Effect f ('MutableObject Context2D)
    -> EffectSyntax f (f 'Unit)
beginPath ctx = ctxCall0 ctx "beginPath"
closePath ctx = ctxCall0 ctx "closePath"
fill ctx = ctxCall0 ctx "fill"
stroke ctx = ctxCall0 ctx "stroke"
save ctx = ctxCall0 ctx "save"
restore ctx = ctxCall0 ctx "restore"

-- | @ctx.moveTo(x, y)@ / @ctx.lineTo(x, y)@ — current-path cursor.
moveTo
  , lineTo ::
    Effect f ('MutableObject Context2D)
    -> Expr f 'Number
    -> Expr f 'Number
    -> EffectSyntax f (f 'Unit)
moveTo ctx = call2 ctx "moveTo"
lineTo ctx = call2 ctx "lineTo"

-- | @ctx.arc(x, y, r, start, end)@. Clockwise.
arc ::
  Effect f ('MutableObject Context2D)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
arc ctx x y r start end =
  ctxCall ctx "arc" (arg x <: arg y <: arg r <: arg start <: arg end <: RecNil)

-- | @ctx.fillText(text, x, y)@ / @strokeText@ — draw text at @(x, y)@.
fillText
  , strokeText ::
    Effect f ('MutableObject Context2D)
    -> Expr f 'String
    -> Expr f 'Number
    -> Expr f 'Number
    -> EffectSyntax f (f 'Unit)
fillText ctx = call3 ctx "fillText"
strokeText ctx = call3 ctx "strokeText"

-- | @ctx.measureText(text)@ — a 'TextMetrics' handle (read @width@).
measureText ::
  Effect f ('MutableObject Context2D)
  -> Expr f 'String
  -> EffectSyntax f (Effect f ('MutableObject TextMetrics))
measureText ctx t = hold $ callMethod ctx "measureText" (arg t <: RecNil)

-- | @ctx.translate(x, y)@.
translate ::
  Effect f ('MutableObject Context2D)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
translate ctx = call2 ctx "translate"

-- | @ctx.rotate(radians)@.
rotate ::
  Effect f ('MutableObject Context2D)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
rotate ctx a = ctxCall ctx "rotate" (arg a <: RecNil)

-- | @ctx.scale(sx, sy)@.
scale ::
  Effect f ('MutableObject Context2D)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
scale ctx = call2 ctx "scale"
