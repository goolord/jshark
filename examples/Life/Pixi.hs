{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | PixiJS 7.4.2 WebGL for Life ('examples/Life/js/pixi.min.js'). All Pixi /
-- GL calls live here via 'ffi'.
module Pixi
  ( Application
  , Texture
  , Sprite
  , pixiAvailable
  , newApplication
  , textureFromBuffer
  , setTextureNearest
  , syncTexture
  , newSprite
  , mountSprite
  , setSpriteViewport
  , render
  )
where

import JShark.Api
import JShark.Dom (DomElement)
import JShark.Rec (Rec (..), (<:))

-- | @PIXI.Application@.
data Application

-- | @PIXI.Texture@ backed by an RGBA byte buffer.
data Texture

-- | @PIXI.Sprite@ — world bitmap positioned on the stage.
data Sprite

-- | @typeof PIXI !== 'undefined'@ — false when the script failed to load.
pixiAvailable :: EffectSyntax f (Expr f 'Bool)
pixiAvailable =
  fmap var (toSyntax (ffi "(() => typeof PIXI !== 'undefined')" RecNil))

-- | @new PIXI.Application({ view, width, height, … })@.
--
-- @autoStart@ is off so the game loop calls 'render' after each sim step.
newApplication ::
  Effect f ('MutableObject DomElement)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f ('MutableObject Application))
newApplication canvas w h bg =
  fmap
    var
    ( toSyntax
        ( ffi
            ( "(view, width, height, backgroundColor) =>"
                <> " new PIXI.Application({"
                <> " view, width, height, backgroundColor,"
                <> " antialias: false, autoStart: false, resolution: 1"
                <> " })"
            )
            (ArgEffect canvas <: arg w <: arg h <: arg bg <: RecNil)
        )
    )

-- | @PIXI.Texture.fromBuffer@ for the full-world RGBA atlas.
textureFromBuffer ::
  Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f ('MutableObject Texture))
textureFromBuffer buf w h =
  fmap
    var
    ( toSyntax
        ( ffi
            ( "(buf, width, height) =>"
                <> " PIXI.Texture.fromBuffer(buf, width, height, {"
                <> " format: PIXI.FORMATS.RGBA,"
                <> " type: PIXI.TYPES.UNSIGNED_BYTE"
                <> " })"
            )
            (arg buf <: arg w <: arg h <: RecNil)
        )
    )

-- | Nearest-neighbour sampling so zoom stays crisp.
setTextureNearest ::
  Effect f ('MutableObject Texture) -> EffectSyntax f (f 'Unit)
setTextureNearest tex = do
  toSyntax_
    $ discard
    $ ffi
      "((t) => { t.baseTexture.scaleMode = PIXI.SCALE_MODES.NEAREST; })"
      (ArgEffect tex <: RecNil)
  done

-- | Push CPU buffer edits to the GPU.
--
-- Full: @baseTexture.update()@. Partial: bind via @renderer.texture@ then one
-- @texSubImage2D@ for the dirty rect (same buffer as 'textureFromBuffer').
syncTexture ::
  Effect f ('MutableObject Application)
  -> Effect f ('MutableObject Texture)
  -> Expr f 'Bool
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
syncTexture app tex full x0 y0 x1 y1 fullW = do
  toSyntax_
    $ discard
    $ ffi
        ( "(app, tex, full, x0, y0, x1, y1, fullW) => {"
            <> " const bt = tex.baseTexture;"
            <> " if (full) { bt.update(); return; }"
            <> " const x = Math.floor(x0)|0, y = Math.floor(y0)|0;"
            <> " const w = Math.ceil(x1) - x, h = Math.ceil(y1) - y;"
            <> " if (w <= 0 || h <= 0) return;"
            <> " const res = bt.resource;"
            <> " const buf = res && (res.source || res.data);"
            <> " if (!buf) { bt.update(); return; }"
            <> " const bound = app.renderer.texture.bind(bt, 0);"
            <> " if (!bound || !bound.texture) { bt.update(); return; }"
            <> " const gl = app.renderer.gl;"
            <> " gl.bindTexture(gl.TEXTURE_2D, bound.texture);"
            <> " const off = (y * fullW + x) * 4;"
            <> " gl.texSubImage2D(gl.TEXTURE_2D, 0, x, y, w, h, gl.RGBA, gl.UNSIGNED_BYTE,"
            <> "   buf.subarray(off, off + w * h * 4));"
            <> " }"
        )
        ( ArgEffect app
            <: ArgEffect tex
            <: arg full
            <: arg x0
            <: arg y0
            <: arg x1
            <: arg y1
            <: arg fullW
            <: RecNil
        )
  done

-- | @new PIXI.Sprite(texture)@.
newSprite ::
  Expr f ('MutableObject Texture)
  -> EffectSyntax f (Expr f ('MutableObject Sprite))
newSprite tex =
  fmap
    var
    ( toSyntax
        ( ffi
            "((texture) => new PIXI.Sprite(texture))"
            (arg tex <: RecNil)
        )
    )

-- | @app.stage.addChild(sprite)@.
mountSprite ::
  Effect f ('MutableObject Application)
  -> Expr f ('MutableObject Sprite)
  -> EffectSyntax f (f 'Unit)
mountSprite app sprite = do
  toSyntax_
    $ discard
    $ ffi
      "((app, s) => { app.stage.addChild(s); })"
      (ArgEffect app <: arg sprite <: RecNil)
  done

-- | Pan/zoom via GPU sprite transform (no CPU repaints on viewport moves).
setSpriteViewport ::
  Effect f ('MutableObject Sprite)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
setSpriteViewport sprite panX panY zoom = do
  toSyntax_
    $ discard
    $ ffi
      "((s, px, py, z) => { s.scale.set(z, z); s.position.set(px, py); })"
      (ArgEffect sprite <: arg panX <: arg panY <: arg zoom <: RecNil)
  done

-- | @app.render()@.
render ::
  Effect f ('MutableObject Application) -> EffectSyntax f (f 'Unit)
render app = do
  toSyntax_
    $ discard
    $ callMethod app "render" RecNil
  done
