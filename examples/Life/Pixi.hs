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
  , uploadTextureFull
  , uploadTextureRegion
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
                <> " antialias: false, autoStart: false, resolution: 1,"
                <> " powerPreference:"
                <> " (matchMedia('(prefers-reduced-motion: reduce)').matches"
                <> " ? 'default' : 'high-performance'),"
                <> " hello: false"
                <> " })"
            )
            (ArgEffect canvas <: arg w <: arg h <: arg bg <: RecNil)
        )
    )

-- | @PIXI.Texture.fromBuffer@ — one RGBA texel per grid cell; the sprite
-- scales it on the GPU (@cellPx@ × zoom).
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

-- | Upload the full grid atlas (@baseTexture.update()@).
uploadTextureFull ::
  Effect f ('MutableObject Texture) -> EffectSyntax f (f 'Unit)
uploadTextureFull tex = do
  toSyntax_
    $ discard
    $ ffi
      "((t) => { t.baseTexture.update(); })"
      (ArgEffect tex <: RecNil)
  done

-- | Partial upload for a dirty rect (grid texel coordinates).
uploadTextureRegion ::
  Effect f ('MutableObject Application)
  -> Effect f ('MutableObject Texture)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
uploadTextureRegion app tex x0 y0 x1 y1 texW = do
  toSyntax_
    $ discard
    $ ffi
        ( "(app, tex, x0, y0, x1, y1, texW) => {"
            <> " const bt = tex.baseTexture;"
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
            <> " const off = (y * texW + x) * 4;"
            <> " gl.texSubImage2D(gl.TEXTURE_2D, 0, x, y, w, h, gl.RGBA, gl.UNSIGNED_BYTE,"
            <> "   buf.subarray(off, off + w * h * 4));"
            <> " }"
        )
        ( ArgEffect app
            <: ArgEffect tex
            <: arg x0
            <: arg y0
            <: arg x1
            <: arg y1
            <: arg texW
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

-- | Pan/zoom on the GPU: sprite scale = @cellPx × zoom@.
setSpriteViewport ::
  Effect f ('MutableObject Sprite)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
setSpriteViewport sprite panX panY zoom cellPx = do
  toSyntax_
    $ discard
    $ ffi
      "((s, px, py, z, cp) => { s.scale.set(z * cp, z * cp); s.position.set(px, py); })"
      (ArgEffect sprite <: arg panX <: arg panY <: arg zoom <: arg cellPx <: RecNil)
  done

-- | @app.render()@.
render ::
  Effect f ('MutableObject Application) -> EffectSyntax f (f 'Unit)
render app = do
  toSyntax_
    $ discard
    $ callMethod app "render" RecNil
  done
