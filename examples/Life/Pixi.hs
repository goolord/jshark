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
  , wireContextRecovery
  , tickGlRecovery
  , textureFromBuffer
  , setTextureNearest
  , uploadTextureFull
  , uploadTextureRegion
  , uploadAndRender
  , newSprite
  , mountSprite
  , setSpriteViewport
  , render
  , drawEraserGhost
  , clearEraserGhost
  )
where

import JShark.Api
import JShark.Dom (DomElement)
import JShark.Generic (MutableObjectOf)
import JShark.Rec (Rec (..), (<:))
import Types (LifeState, canvasBgPixi, canvasH, canvasW, cellPx, texH, texW)

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
            ( "(view, width, height, backgroundColor) => {"
                <> " if (view.__lifePixiApp) {"
                <> "   const gl = view.__lifePixiApp.renderer?.gl;"
                <> "   if (!gl?.isContextLost?.()) return view.__lifePixiApp;"
                <> "   try { view.__lifePixiApp.destroy(true); } catch (_) {}"
                <> "   view.__lifePixiApp = null;"
                <> " }"
                <> " const power = matchMedia('(prefers-reduced-motion: reduce)').matches"
                <> "   ? 'default' : 'high-performance';"
                <> " const app = new PIXI.Application({"
                <> " view, width, height, backgroundColor,"
                <> " antialias: false, autoStart: false, resolution: 1, hello: false,"
                <> " powerPreference: power, preserveDrawingBuffer: true"
                <> " });"
                <> " view.__lifePixiApp = app;"
                <> " return app;"
                <> " }"
            )
            (ArgEffect canvas <: arg w <: arg h <: arg bg <: RecNil)
        )
    )

-- | Pause on @webglcontextlost@; recreate Pixi on restore (or poll recovery).
wireContextRecovery ::
  Effect f ('MutableObject DomElement)
  -> Effect f ('MutableObject ())
  -> Effect f (MutableObjectOf LifeState)
  -> EffectSyntax f (f 'Unit)
wireContextRecovery canvas viewport state = do
  toSyntax_
    $ discard
    $ ffi
        ( "(view, viewport, state, texW, texH, canvasW, canvasH, bg, cellPx) => {"
            <> " const rebuild = () => {"
            <> "   const oldApp = view.__lifePixiApp;"
            <> "   if (oldApp?.destroy) {"
            <> "     try { oldApp.destroy(true, {children:true, texture:true, baseTexture:true}); }"
            <> "     catch (_) {}"
            <> "   }"
            <> "   view.__lifePixiApp = null;"
            <> "   const power = matchMedia('(prefers-reduced-motion: reduce)').matches"
            <> "     ? 'default' : 'high-performance';"
            <> "   const app = new PIXI.Application({"
            <> "     view, width: canvasW, height: canvasH, backgroundColor: bg,"
            <> "     antialias: false, autoStart: false, resolution: 1, hello: false,"
            <> "     powerPreference: power, preserveDrawingBuffer: true"
            <> "   });"
            <> "   view.__lifePixiApp = app;"
            <> "   viewport.app = app;"
            <> "   const buf = state.rgbaPixels;"
            <> "   if (!buf) throw new Error('missing rgba buffer');"
            <> "   const tex = PIXI.Texture.fromBuffer(buf, texW, texH, {"
            <> "     format: PIXI.FORMATS.RGBA,"
            <> "     type: PIXI.TYPES.UNSIGNED_BYTE"
            <> "   });"
            <> "   tex.baseTexture.scaleMode = PIXI.SCALE_MODES.NEAREST;"
            <> "   const sprite = new PIXI.Sprite(tex);"
            <> "   app.stage.addChild(sprite);"
            <> "   viewport.texture = tex;"
            <> "   viewport.sprite = sprite;"
            <> "   const panX = viewport.panX || 0;"
            <> "   const panY = viewport.panY || 0;"
            <> "   const zoom = viewport.zoom || 1;"
            <> "   sprite.scale.set(zoom * cellPx, zoom * cellPx);"
            <> "   sprite.position.set(panX, panY);"
            <> "   if (tex.baseTexture.resource) {"
            <> "     tex.baseTexture.resource.data = buf;"
            <> "     if (tex.baseTexture.resource.source) tex.baseTexture.resource.source = buf;"
            <> "   }"
            <> "   const bt = tex.baseTexture;"
            <> "   const gl = app.renderer.gl;"
            <> "   try {"
            <> "     if (bt.resource?.update) bt.resource.update();"
            <> "     const bound = app.renderer.texture.bind(bt, 0);"
            <> "     if (bound?.texture) {"
            <> "       gl.bindTexture(gl.TEXTURE_2D, bound.texture);"
            <> "       gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1);"
            <> "       gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, texW | 0, texH | 0,"
            <> "         0, gl.RGBA, gl.UNSIGNED_BYTE, buf);"
            <> "     } else bt.update();"
            <> "   } catch (_) { bt.update(); }"
            <> "   app.render();"
            <> "   viewport.renderPanValid = false;"
            <> " };"
            <> " viewport._lifeRebuild = rebuild;"
            <> " const onRestored = () => {"
            <> "   viewport.glLost = 0;"
            <> "   console.info('[Life] WebGL context restored');"
            <> "   try { rebuild(); state.sceneDirty = true; }"
            <> "   catch (err) { console.error('[Life] WebGL restore failed', err); }"
            <> " };"
            <> " const glView = view.__lifePixiApp?.renderer?.view || view;"
            <> " viewport.glLost = 0;"
            <> " glView.addEventListener('webglcontextlost', (e) => {"
            <> "   e.preventDefault();"
            <> "   viewport.glLost = 1;"
            <> "   console.warn('[Life] WebGL context lost — 2D fallback active');"
            <> " }, false);"
            <> " glView.addEventListener('webglcontextrestored', onRestored, false);"
            <> " }"
        )
        ( ArgEffect canvas
            <: ArgEffect viewport
            <: ArgEffect state
            <: arg (number texW)
            <: arg (number texH)
            <: arg (number canvasW)
            <: arg (number canvasH)
            <: arg (number canvasBgPixi)
            <: arg (number (fromIntegral cellPx))
            <: RecNil
        )
  done

-- | Per-frame GL health check. Detects contexts lost before our listener
--   attached (or without an event), and rebuilds Pixi once GL is usable again.
--   While @glLost@ is set, 'Engine.renderLife' draws via the 2D fallback.
tickGlRecovery ::
  Effect f ('MutableObject DomElement)
  -> Effect f ('MutableObject ())
  -> Effect f (MutableObjectOf LifeState)
  -> EffectSyntax f (f 'Unit)
tickGlRecovery canvas viewport state = do
  toSyntax_
    $ discard
    $ ffi
        ( "(view, viewport, state) => {"
            <> " const gl = view.__lifePixiApp?.renderer?.gl;"
            <> " const lost = !gl || (gl.isContextLost && gl.isContextLost());"
            <> " if (!viewport.glLost) {"
            <> "   if (lost) {"
            <> "     viewport.glLost = 1;"
            <> "     console.warn('[Life] WebGL unavailable — 2D fallback active');"
            <> "   }"
            <> "   return;"
            <> " }"
            <> " if (lost) return;"
            <> " if (typeof viewport._lifeRebuild !== 'function') return;"
            <> " if (viewport._glCooldown > 0) { viewport._glCooldown--; return; }"
            <> " viewport.glLost = 0;"
            <> " console.info('[Life] WebGL recovered — rebuilding GPU renderer');"
            <> " try {"
            <> "   viewport._lifeRebuild();"
            <> "   state.sceneDirty = true;"
            <> " } catch (err) {"
            <> "   console.error('[Life] WebGL rebuild failed', err);"
            <> "   viewport.glLost = 1;"
            <> "   viewport._glCooldown = 180;"
            <> " }"
            <> " }"
        )
        (ArgEffect canvas <: ArgEffect viewport <: ArgEffect state <: RecNil)
  done

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
            ( "(buf, width, height) => {"
                <> " const tex = PIXI.Texture.fromBuffer(buf, width, height, {"
                <> " format: PIXI.FORMATS.RGBA,"
                <> " type: PIXI.TYPES.UNSIGNED_BYTE"
                <> " });"
                <> " const res = tex.baseTexture.resource;"
                <> " if (res) { res.data = buf; if (res.source) res.source = buf; }"
                <> " return tex;"
                <> " }"
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

-- | Push @buf@ to GPU via @texImage2D@ (Pixi @update@ alone misses on some GPUs).
syncTextureBuffer ::
  Expr f ('MutableObject Application)
  -> Effect f ('MutableObject Texture)
  -> Expr f 'Uint8Array
  -> EffectSyntax f (f 'Unit)
syncTextureBuffer app tex buf = do
  toSyntax_
    $ discard
    $ ffi
        ( "(app, t, buf) => {"
            <> " const live = app?.stage?.children?.[0]?.texture ?? t;"
            <> " const bt = live?.baseTexture;"
            <> " const renderer = app?.renderer;"
            <> " if (!bt || !renderer) return;"
            <> " const gl = renderer.gl;"
            <> " if (gl?.isContextLost?.()) return;"
            <> " const res = bt.resource;"
            <> " if (res && buf) {"
            <> "   if (res.data !== buf) res.data = buf;"
            <> "   if (res.source && res.source !== buf) res.source = buf;"
            <> " }"
            <> " const data = res?.data ?? buf;"
            <> " if (!data) { bt.update(); return; }"
            <> " try {"
            <> "   if (res?.update) res.update();"
            <> "   const bound = renderer.texture.bind(bt, 0);"
            <> "   if (bound?.texture) {"
            <> "     gl.bindTexture(gl.TEXTURE_2D, bound.texture);"
            <> "     gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1);"
            <> "     gl.texImage2D("
            <> "       gl.TEXTURE_2D, 0, gl.RGBA, bt.width | 0, bt.height | 0,"
            <> "       0, gl.RGBA, gl.UNSIGNED_BYTE, data"
            <> "     );"
            <> "   } else bt.update();"
            <> " } catch (_) { bt.update(); }"
            <> " }"
        )
        (arg app <: ArgEffect tex <: arg buf <: RecNil)
  done

-- | Sync CPU @rgbaPixels@ buffer to the GPU (@resource.update@ + @baseTexture.update@).
uploadTextureFull ::
  Expr f ('MutableObject Application)
  -> Effect f ('MutableObject Texture)
  -> Expr f 'Uint8Array
  -> EffectSyntax f (f 'Unit)
uploadTextureFull app tex buf =
  uploadTextureRegion app tex buf (number 0) (number 0) (number 0) (number 0) (number 0)

-- | Full atlas upload (region args ignored).
uploadTextureRegion ::
  Expr f ('MutableObject Application)
  -> Effect f ('MutableObject Texture)
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
uploadTextureRegion app tex buf _x0 _y0 _x1 _y1 _gridTexW =
  syncTextureBuffer app tex buf

-- | Upload painted @buf@ and draw the stage (same turn — avoids stale framebuffer).
uploadAndRender ::
  Expr f ('MutableObject Application)
  -> Effect f ('MutableObject Texture)
  -> Expr f 'Uint8Array
  -> EffectSyntax f (f 'Unit)
uploadAndRender app tex buf = do
  toSyntax_
    $ discard
    $ ffi
        ( "(app, t, buf) => {"
            <> " const live = app?.stage?.children?.[0]?.texture ?? t;"
            <> " const bt = live?.baseTexture;"
            <> " const renderer = app?.renderer;"
            <> " if (!bt || !renderer) return;"
            <> " const gl = renderer.gl;"
            <> " if (gl?.isContextLost?.()) return;"
            <> " const res = bt.resource;"
            <> " if (res && buf) {"
            <> "   if (res.data !== buf) res.data = buf;"
            <> "   if (res.source && res.source !== buf) res.source = buf;"
            <> " }"
            <> " const data = res?.data ?? buf;"
            <> " if (data) {"
            <> "   try {"
            <> "     if (res?.update) res.update();"
            <> "     const bound = renderer.texture.bind(bt, 0);"
            <> "     if (bound?.texture) {"
            <> "       gl.bindTexture(gl.TEXTURE_2D, bound.texture);"
            <> "       gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1);"
            <> "       gl.texImage2D("
            <> "         gl.TEXTURE_2D, 0, gl.RGBA, bt.width | 0, bt.height | 0,"
            <> "         0, gl.RGBA, gl.UNSIGNED_BYTE, data"
            <> "       );"
            <> "     } else bt.update();"
            <> "   } catch (_) { bt.update(); }"
            <> " }"
            <> " renderer.render(app.stage);"
            <> " }"
        )
        (arg app <: ArgEffect tex <: arg buf <: RecNil)
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
setSpriteViewport sprite panX panY zoom px = do
  toSyntax_
    $ discard
    $ ffi
      "((s, px, py, z, cp) => { s.scale.set(z * cp, z * cp); s.position.set(px, py); })"
      (ArgEffect sprite <: arg panX <: arg panY <: arg zoom <: arg px <: RecNil)
  done

-- | @app.render()@.
render ::
  Expr f ('MutableObject Application) -> EffectSyntax f (f 'Unit)
render app = do
  toSyntax_
    $ discard
    $ ffi
        ( "((app) => {"
            <> " const gl = app?.renderer?.gl;"
            <> " if (gl?.isContextLost?.()) return;"
            <> " app.render();"
            <> " })"
        )
        (arg app <: RecNil)
  done

-- | Eraser brush preview drawn as a @PIXI.Graphics@ child of the stage.
--
-- The preview must live INSIDE the WebGL scene: a separate transparent
-- canvas stacked over the board makes Chromium's compositor occlusion-cull
-- the WebGL quad beneath it, blanking the whole board (seen in headless and
-- embedded/software-composited browsers).
drawEraserGhost ::
  Expr f ('MutableObject Application)
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
drawEraserGhost app alive w h gx gy radius panX panY zoom px = do
  toSyntax_
    $ discard
    $ ffi
        ( "((app,alive,w,h,gx,gy,r,panX,panY,zoom,cellPx) => {"
            <> " if (!app?.stage) return;"
            <> " const gl = app.renderer?.gl;"
            <> " if (gl?.isContextLost?.()) return;"
            <> " let gfx = app.__eraserGhostGfx;"
            <> " if (!gfx || gfx.destroyed) {"
            <> "   gfx = new PIXI.Graphics();"
            <> "   app.__eraserGhostGfx = gfx;"
            <> " }"
            <> " if (gfx.parent !== app.stage) app.stage.addChild(gfx);"
            <> " gfx.visible = true;"
            <> " gfx.clear();"
            <> " const ri = Math.max(0, Math.floor(r)) | 0;"
            <> " const scale = cellPx * zoom, rr = ri * ri;"
            <> " gfx.beginFill(0xf87171, 0.5);"
            <> " for (let dy = -ri; dy <= ri; dy++) {"
            <> "   for (let dx = -ri; dx <= ri; dx++) {"
            <> "     if (dx * dx + dy * dy > rr) continue;"
            <> "     const x = (gx + dx) | 0, y = (gy + dy) | 0;"
            <> "     if (x < 0 || y < 0 || x >= w || y >= h) continue;"
            <> "     if (alive[y * w + x] & 1)"
            <> "       gfx.drawRect(panX + x * scale, panY + y * scale, scale, scale);"
            <> "   }"
            <> " }"
            <> " gfx.endFill();"
            <> " gfx.lineStyle(Math.max(1, scale * 0.15), 0xf87171, 0.85);"
            <> " gfx.drawCircle("
            <> "   panX + (gx + 0.5) * scale, panY + (gy + 0.5) * scale,"
            <> "   (ri + 0.5) * scale);"
            <> " app.__eraserGhostOn = true;"
            <> " app.render();"
            <> " })"
        )
        ( arg app
            <: arg alive
            <: arg w
            <: arg h
            <: arg gx
            <: arg gy
            <: arg radius
            <: arg panX
            <: arg panY
            <: arg zoom
            <: arg px
            <: RecNil
        )
  done

-- | Hide the eraser preview; re-renders once so it disappears immediately.
clearEraserGhost ::
  Expr f ('MutableObject Application) -> EffectSyntax f (f 'Unit)
clearEraserGhost app = do
  toSyntax_
    $ discard
    $ ffi
        ( "((app) => {"
            <> " if (!app || !app.__eraserGhostOn) return;"
            <> " const gfx = app.__eraserGhostGfx;"
            <> " if (gfx && !gfx.destroyed) { gfx.clear(); gfx.visible = false; }"
            <> " app.__eraserGhostOn = false;"
            <> " const gl = app.renderer?.gl;"
            <> " if (gl?.isContextLost?.()) return;"
            <> " app.render();"
            <> " })"
        )
        (arg app <: RecNil)
  done
