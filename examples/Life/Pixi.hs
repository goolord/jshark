{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | PixiJS 7.4.2 WebGL for Life ('examples/Life/js/pixi.min.js'). All Pixi /
-- GL calls live here via 'ffi', including the cell persist + SDF shader.
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
  , installLifeShader
  , presentGrid
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

-- | Shared @new PIXI.Application@ construction. Expects @view@, @width@,
--   @height@ and @backgroundColor@ in scope; binds @app@ and caches it on
--   the canvas for 'tickGlRecovery'.
newAppJs :: String
newAppJs =
  " const power = matchMedia('(prefers-reduced-motion: reduce)').matches"
    <> "   ? 'default' : 'high-performance';"
    <> " const app = new PIXI.Application({"
    <> "   view, width, height, backgroundColor,"
    <> "   antialias: false, autoStart: false, resolution: 1, hello: false,"
    <> "   powerPreference: power"
    <> " });"
    <> " view.__lifePixiApp = app;"

-- | Drop persist RTs while their creating renderer is still alive.
--   Expects @viewport@.
destroyPersistJs :: String
destroyPersistJs =
  " if (viewport.persistA && viewport.persistA.destroy) {"
    <> "   try { viewport.persistA.destroy(true); } catch (_) {}"
    <> " }"
    <> " if (viewport.persistB && viewport.persistB.destroy) {"
    <> "   try { viewport.persistB.destroy(true); } catch (_) {}"
    <> " }"
    <> " viewport.persistA = null;"
    <> " viewport.persistB = null;"
    <> " viewport.persistSrc = null;"
    <> " viewport.persistDst = null;"
    <> " viewport.persistMixer = null;"
    <> " viewport.persistMixFilter = null;"
    <> " viewport.sdfFilter = null;"
    <> " viewport.lifeShader = 0;"

-- | Phosphor mix (atlas space) + rounded-cell SDF (screen space).
--   Expects @app@, @viewport@, @sprite@, @currTex@, @texW@, @texH@, @bgHex@.
installShaderJs :: String
installShaderJs =
  " viewport.lifeShader = 0;"
    <> " var prevPrec;"
    <> " try {"
    <> " const reduced = matchMedia('(prefers-reduced-motion: reduce)').matches;"
    <> " const persistK = reduced ? 1 : 0.22;"
    <> " const bgR = ((bgHex >> 16) & 255) / 255;"
    <> " const bgG = ((bgHex >> 8) & 255) / 255;"
    <> " const bgB = (bgHex & 255) / 255;"
    <> destroyPersistJs
    <> " prevPrec = PIXI.settings.PRECISION_FRAGMENT;"
    <> " PIXI.settings.PRECISION_FRAGMENT = 'highp';"
    <> " const rtOpts = { width: texW, height: texH, resolution: 1,"
    <> "   scaleMode: PIXI.SCALE_MODES.NEAREST };"
    <> " const persistA = PIXI.RenderTexture.create(rtOpts);"
    <> " const persistB = PIXI.RenderTexture.create(rtOpts);"
    <> " persistA.clearColor = [bgR, bgG, bgB, 0];"
    <> " persistB.clearColor = [bgR, bgG, bgB, 0];"
    <> " if (app && app.renderer && app.renderer.renderTexture) {"
    <> "   const empty = new PIXI.Container();"
    <> "   app.renderer.render(empty, { renderTexture: persistA, clear: true });"
    <> "   app.renderer.render(empty, { renderTexture: persistB, clear: true });"
    <> " }"
    <> " const mixFrag ="
    <> " 'varying vec2 vTextureCoord;'"
    <> "+'uniform sampler2D uSampler;'"
    <> "+'uniform sampler2D uPersist;'"
    <> "+'uniform float uK;'"
    <> "+'void main(void){'"
    <> "+'vec4 curr=texture2D(uSampler,vTextureCoord);'"
    <> "+'vec4 prev=texture2D(uPersist,vTextureCoord);'"
    <> "+'gl_FragColor=mix(prev,curr,clamp(uK,0.0,1.0));'"
    <> "+'}';"
    <> " const sdfFrag ="
    <> " 'varying vec2 vTextureCoord;'"
    <> "+'uniform sampler2D uSampler;'"
    <> "+'uniform vec2 uTexSize;'"
    <> "+'uniform vec3 uBg;'"
    <> "+'uniform float uTime;'"
    <> "+'uniform float uMotion;'"
    <> "+'void main(void){'"
    <> "+'vec2 cell=vTextureCoord*uTexSize;'"
    <> "+'vec2 id=floor(cell);'"
    <> "+'vec2 f=fract(cell);'"
    <> "+'vec2 uv=(id+0.5)/uTexSize;'"
    <> "+'vec4 col=texture2D(uSampler,uv);'"
    <> "+'float live=clamp(col.a,0.0,1.0);'"
    <> "+'vec2 p=f-0.5;'"
    <> "+'float sq=pow(abs(p.x)*2.05,3.4)+pow(abs(p.y)*2.05,3.4);'"
    <> "+'float body=smoothstep(1.12,0.42,sq)*live;'"
    <> "+'float glow=exp(-dot(p,p)*6.4)*0.58*live;'"
    <> "+'float core=exp(-dot(p,p)*34.0)*0.26*live;'"
    <> "+'vec2 lp=p-vec2(-0.11,-0.13);'"
    <> "+'float spec=exp(-dot(lp,lp)*42.0)*0.22*live;'"
    <> "+'float phase=sin(uTime*1.65+id.x*0.37+id.y*0.53);'"
    <> "+'float pulse=1.0+uMotion*phase*0.04*live;'"
    <> "+'vec3 lit=col.rgb*pulse;'"
    <> "+'vec3 rgb=mix(uBg,lit,clamp(body+glow*0.92,0.0,1.0));'"
    <> "+'rgb+=lit*core+vec3(1.0,0.97,0.92)*spec;'"
    <> "+'gl_FragColor=vec4(rgb,1.0);'"
    <> "+'}';"
    <> " const mixFilter = new PIXI.Filter(undefined, mixFrag, {"
    <> "   uPersist: persistA, uK: persistK"
    <> " });"
    <> " mixFilter.padding = 0;"
    <> " const mixer = new PIXI.Sprite(currTex);"
    <> " mixer.width = texW; mixer.height = texH;"
    <> " mixer.filters = [mixFilter];"
    <> " mixer.filterArea = new PIXI.Rectangle(0, 0, texW, texH);"
    <> " const sdfFilter = new PIXI.Filter(undefined, sdfFrag, {"
    <> "   uTexSize: [texW, texH], uBg: [bgR, bgG, bgB],"
    <> "   uTime: 0, uMotion: reduced ? 0 : 1"
    <> " });"
    <> " sdfFilter.padding = 0;"
    <> " sdfFilter.resolution = 1;"
    <> " PIXI.settings.PRECISION_FRAGMENT = prevPrec;"
    <> " sprite.filters = [sdfFilter];"
    <> " viewport.persistA = persistA;"
    <> " viewport.persistB = persistB;"
    <> " viewport.persistSrc = persistA;"
    <> " viewport.persistDst = persistB;"
    <> " viewport.persistMixer = mixer;"
    <> " viewport.persistMixFilter = mixFilter;"
    <> " viewport.sdfFilter = sdfFilter;"
    <> " viewport.persistK = persistK;"
    <> " viewport.persistWarm = persistK >= 1 ? 1 : 24;"
    <> " viewport.lifeShader = 1;"
    <> " } catch (err) {"
    <> "   if (prevPrec !== undefined) PIXI.settings.PRECISION_FRAGMENT = prevPrec;"
    <> "   viewport.lifeShader = 0;"
    <> "   console.error('[Life] cell shader failed', err);"
    <> " }"

-- | Hot path: optional persist ping-pong, then stage present.
--   Expects @app@, @viewport@, @currTex@, @now@, @upload@, @stageDirty@.
presentGridJs :: String
presentGridJs =
  " if (!app || !app.renderer) return;"
    <> " if (app.renderer.gl && app.renderer.gl.isContextLost"
    <> "   && app.renderer.gl.isContextLost()) return;"
    <> " if (upload && currTex && currTex.baseTexture) currTex.baseTexture.update();"
    <> " const pMixer = viewport.persistMixer;"
    <> " const pMixF = viewport.persistMixFilter;"
    <> " const pSdf = viewport.sdfFilter;"
    <> " const pSpr = viewport.sprite;"
    <> " if (!viewport.lifeShader || !pMixer || !pMixF || !pSdf || !pSpr) {"
    <> "   if (upload || stageDirty) app.renderer.render(app.stage);"
    <> "   return;"
    <> " }"
    <> " if (upload) viewport.persistWarm = viewport.persistK >= 1 ? 1 : 24;"
    <> " const doPersist = upload || viewport.persistWarm > 0;"
    <> " const motion = pSdf.uniforms.uMotion;"
    <> " if (doPersist) {"
    <> "   try {"
    <> "     pMixF.uniforms.uPersist = viewport.persistSrc;"
    <> "     pMixF.uniforms.uK = viewport.persistK;"
    <> "     pMixer.texture = currTex;"
    <> "     app.renderer.render(pMixer, {"
    <> "       renderTexture: viewport.persistDst, clear: true"
    <> "     });"
    <> "     const pTmp = viewport.persistSrc;"
    <> "     viewport.persistSrc = viewport.persistDst;"
    <> "     viewport.persistDst = pTmp;"
    <> "     pSpr.texture = viewport.persistSrc;"
    <> "     if (viewport.persistWarm > 0) viewport.persistWarm--;"
    <> "   } catch (err) {"
    <> "     console.error('[Life] cell shader present failed', err);"
    <> "   }"
    <> " }"
    <> " if (motion) pSdf.uniforms.uTime = now * 0.001;"
    <> " if (doPersist || motion || stageDirty) app.renderer.render(app.stage);"

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
                <> newAppJs
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
      ( "(view, viewport, state, texW, texH, width, height, backgroundColor, cellPx) => {"
            <> " const rebuild = () => {"
            <> destroyPersistJs
            <> "   const oldApp = view.__lifePixiApp;"
            <> "   if (oldApp?.destroy) {"
            <> "     try { oldApp.destroy(true, {children:true, texture:true, baseTexture:true}); }"
            <> "     catch (_) {}"
            <> "   }"
          <> "   view.__lifePixiApp = null;"
          <> newAppJs
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
          <> "   const zoom = viewport.zoom || 1;"
          <> "   sprite.scale.set(zoom * cellPx, zoom * cellPx);"
          <> "   sprite.position.set(viewport.panX || 0, viewport.panY || 0);"
          <> "   const currTex = tex;"
          <> "   const bgHex = backgroundColor;"
          <> installShaderJs
            <> "   const now = 0, upload = true, stageDirty = true;"
            <> presentGridJs
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

-- | Nearest-neighbour sampling so the cell shader can fetch one texel
--   per cell without bleeding into neighbours.
setTextureNearest ::
  Effect f ('MutableObject Texture) -> EffectSyntax f (f 'Unit)
setTextureNearest tex = do
  toSyntax_
    $ discard
    $ ffi
      "((t) => { t.baseTexture.scaleMode = PIXI.SCALE_MODES.NEAREST; })"
      (ArgEffect tex <: RecNil)
  done

-- | Attach the Life cell shader: atlas-space phosphor persist plus a
--   screen-space rounded-cell SDF. Stored on @viewport@ so rebuild and
--   'presentGrid' share one setup.
installLifeShader ::
  Effect f ('MutableObject Application)
  -> Effect f ('MutableObject ())
  -> Expr f ('MutableObject Sprite)
  -> Expr f ('MutableObject Texture)
  -> EffectSyntax f (f 'Unit)
installLifeShader app viewport sprite tex = do
  toSyntax_
    $ discard
    $ ffi
      ( "(app, viewport, sprite, currTex, texW, texH, bgHex) => {"
          <> installShaderJs
          <> " }"
      )
      ( ArgEffect app
          <: ArgEffect viewport
          <: arg sprite
          <: arg tex
          <: arg (number texW)
          <: arg (number texH)
          <: arg (number canvasBgPixi)
          <: RecNil
      )
  done

-- | Upload the atlas when it changed, ease persist toward it, then draw
--   the stage through the cell SDF. Persist runs on paint plus a short
--   settle; the stage presents for persist, motion, or a dirty viewport.
presentGrid ::
  Expr f ('MutableObject Application)
  -> Effect f ('MutableObject ())
  -> Effect f ('MutableObject Texture)
  -> Expr f 'Number
  -> Expr f 'Bool
  -> Expr f 'Bool
  -> EffectSyntax f (f 'Unit)
presentGrid app viewport tex now upload stageDirty = do
  toSyntax_
    $ discard
    $ ffi
      ( "(app, viewport, currTex, now, upload, stageDirty) => {"
          <> presentGridJs
          <> " }"
      )
      ( arg app
          <: ArgEffect viewport
          <: ArgEffect tex
          <: arg now
          <: arg upload
          <: arg stageDirty
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
  -> Effect f ('MutableObject ())
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
drawEraserGhost app viewport alive w h gx gy radius panX panY zoom px = do
  toSyntax_
    $ discard
    $ ffi
      ( "((app,viewport,alive,w,h,gx,gy,r,panX,panY,zoom,cellPx) => {"
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
          <> " viewport.renderPanValid = false;"
          <> " })"
      )
      ( arg app
          <: ArgEffect viewport
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

-- | Hide the eraser preview; invalidates the viewport so the next frame's
--   render drops it from the scene.
clearEraserGhost ::
  Expr f ('MutableObject Application)
  -> Effect f ('MutableObject ())
  -> EffectSyntax f (f 'Unit)
clearEraserGhost app viewport = do
  toSyntax_
    $ discard
    $ ffi
      ( "((app, viewport) => {"
          <> " if (!app || !app.__eraserGhostOn) return;"
          <> " const gfx = app.__eraserGhostGfx;"
          <> " if (gfx && !gfx.destroyed) { gfx.clear(); gfx.visible = false; }"
          <> " app.__eraserGhostOn = false;"
          <> " viewport.renderPanValid = false;"
          <> " })"
      )
      (arg app <: ArgEffect viewport <: RecNil)
  done
