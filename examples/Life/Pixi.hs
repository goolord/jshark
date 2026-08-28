{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | PixiJS 7.4.2 WebGL for Life ('examples/Life/js/pixi.min.js'). All Pixi /
-- GL calls live here via 'ffi', including two-frame onion skin + glow SDF.
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
  , prefetchLifeShader
  , lifeCellShaderUrl
  , replaceGridTexture
  , presentGrid
  , newSprite
  , mountSprite
  , setSpriteViewport
  , render
  , drawEraserGhost
  , clearEraserGhost
  , drawGliderGhost
  )
where

import qualified Data.Text as T
import JShark.Api
import JShark.Api.Generic (MutableObjectOf)
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Dom (DomElement)
import JShark.Promise (Promise)
import Types (LifeState, canvasBgPixi, canvasH, canvasW, cellPx, texH, texW)

-- | Fragment shader served beside @js/@ in the Life frame.
lifeCellShaderUrl :: T.Text
lifeCellShaderUrl = "js/shaders/cell.frag.glsl"

-- | Fetch @cell.frag.glsl@ once; cached on @viewport.cellFragSrc@.
prefetchLifeShaderJs :: String
prefetchLifeShaderJs =
  "(viewport, url) => (async () => {"
    <> " if (viewport.cellFragSrc) return;"
    <> " const r = await fetch(url);"
    <> " if (!r.ok) throw new Error('Life shader fetch failed: ' + url);"
    <> " viewport.cellFragSrc = await r.text();"
    <> " })()"

-- | @PIXI.Application@.
data Application

-- | @PIXI.Texture@ backed by an RGBA byte buffer.
data Texture

-- | @PIXI.Sprite@: world bitmap positioned on the stage.
data Sprite

-- | @typeof PIXI !== 'undefined'@: false when the script failed to load.
pixiAvailable :: EffectSyntax f (Expr f 'Bool)
pixiAvailable =
  fmap var (toSyntax (ffi "(() => typeof PIXI !== 'undefined')" RecNil))

-- | Shared @new PIXI.Application@ construction. Expects @view@, @width@,
--   @height@ and @backgroundColor@ in scope; binds @app@ and caches it on
--   the canvas for 'tickGlRecovery'. @high-performance@ makes Chromium on
--   Linux switch GPU process after the first context is created; Pixi then
--   logs "WebGL context was lost" and we fall back to the 2D atlas blit.
--   @default@ keeps the context on whatever GPU created it.
newAppJs :: String
newAppJs =
  " const app = new PIXI.Application({"
    <> "   view, width, height, backgroundColor,"
    <> "   antialias: false, autoStart: false, resolution: 1, hello: false,"
    <> "   powerPreference: 'default'"
    <> " });"
    <> " view.__lifePixiApp = app;"
    <> " const gl = app.renderer && app.renderer.gl;"
    <> " let renderer = 'none';"
    <> " if (gl) {"
    <> "   const ext = gl.getExtension('WEBGL_debug_renderer_info');"
    <> "   renderer = ext"
    <> "     ? String(gl.getParameter(ext.UNMASKED_RENDERER_WEBGL))"
    <> "     : 'webgl';"
    <> "   if (gl.isContextLost && gl.isContextLost()) renderer += ' (lost)';"
    <> " }"
    <> " view.__lifeGlRenderer = renderer;"
    <> " console.info('[Life] GL', renderer);"
    <> " if (!view.__lifeGlArmed) {"
    <> "   view.__lifeGlArmed = 1;"
    <> "   view.addEventListener('webglcontextlost', (e) => {"
    <> "     e.preventDefault();"
    <> "   }, false);"
    <> " }"

-- | Drop onion-skin atlas RT while its creating renderer is still alive.
--   Expects @viewport@.
destroyPersistJs :: String
destroyPersistJs =
  " if (viewport.atlasPrev && viewport.atlasPrev.destroy) {"
    <> "   try { viewport.atlasPrev.destroy(true); } catch (_) {}"
    <> " }"
    <> " if (viewport.atlasCopySpr && viewport.atlasCopySpr.destroy) {"
    <> "   try { viewport.atlasCopySpr.destroy(true); } catch (_) {}"
    <> " }"
    <> " viewport.atlasPrev = null;"
    <> " viewport.atlasCopySpr = null;"
    <> " viewport.sdfFilter = null;"
    <> " viewport.lifeShader = 0;"

-- | Build Pixi filter from @viewport.cellFragSrc@ (see 'prefetchLifeShader').
--   Expects @app@, @viewport@, @sprite@, @currTex@, @texW@, @texH@, @bgHex@.
installShaderJs :: String
installShaderJs =
  " viewport.lifeShader = 0;"
    <> " var prevPrec;"
    <> " try {"
    <> " const sdfFrag = viewport.cellFragSrc;"
    <> " if (!sdfFrag) throw new Error('Life cell shader not loaded');"
    <> " const bgR = ((bgHex >> 16) & 255) / 255;"
    <> " const bgG = ((bgHex >> 8) & 255) / 255;"
    <> " const bgB = (bgHex & 255) / 255;"
    <> destroyPersistJs
    <> " prevPrec = PIXI.settings.PRECISION_FRAGMENT;"
    <> " PIXI.settings.PRECISION_FRAGMENT = 'highp';"
    <> " const viewW = (app.renderer && app.renderer.width) || "
    <> show (round canvasW :: Int)
    <> ";"
    <> " const viewH = (app.renderer && app.renderer.height) || "
    <> show (round canvasH :: Int)
    <> ";"
    <> " let scratch = viewport._glScratch;"
    <> " if (!scratch) {"
    <> "   scratch = viewport._glScratch = {"
    <> "     pan: new Float32Array(2),"
    <> "     texSize: new Float32Array(2),"
    <> "     filterArea: new PIXI.Rectangle(0, 0, viewW, viewH),"
    <> "     emptyContainer: new PIXI.Container()"
    <> "   };"
    <> " } else {"
    <> "   scratch.filterArea.width = viewW;"
    <> "   scratch.filterArea.height = viewH;"
    <> " }"
    <> " scratch.texSize[0] = texW; scratch.texSize[1] = texH;"
    <> " const rtOpts = { width: texW, height: texH, resolution: 1,"
    <> "   scaleMode: PIXI.SCALE_MODES.NEAREST };"
    <> " const atlasPrev = PIXI.RenderTexture.create(rtOpts);"
    <> " atlasPrev.clearColor = [bgR, bgG, bgB, 0];"
    <> " if (app && app.renderer && app.renderer.renderTexture) {"
    <> "   app.renderer.render(scratch.emptyContainer,"
    <> "     { renderTexture: atlasPrev, clear: true });"
    <> " }"
    <> " if (currTex && currTex.baseTexture) {"
    <> "   currTex.baseTexture.wrapMode = PIXI.WRAP_MODES.CLAMP;"
    <> " }"
    <> " const sdfFilter = new PIXI.Filter(undefined, sdfFrag, {"
    <> "   uTexSize: scratch.texSize,"
    <> "   uPan: scratch.pan, uBg: [bgR, bgG, bgB],"
    <> "   uAtlas: currTex, uPrevAtlas: atlasPrev, uCellPx: 3, uTime: 0"
    <> " });"
    <> " sdfFilter.autoFit = false;"
    <> " sdfFilter.padding = 0;"
    <> " sdfFilter.resolution = 1;"
    <> " sdfFilter.filterArea = scratch.filterArea;"
    <> " const atlasCopySpr = new PIXI.Sprite(currTex);"
    <> " atlasCopySpr.width = texW;"
    <> " atlasCopySpr.height = texH;"
    <> " PIXI.settings.PRECISION_FRAGMENT = prevPrec;"
    <> " sprite.filters = null;"
    <> " sprite.renderable = false;"
    <> " sprite.visible = false;"
    <> " if (viewport.screenQuad && viewport.screenQuad.destroy) {"
    <> "   try { viewport.screenQuad.destroy(); } catch (_) {}"
    <> " }"
    <> " const quad = new PIXI.Graphics();"
    <> " quad.beginFill(0xffffff, 1);"
    <> " quad.drawRect(0, 0, viewW, viewH);"
    <> " quad.endFill();"
    <> " quad.filters = [sdfFilter];"
    <> " quad.filterArea = scratch.filterArea;"
    <> " app.stage.addChild(quad);"
    <> " viewport.screenQuad = quad;"
    <> " viewport.atlasPrev = atlasPrev;"
    <> " viewport.atlasCopySpr = atlasCopySpr;"
    <> " viewport.sdfFilter = sdfFilter;"
    <> " viewport.atlasTexW = texW;"
    <> " viewport.atlasTexH = texH;"
    <> " viewport.onionSkip = 2;"
    <> " viewport.lifeShader = 1;"
    <> " } catch (err) {"
    <> "   if (prevPrec !== undefined) PIXI.settings.PRECISION_FRAGMENT = prevPrec;"
    <> "   viewport.lifeShader = 0;"
    <> "   console.error('[Life] cell shader failed', err);"
    <> " }"

-- | Hot path: two-frame onion skin, outside glow SDF, then stage present.
--   Expects @app@, @viewport@, @currTex@, @now@, @upload@, @stageDirty@.
presentGridJs :: String
presentGridJs =
  " if (!app || !app.renderer) return;"
    <> " if (app.renderer.gl && app.renderer.gl.isContextLost"
    <> "   && app.renderer.gl.isContextLost()) return;"
    <> " if (upload && currTex && currTex.baseTexture) currTex.baseTexture.update();"
    <> " const pSdf = viewport.sdfFilter;"
    <> " const pSpr = viewport.sprite;"
    <> " if (!viewport.lifeShader || !pSdf || !pSpr) {"
    <> "   if (upload || stageDirty) app.renderer.render(app.stage);"
    <> "   return;"
    <> " }"
    <> " const tw = (currTex && currTex.width) ? (currTex.width | 0) : 0;"
    <> " const th = (currTex && currTex.height) ? (currTex.height | 0) : 0;"
    <> " let prevRT = viewport.atlasPrev;"
    <> " if (prevRT && tw && th && (prevRT.width !== tw || prevRT.height !== th)) {"
    <> "   if (prevRT.destroy) { try { prevRT.destroy(true); } catch (_) {} }"
    <> "   viewport.atlasPrev = null;"
    <> "   prevRT = null;"
    <> "   viewport.onionSkip = 2;"
    <> " }"
    <> " if (!prevRT && tw && th && app && app.renderer && app.renderer.renderTexture) {"
    <> "   prevRT = PIXI.RenderTexture.create({"
    <> "     width: tw, height: th, resolution: 1,"
    <> "     scaleMode: PIXI.SCALE_MODES.NEAREST"
    <> "   });"
    <> "   viewport.atlasPrev = prevRT;"
    <> "   app.renderer.render("
    <> "     viewport._glScratch ? viewport._glScratch.emptyContainer"
    <> "       : new PIXI.Container(),"
    <> "     { renderTexture: prevRT, clear: true });"
    <> "   if (viewport.atlasCopySpr) {"
    <> "     viewport.atlasCopySpr.width = tw;"
    <> "     viewport.atlasCopySpr.height = th;"
    <> "   }"
    <> " }"
    <> " const prevTex = viewport.atlasPrev || currTex;"
    <> " pSdf.uniforms.uAtlas = currTex;"
    <> " if ((viewport.onionSkip | 0) > 0) {"
    <> "   pSdf.uniforms.uPrevAtlas = currTex;"
    <> "   viewport.onionSkip = (viewport.onionSkip | 0) - 1;"
    <> " } else {"
    <> "   pSdf.uniforms.uPrevAtlas = prevTex;"
    <> " }"
    <> " const viewW = (app.renderer.screen && app.renderer.screen.width) || app.renderer.width || 768;"
    <> " const viewH = (app.renderer.screen && app.renderer.screen.height) || app.renderer.height || 576;"
    <> " const quad = viewport.screenQuad;"
    <> " const scratch = viewport._glScratch;"
    <> " if (quad && quad.clear && (quad.width !== viewW || quad.height !== viewH)) {"
    <> "   quad.clear(); quad.beginFill(0xffffff, 1);"
    <> "   quad.drawRect(0, 0, viewW, viewH); quad.endFill();"
    <> " }"
    <> " if (scratch) {"
    <> "   scratch.pan[0] = pSpr.position.x || 0;"
    <> "   scratch.pan[1] = pSpr.position.y || 0;"
    <> "   if (currTex && currTex.width) {"
    <> "     scratch.texSize[0] = currTex.width;"
    <> "     scratch.texSize[1] = currTex.height;"
    <> "   }"
    <> "   scratch.filterArea.width = viewW;"
    <> "   scratch.filterArea.height = viewH;"
    <> "   pSdf.filterArea = scratch.filterArea;"
    <> "   if (quad) quad.filterArea = scratch.filterArea;"
    <> " } else {"
    <> "   pSdf.filterArea = new PIXI.Rectangle(0, 0, viewW, viewH);"
    <> "   pSdf.uniforms.uPan = [pSpr.position.x || 0, pSpr.position.y || 0];"
    <> "   if (currTex && currTex.width)"
    <> "     pSdf.uniforms.uTexSize = [currTex.width, currTex.height];"
    <> "   if (quad) quad.filterArea = pSdf.filterArea;"
    <> " }"
    <> " pSdf.autoFit = false;"
    <> " pSdf.uniforms.uCellPx = pSpr.scale && pSpr.scale.x ? pSpr.scale.x : 3;"
    <> " pSdf.uniforms.uTime = (now || 0) * 0.001;"
    <> " pSpr.renderable = false;"
    <> " pSpr.visible = false;"
    <> " if (upload || stageDirty) app.renderer.render(app.stage);"
    <> " if (upload && viewport.atlasPrev && viewport.atlasCopySpr && currTex) {"
    <> "   try {"
    <> "     const copy = viewport.atlasCopySpr;"
    <> "     copy.texture = currTex;"
    <> "     copy.width = currTex.width;"
    <> "     copy.height = currTex.height;"
    <> "     app.renderer.render(copy, {"
    <> "       renderTexture: viewport.atlasPrev, clear: true"
    <> "     });"
    <> "   } catch (err) {"
    <> "     console.error('[Life] atlas prev copy failed', err);"
    <> "   }"
    <> " }"

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
          <> " texW = viewport.worldW || texW;"
          <> " texH = viewport.worldH || texH;"
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
          <> "   console.warn('[Life] WebGL context lost, using 2D fallback');"
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
          <> "     console.warn('[Life] WebGL unavailable, using 2D fallback');"
          <> "   }"
          <> "   return;"
          <> " }"
          <> " if (lost) return;"
          <> " if (typeof viewport._lifeRebuild !== 'function') return;"
          <> " if (viewport._glCooldown > 0) { viewport._glCooldown--; return; }"
          <> " viewport.glLost = 0;"
          <> " console.info('[Life] WebGL recovered, rebuilding GPU renderer');"
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

-- | @PIXI.Texture.fromBuffer@: one RGBA texel per grid cell; the sprite
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

-- | Attach the Life cell shader from @viewport.cellFragSrc@.
--   Call 'prefetchLifeShader' before the first install.
installLifeShader ::
  Effect f ('MutableObject Application)
  -> Effect f ('MutableObject ())
  -> Expr f ('MutableObject Sprite)
  -> Expr f ('MutableObject Texture)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
installLifeShader app viewport sprite tex w h = do
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
          <: arg w
          <: arg h
          <: arg (number canvasBgPixi)
          <: RecNil
      )
  done

prefetchLifeShader ::
  Effect f ('MutableObject ())
  -> Expr f 'String
  -> Effect f ('MutableObject (Promise 'Unit))
prefetchLifeShader viewport url =
  ffi prefetchLifeShaderJs (ArgEffect viewport <: arg url <: RecNil)

replaceGridTexture ::
  Effect f ('MutableObject ())
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f ('MutableObject Texture))
replaceGridTexture viewport buf w h =
  fmap
    var
    ( toSyntax
        ( ffi
            ( "(viewport, buf, w, h) => {"
                <> destroyPersistJs
                <> " viewport.onionSkip = 2;"
                <> " const old = viewport.texture;"
                <> " const tex = PIXI.Texture.fromBuffer(buf, w, h, {"
                <> "   format: PIXI.FORMATS.RGBA,"
                <> "   type: PIXI.TYPES.UNSIGNED_BYTE"
                <> " });"
                <> " tex.baseTexture.scaleMode = PIXI.SCALE_MODES.NEAREST;"
                <> " tex.baseTexture.wrapMode = PIXI.WRAP_MODES.CLAMP;"
                <> " const res = tex.baseTexture.resource;"
                <> " if (res) { res.data = buf; if (res.source) res.source = buf; }"
                <> " if (viewport.sprite) viewport.sprite.texture = tex;"
                <> " viewport.texture = tex;"
                <> " if (old && old !== tex && old.destroy) {"
                <> "   try { old.destroy(true); } catch (_) {}"
                <> " }"
                <> " return tex;"
                <> " }"
            )
            (ArgEffect viewport <: arg buf <: arg w <: arg h <: RecNil)
        )
    )

-- | Upload the atlas when it changed, blend two frames, draw outside glow,
--   then present the stage when the atlas or viewport changed.
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

-- | Preview aimed glider cells + drag line on the Pixi stage (never the
--   2D overlay canvas — that blanks WebGL).
drawGliderGhost ::
  Expr f ('MutableObject Application)
  -> Effect f ('MutableObject ())
  -> Expr f ('Array ('Array 'Number))
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
drawGliderGhost app viewport cells gx gy cx cy panX panY zoom px = do
  toSyntax_
    $ discard
    $ ffi
      ( "((app,viewport,cells,gx,gy,cx,cy,panX,panY,zoom,cellPx) => {"
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
          <> " const scale = cellPx * zoom;"
          <> " gfx.beginFill(0x60a5fa, 0.42);"
          <> " for (let i = 0; i < cells.length; i++) {"
          <> "   const c = cells[i], x = gx + (c[0] | 0), y = gy + (c[1] | 0);"
          <> "   gfx.drawRect(panX + x * scale, panY + y * scale, scale, scale);"
          <> " }"
          <> " gfx.endFill();"
          <> " const ax = panX + (gx + 1.5) * scale;"
          <> " const ay = panY + (gy + 1.5) * scale;"
          <> " gfx.lineStyle(Math.max(1, scale * 0.12), 0x38bdf8, 0.85);"
          <> " gfx.moveTo(ax, ay); gfx.lineTo(cx, cy);"
          <> " app.__eraserGhostOn = true;"
          <> " viewport.renderPanValid = false;"
          <> " })"
      )
      ( arg app
          <: ArgEffect viewport
          <: arg cells
          <: arg gx
          <: arg gy
          <: arg cx
          <: arg cy
          <: arg panX
          <: arg panY
          <: arg zoom
          <: arg px
          <: RecNil
      )
  done
