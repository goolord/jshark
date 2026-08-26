{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Client (mainJS) where

import Catalog (catalogDisturb)
import Discover
  ( IndexTracker
  , Registry
  , initIndexContainer
  , initIndexTracker
  , initRegistry
  , initSeenSpecies
  , stepIndexTracker
  )
import Engine
import GHC.Generics (Generic)
import Grid
  ( BoundScratch (..)
  , RenderDirty (..)
  , StepCtx (StepCtx)
  , cellIdx
  , packedIsAlive
  , u8Get
  )
import JShark.Api
import qualified JShark.Array as Array
import qualified JShark.Dom as Dom
import JShark.Generic (MutableObjectOf, toObject)
import qualified JShark.Generic as G
import qualified JShark.Map as Map
import qualified JShark.Math as Math
import JShark.Promise (promiseThen)
import JShark.Rec (Rec (..), (<:))
import qualified JShark.Set as Set
import qualified JShark.String as String
import qualified JShark.Timers as Timers
import JShark.Types (Effect (Lift), Expr (Literal, Var))
import qualified JShark.Types as Ts
import JShark.Worker (performanceNow)
import Names (lookupDisplayName)
import qualified Pixi
import Types
  ( LifeState
  , boardId
  , canvasBgPixi
  , canvasH
  , canvasW
  , cellPx
  , eraserDefaultRadius
  , eraserMaxRadius
  , eraserMinRadius
  , eraserToolSid
  , gridH
  , gridW
  , hoverRadius
  , hudRefreshMs
  , indexRefreshMs
  , lifeBoard2dId
  , lifeDebugCollapseId
  , lifeDebugId
  , lifeEraserGhostId
  , lifeEraserRadiusId
  , lifeEraserRadiusValId
  , lifeEraserSizeId
  , lifePauseOverlayId
  , lifeSettingsCollapseId
  , lifeSettingsGridId
  , lifeSettingsId
  , lifeSettingsResetId
  , lifeSettingsTickId
  , lifeSettingsTickValId
  , lifeSettingsZoomId
  , lifeSettingsZoomInId
  , lifeSettingsZoomOutId
  , lifeStatCellsId
  , lifeStatEngineId
  , lifeStatFpsId
  , lifeStatGenId
  , lifeStatTickId
  , lifeStatZoomId
  , lifeToolsCollapseId
  , lifeToolsId
  , lifeTooltipId
  , lifeTooltipNameId
  , lifeTooltipSwatchId
  , seedH
  , seedOx
  , seedOy
  , seedW
  , simBudgetMs
  , tickMaxMs
  , tickMinMs
  , toggleToolSid
  , zoomLevelLabels
  , zoomLevels
  )
import WorkerBridge (engineModeLabel, engineTickMs, setEngineRenderMs)

data Fps = Fps
  { lastMs :: Double
  , fps :: Double
  }
  deriving Generic

mainJS :: forall f. EffectSyntax f (f 'Unit)
mainJS = do
  canvas <- Dom.lookupId (string boardId)
  pixiOk <- Pixi.pixiAvailable
  whenS pixiOk $
    do
      app <-
        Pixi.newApplication
          canvas
          (number canvasW)
          (number canvasH)
          (number canvasBgPixi)
      boot canvas app
  whenS (not_ pixiOk) $
    do
      toSyntax_
        $ discard
        $ ffi
          "(() => { console.error('[Life] PixiJS failed to load, check js/pixi.min.js'); })"
          RecNil
      done

boot ::
  Effect f ('MutableObject Dom.DomElement)
  -> Expr f ('MutableObject Pixi.Application)
  -> EffectSyntax f (f 'Unit)
boot canvas app = do
  appH <- hold (expr app)
  viewport <- initViewport
  renderDirty <- hold (toObject (RenderDirty 0 0 0 0 False False))
  let
    shaderP = Pixi.prefetchLifeShader viewport (string Pixi.lifeCellShaderUrl)
  promiseThen shaderP $ \_ ->
    stmts (bootLoaded canvas app appH viewport renderDirty)
  done

bootLoaded ::
  Effect f ('MutableObject Dom.DomElement)
  -> Expr f ('MutableObject Pixi.Application)
  -> Effect f ('MutableObject Pixi.Application)
  -> Effect f ('MutableObject ())
  -> Effect f ('MutableObject (G.As RenderDirty))
  -> EffectSyntax f (f 'Unit)
bootLoaded canvas app appH viewport renderDirty = do
  state <- initLife appH viewport
  _ <- setProp viewport "app" app
  Pixi.wireContextRecovery canvas viewport state
  stepCtx <- hold (toObject (StepCtx 0 0 (-1) (-1) 0 0 0 0 0))
  editScratch <- hold (toObject (BoundScratch 0 1e9 1e9 (-1) (-1)))
  registry <- initRegistry
  indexTracker <- initIndexTracker
  seenSpecies <- initSeenSpecies
  typesList <- initIndexContainer
  tooltip <- Dom.lookupId (string lifeTooltipId)
  swatchEl <- Dom.lookupId (string lifeTooltipSwatchId)
  nameEl <- Dom.lookupId (string lifeTooltipNameId)
  statGen <- Dom.lookupId (string lifeStatGenId)
  statCells <- Dom.lookupId (string lifeStatCellsId)
  statFps <- Dom.lookupId (string lifeStatFpsId)
  statZoom <- Dom.lookupId (string lifeStatZoomId)
  statTick <- Dom.lookupId (string lifeStatTickId)
  statEngine <- Dom.lookupId (string lifeStatEngineId)
  settingsZoom <- Dom.lookupId (string lifeSettingsZoomId)
  meter <- hold (G.toObject (Fps (-1) 0))
  tipSym <- toSyntax emptyObject
  let
    tipRef = Lift (Var tipSym)
  hitsSym <- toSyntax Set.new
  let
    hits = Lift (Var hitsSym)
  sidsScratch <- bindExpr $ Array.fromEffects []
  toolRef <- initTool
  toolsMap <- initDisturbCatalog
  toolBtns <- Dom.lookupSelector (string ".life-tool")
  toolBtnsE <- bindExpr toolBtns
  toolsTray <- Dom.lookupId (string lifeToolsId)
  toolsCollapse <- Dom.lookupId (string lifeToolsCollapseId)
  debugTray <- Dom.lookupId (string lifeDebugId)
  debugCollapse <- Dom.lookupId (string lifeDebugCollapseId)
  settingsTray <- Dom.lookupId (string lifeSettingsId)
  settingsCollapse <- Dom.lookupId (string lifeSettingsCollapseId)
  settingsZoomIn <- Dom.lookupId (string lifeSettingsZoomInId)
  settingsZoomOut <- Dom.lookupId (string lifeSettingsZoomOutId)
  settingsReset <- Dom.lookupId (string lifeSettingsResetId)
  settingsGrid <- Dom.lookupId (string lifeSettingsGridId)
  settingsTick <- Dom.lookupId (string lifeSettingsTickId)
  settingsTickVal <- Dom.lookupId (string lifeSettingsTickValId)
  pauseOverlay <- Dom.lookupId (string lifePauseOverlayId)
  eraserGhost <- Dom.lookupId (string lifeEraserGhostId)
  fallback2d <- Dom.lookupId (string lifeBoard2dId)
  eraserSize <- Dom.lookupId (string lifeEraserSizeId)
  eraserRadius <- Dom.lookupId (string lifeEraserRadiusId)
  eraserRadiusVal <- Dom.lookupId (string lifeEraserRadiusValId)
  wire canvas state tooltip tipRef toolRef toolsMap viewport editScratch
  wireTools toolRef toolBtnsE canvas eraserSize
  wireEraserSize toolRef eraserRadius eraserRadiusVal
  syncEraserUi toolRef canvas eraserSize
  wireCollapse
    toolsTray
    toolsCollapse
    "Collapse tools"
    "Expand tools"
    "−"
    "+"
  wireCollapse
    debugTray
    debugCollapse
    "Collapse debug"
    "Expand debug"
    "−"
    "debug"
  wireCollapse
    settingsTray
    settingsCollapse
    "Collapse settings"
    "Expand settings"
    "−"
    "settings"
  wireSettings viewport settingsZoomIn settingsZoomOut settingsReset
  wireSimSettings state viewport settingsGrid settingsTick settingsTickVal
  renderLife viewport renderDirty state fallback2d
  Timers.foreverFrame $ \now -> do
    tickFps meter now
    paused <- state.paused
    whenS (not_ paused) $
      stepLifeBudget state viewport registry stepCtx now
    tickIndex state registry indexTracker seenSpecies typesList now
    Pixi.tickGlRecovery canvas viewport state
    renderStart <- performanceNow
    renderLife viewport renderDirty state fallback2d
    renderEnd <- performanceNow
    setEngineRenderMs (renderEnd - renderStart)
    syncPauseOverlay state pauseOverlay
    lastHud <- getProp viewport "lastHudMs"
    whenS (now - lastHud .>= number (fromIntegral hudRefreshMs)) $ do
      updateHud
        state
        meter
        viewport
        statGen
        statCells
        statFps
        statZoom
        statTick
        statEngine
        settingsZoom
      _ <- setProp viewport "lastHudMs" now
      done
    tickHover tipRef sidsScratch state registry tooltip swatchEl nameEl hits toolRef
    tickEraserGhost toolRef tipRef state viewport eraserGhost

wire ::
  Effect f ('MutableObject Dom.DomElement)
  -> Effect f (MutableObjectOf LifeState)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject ())
  -> Effect f ('MutableObject ())
  -> Effect f ('Map 'Number ('Array ('Array 'Number)))
  -> Effect f ('MutableObject ())
  -> Effect f (MutableObjectOf BoundScratch)
  -> EffectSyntax f (f 'Unit)
wire canvas state tooltip tipRef toolRef toolsMap viewport editScratch = do
  _ <- Dom.setStyleProperty tooltip "visibility" (string "hidden")
  _ <- Dom.setAttribute tooltip "aria-hidden" (string "true")
  _ <- setProp tipRef "over" (number 0)
  _ <- setProp tipRef "gx" (number (-1))
  _ <- setProp tipRef "gy" (number (-1))
  _ <- setProp tipRef "cx" (number 0)
  _ <- setProp tipRef "cy" (number 0)
  _ <- setProp tipRef "shownGx" (number (-2))
  _ <- setProp tipRef "shownGy" (number (-2))
  _ <- setProp tipRef "fp" (string "")
  _ <- setProp tipRef "swatchSid" (number (-1))
  win <- hold window
  let
    endDrag = do
      _ <- setProp viewport "dragging" (number 0)
      _ <- setProp viewport "erasing" (number 0)
      syncEraserCursor canvas toolRef
  addEventListener "keydown" win $ \(e :: Expr f ('MutableObject ())) ->
    stmts $ do
      code <- getProp' e "code"
      toSyntax $
        stringCaseE
          code
          [
            ( "Escape"
            , discard $
                stmts $ do
                  toSyntax_ $ callMethod (expr e) "preventDefault" RecNil
                  togglePause state
                  done
            )
          ,
            ( "Equal"
            , discard $
                stmts $ do
                  toSyntax_ $ callMethod (expr e) "preventDefault" RecNil
                  zoomIn viewport
                  done
            )
          ,
            ( "Minus"
            , discard $
                stmts $ do
                  toSyntax_ $ callMethod (expr e) "preventDefault" RecNil
                  zoomOut viewport
                  done
            )
          ,
            ( "NumpadAdd"
            , discard $
                stmts $ do
                  toSyntax_ $ callMethod (expr e) "preventDefault" RecNil
                  zoomIn viewport
                  done
            )
          ,
            ( "NumpadSubtract"
            , discard $
                stmts $ do
                  toSyntax_ $ callMethod (expr e) "preventDefault" RecNil
                  zoomOut viewport
                  done
            )
          ]
          noOp
  addEventListener "mousedown" canvas $ \(e :: Expr f ('MutableObject ())) ->
    stmts $ do
      toSyntax_ $ callMethod canvas "focus" RecNil
      btn <- getProp' e "button"
      shift <- getProp' e "shiftKey"
      whenS (shift .&& btn .== 0) $ do
        toSyntax_ $ callMethod (expr e) "preventDefault" RecNil
        cx <- getProp' e "clientX"
        cy <- getProp' e "clientY"
        _ <- setProp viewport "dragging" (number 1)
        _ <- setProp viewport "dragX" cx
        _ <- setProp viewport "dragY" cy
        _ <- setProp viewport "dragStartX" cx
        _ <- setProp viewport "dragStartY" cy
        _ <- setProp viewport "moved" (number 0)
        Dom.setStyleProperty canvas "cursor" (string "grabbing")
      whenS (not_ shift .&& btn .== 0) $ do
        sid <- getProp toolRef "sid"
        whenS (sid .== number (fromIntegral eraserToolSid)) $ do
          _ <- setProp viewport "erasing" (number 1)
          cx <- getProp' e "clientX"
          cy <- getProp' e "clientY"
          ox <- getProp' e "offsetX"
          oy <- getProp' e "offsetY"
          (gx, gy) <- gridFromPointer canvas viewport ox oy
          syncPointerTip viewport tipRef cx cy gx gy
          applyErase state editScratch toolRef gx gy
  addEventListener "mouseup" canvas $ \_ -> stmts endDrag
  addEventListener "mouseup" win $ \_ -> stmts endDrag
  addEventListener "click" canvas $ \(e :: Expr f ('MutableObject ())) ->
    stmts $ do
      moved <- getProp viewport "moved"
      whenS (moved .== 0) $ do
        ox <- getProp' e "offsetX"
        oy <- getProp' e "offsetY"
        (gx, gy) <- gridFromPointer canvas viewport ox oy
        applyClick state editScratch toolRef toolsMap gx gy
      _ <- setProp viewport "moved" (number 0)
      done
  addEventListener "mousemove" canvas $ \(e :: Expr f ('MutableObject ())) ->
    stmts $ do
      cx <- getProp' e "clientX"
      cy <- getProp' e "clientY"
      dragging <- getProp viewport "dragging"
      erasing <- getProp viewport "erasing"
      ifS
        (dragging .== 1)
        ( do
            dragX <- getProp viewport "dragX"
            dragY <- getProp viewport "dragY"
            panX <- getProp viewport "panX"
            panY <- getProp viewport "panY"
            clientW <- getProp canvas "clientWidth"
            let
              bufScale = number canvasW / clientW
            _ <- setProp viewport "panX" (panX + (cx - dragX) * bufScale)
            _ <- setProp viewport "panY" (panY + (cy - dragY) * bufScale)
            clampPan viewport
            invalidateViewportRender viewport
            _ <- setProp viewport "dragX" cx
            _ <- setProp viewport "dragY" cy
            startX <- getProp viewport "dragStartX"
            startY <- getProp viewport "dragStartY"
            let
              dx = cx - startX
              dy = cy - startY
            whenS (dx * dx + dy * dy .> 9) $
              setProp viewport "moved" (number 1)
        )
        ( ifS
            (erasing .== 1)
            ( do
                ox <- getProp' e "offsetX"
                oy <- getProp' e "offsetY"
                (gx, gy) <- gridFromPointer canvas viewport ox oy
                syncPointerTip viewport tipRef cx cy gx gy
                applyErase state editScratch toolRef gx gy
            )
            ( do
                ox <- getProp' e "offsetX"
                oy <- getProp' e "offsetY"
                (gx, gy) <- gridFromPointer canvas viewport ox oy
                syncPointerTip viewport tipRef cx cy gx gy
            )
        )
  addEventListener "mouseleave" canvas $ \_ ->
    stmts $ do
      _ <- setProp tipRef "over" (number 0)
      endDrag
  toSyntax_ $ callMethod canvas "focus" RecNil
  done

hideTooltip ::
  Effect f ('MutableObject ())
  -> Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
hideTooltip tipRef tooltip = do
  _ <- setProp tipRef "fp" (string "")
  _ <- setProp tipRef "shownGx" (number (-2))
  _ <- setProp tipRef "shownGy" (number (-2))
  _ <- Dom.setAttribute tooltip "aria-hidden" (string "true")
  Dom.setStyleProperty tooltip "visibility" (string "hidden")

syncPointerTip ::
  Effect f ('MutableObject ())
  -> Effect f ('MutableObject ())
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
syncPointerTip viewport tipRef cx cy gx gy = do
  w <- getProp viewport "worldW"
  h <- getProp viewport "worldH"
  _ <- setProp tipRef "cx" cx
  _ <- setProp tipRef "cy" cy
  _ <- setProp tipRef "gx" gx
  _ <- setProp tipRef "gy" gy
  setProp
    tipRef
    "over"
    (if_ (gx .>= 0 .&& gy .>= 0 .&& gx .< w .&& gy .< h) (number 1) (number 0))

tickHover ::
  Effect f ('MutableObject ())
  -> Expr f ('Array 'Number)
  -> Effect f (MutableObjectOf LifeState)
  -> Effect f ('MutableObject Registry)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('Set Number)
  -> Effect f ('MutableObject ())
  -> EffectSyntax f (f 'Unit)
tickHover tipRef sidsScratch state registry tooltip swatchEl nameEl hits toolRef = do
  sid <- getProp toolRef "sid"
  ifS
    (sid .== number (fromIntegral eraserToolSid))
    (hideTooltip tipRef tooltip)
    ( do
        over <- getProp tipRef "over"
        ifS
          (over .== 0)
          ( do
              fp <- getProp tipRef "fp"
              whenS (fp .!= string "") (hideTooltip tipRef tooltip)
          )
          ( do
              gx <- getProp tipRef "gx"
              gy <- getProp tipRef "gy"
              lastGx <- getProp tipRef "shownGx"
              lastGy <- getProp tipRef "shownGy"
              whenS (gx .!= lastGx .|| gy .!= lastGy) $ do
                _ <- setProp tipRef "shownGx" gx
                _ <- setProp tipRef "shownGy" gy
                cx <- getProp tipRef "cx"
                cy <- getProp tipRef "cy"
                w <- state.worldW
                h <- state.worldH
                applyHover
                  tipRef
                  sidsScratch
                  state
                  registry
                  tooltip
                  swatchEl
                  nameEl
                  hits
                  w
                  h
                  gx
                  gy
                  cx
                  cy
          )
    )

-- | Grid index lookup, not a board-wide collision scan. A live cursor
--   cell is O(1). Empty cells search the Chebyshev square of
--   'hoverRadius' (25 cells at r=2). DOM writes only when the species
--   set changes: the tooltip does not follow the cursor inside a cell.
applyHover ::
  Effect f ('MutableObject ())
  -> Expr f ('Array 'Number)
  -> Effect f (MutableObjectOf LifeState)
  -> Effect f ('MutableObject Registry)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('Set Number)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
applyHover tipRef sidsScratch state registry tooltip swatchEl nameEl hits w h gx gy cx cy = do
  alive <- state.alive
  species <- state.species
  pal <- state.palette
  _ <- Set.clear hits
  let
    i = cellIdx w gx gy
  ifS
    (packedIsAlive alive i)
    ( do
        sid <- u8Get species i
        _ <- Set.insert hits sid
        setProp tipRef "swatchSid" sid
    )
    (collectNearby alive species w h gx gy hits tipRef)
  hitN <- Set.size hits
  ifS
    (hitN .== 0)
    (hideTooltip tipRef tooltip)
    ( do
        Array.clear_ sidsScratch
        _ <- Set.mapM_ (\sid -> Array.push_ sidsScratch sid) hits
        toSyntax_ $ Array.sort sidsScratch (\x y -> x - y)
        _ <- setProp tipRef "fpBuild" (string "")
        _ <- setProp tipRef "label" (string "")
        _ <-
          forRange_ (number 0) (Array.length sidsScratch) $ \idx -> do
            sid <- pure (Array.index sidsScratch idx)
            curFp <- getProp tipRef "fpBuild"
            _ <-
              setProp
                tipRef
                "fpBuild"
                ( if_
                    (curFp .== string "")
                    (toString sid)
                    (curFp <> string "," <> toString sid)
                )
            nm <- lookupDisplayName sid registry
            curLabel <- getProp tipRef "label"
            ifS
              (curLabel .== string "")
              (setProp tipRef "label" nm)
              (setProp tipRef "label" (curLabel <> string ", " <> nm))
        fp <- getProp tipRef "fpBuild"
        prev <- getProp tipRef "fp"
        whenS (structuralNEq fp prev) $ do
          _ <- setProp tipRef "fp" fp
          label <- getProp tipRef "label"
          swatchSid <- getProp tipRef "swatchSid"
          let
            base = swatchSid * number 3
          r <- u8Get pal base
          g <- u8Get pal (base + 1)
          b <- u8Get pal (base + 2)
          rgb <-
            pure
              ( string "rgb("
                  <> toString r
                  <> string ","
                  <> toString g
                  <> string ","
                  <> toString b
                  <> string ")"
              )
          _ <- Dom.setStyleProperty swatchEl "background" rgb
          _ <- Dom.setTextContent nameEl label
          let
            off = number 12
          _ <-
            Dom.setStyleProperty
              tooltip
              "transform"
              ( string "translate("
                  <> toString (cx + off)
                  <> string "px,"
                  <> toString (cy + off)
                  <> string "px)"
              )
          _ <- Dom.setAttribute tooltip "aria-hidden" (string "false")
          Dom.setStyleProperty tooltip "visibility" (string "visible")
    )

collectNearby ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Effect f ('Set Number)
  -> Effect f ('MutableObject ())
  -> EffectSyntax f (f 'Unit)
collectNearby alive species w h gx gy hits tipRef = do
  let
    r = number (fromIntegral hoverRadius)
    side = r + r + number 1
    cells = side * side
  _ <- setProp tipRef "best" (number 999999)
  forRange_ (number 0) cells $ \k -> do
    let
      dx = rem_ k side - r
      dy = Math.floor (k / side) - r
      x = gx + dx
      y = gy + dy
    whenS (x .>= 0 .&& y .>= 0 .&& x .< w .&& y .< h) $ do
      let
        j = cellIdx w x y
      whenS (packedIsAlive alive j) $ do
        let
          dist = dx * dx + dy * dy
        sid <- u8Get species j
        best <- getProp tipRef "best"
        whenS (dist .< best) $ do
          _ <- setProp tipRef "best" dist
          _ <- setProp tipRef "swatchSid" sid
          _ <- Set.clear hits
          Set.insert hits sid
        whenS (dist .== best) $ Set.insert hits sid

gridFromPointer ::
  Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject ())
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number, Expr f 'Number)
gridFromPointer canvas viewport localX localY = do
  clientW <- getProp canvas "clientWidth"
  panX <- getProp viewport "panX"
  panY <- getProp viewport "panY"
  zoom <- getProp viewport "zoom"
  let
    px = number (fromIntegral cellPx)
    bufScale = number canvasW / clientW
  pure
    ( Math.floor ((localX * bufScale - panX) / zoom / px)
    , Math.floor ((localY * bufScale - panY) / zoom / px)
    )

initViewport :: EffectSyntax f (Effect f ('MutableObject ()))
initViewport = do
  viewport <- hold newObject
  let
    cx = number (fromIntegral (seedOx + seedW `div` 2))
    cy = number (fromIntegral (seedOy + seedH `div` 2))
    px = number (fromIntegral cellPx)
  _ <- setProp viewport "panX" (number (canvasW / 2) - cx * px)
  _ <- setProp viewport "panY" (number (canvasH / 2) - cy * px)
  _ <- setProp viewport "zoom" (number 1)
  _ <- setProp viewport "worldW" (number (fromIntegral gridW))
  _ <- setProp viewport "worldH" (number (fromIntegral gridH))
  _ <- setProp viewport "lastStepMs" (number (-1))
  _ <- setProp viewport "renderPanX" (number (canvasW / 2) - cx * px)
  _ <- setProp viewport "renderPanY" (number (canvasH / 2) - cy * px)
  _ <- setProp viewport "renderZoom" (number 1)
  _ <- setProp viewport "renderPanValid" true_
  _ <- setProp viewport "dragging" (number 0)
  _ <- setProp viewport "dragX" (number 0)
  _ <- setProp viewport "dragY" (number 0)
  _ <- setProp viewport "dragStartX" (number 0)
  _ <- setProp viewport "dragStartY" (number 0)
  _ <- setProp viewport "moved" (number 0)
  _ <- setProp viewport "erasing" (number 0)
  _ <- setProp viewport "zoomLevels" zoomLevelsLit
  _ <- setProp viewport "zoomLabels" zoomLabelsLit
  _ <- setProp viewport "zoomIndices" zoomIndicesLit
  _ <- setProp viewport "glLost" (number 0)
  clampPan viewport
  pure viewport

zoomIndicesLit :: forall f. Expr f ('Array 'Number)
zoomIndicesLit =
  Literal $
    Ts.ValueArray (map (Ts.ValueNumber . fromIntegral) [0 .. length zoomLevels - 1])

zoomLevelsLit :: forall f. Expr f ('Array 'Number)
zoomLevelsLit =
  Literal $ Ts.ValueArray (map Ts.ValueNumber zoomLevels)

zoomLabelsLit :: forall f. Expr f ('Array 'String)
zoomLabelsLit =
  Literal $ Ts.ValueArray (map Ts.ValueString zoomLevelLabels)

nearestZoomIndex ::
  Expr f ('Array 'Number)
  -> Expr f ('Array 'Number)
  -> Expr f 'Number
  -> Expr f 'Number
nearestZoomIndex levels indices zoom =
  Array.reduce
    indices
    (number 0)
    ( \bestIdx i ->
        let
          bestDist = abs (Array.index levels bestIdx - zoom)
          curDist = abs (Array.index levels i - zoom)
         in
          if_ (curDist .< bestDist) i bestIdx
    )

clampZoomIndex ::
  Expr f ('Array 'Number)
  -> Expr f 'Number
  -> Expr f 'Number
clampZoomIndex indices idx =
  let
    len = Array.length indices
   in
    Math.max (number 0) (Math.min (len - number 1) idx)

stepZoom ::
  Effect f ('MutableObject ())
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
stepZoom viewport delta = do
  levels <- getProp viewport "zoomLevels"
  indices <- getProp viewport "zoomIndices"
  z0 <- getProp viewport "zoom"
  let
    idx = nearestZoomIndex levels indices z0
    nextIdx = clampZoomIndex indices (idx + delta)
    z1 = Array.index levels nextIdx
  applyZoom viewport z0 z1

applyZoom ::
  Effect f ('MutableObject ())
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
applyZoom viewport z0 z1 = do
  let
    cx = number (canvasW / 2)
    cy = number (canvasH / 2)
  whenS (z1 .!= z0) $ do
    panX0 <- getProp viewport "panX"
    panY0 <- getProp viewport "panY"
    _ <- setProp viewport "zoom" z1
    _ <- setProp viewport "panX" (cx - (cx - panX0) * z1 / z0)
    _ <- setProp viewport "panY" (cy - (cy - panY0) * z1 / z0)
    clampPan viewport
    invalidateViewportRender viewport

zoomIn ::
  Effect f ('MutableObject ())
  -> EffectSyntax f (f 'Unit)
zoomIn viewport = stepZoom viewport (number 1)

zoomOut ::
  Effect f ('MutableObject ())
  -> EffectSyntax f (f 'Unit)
zoomOut viewport = stepZoom viewport (number (-1))

invalidateViewportRender ::
  Effect f ('MutableObject ())
  -> EffectSyntax f (f 'Unit)
invalidateViewportRender viewport =
  setProp viewport "renderPanValid" false_

clampPan ::
  Effect f ('MutableObject ())
  -> EffectSyntax f (f 'Unit)
clampPan viewport = do
  zoom <- getProp viewport "zoom"
  panX <- getProp viewport "panX"
  panY <- getProp viewport "panY"
  let
    px = number (fromIntegral cellPx)
    scale = px * zoom
  gw <- getProp viewport "worldW"
  gh <- getProp viewport "worldH"
  let
    worldW = gw * scale
    worldH = gh * scale
    cw = number canvasW
    ch = number canvasH
    minPanX = Math.min (number 0) (cw - worldW)
    maxPanX = Math.max (number 0) (cw - worldW)
    minPanY = Math.min (number 0) (ch - worldH)
    maxPanY = Math.max (number 0) (ch - worldH)
  _ <-
    setProp viewport "panX" $
      Math.max minPanX (Math.min maxPanX panX)
  setProp viewport "panY" $
    Math.max minPanY (Math.min maxPanY panY)

initTool :: EffectSyntax f (Effect f ('MutableObject ()))
initTool = do
  toolRef <- hold newObject
  _ <- setProp toolRef "sid" (number (fromIntegral toggleToolSid))
  _ <-
    setProp toolRef "eraserRadius" (number (fromIntegral eraserDefaultRadius))
  pure toolRef

initDisturbCatalog ::
  EffectSyntax f (Effect f ('Map 'Number ('Array ('Array 'Number))))
initDisturbCatalog = do
  pairs <- bindExpr catalogDisturb
  hold $ Map.fromEntries pairs

applyClick ::
  Effect f (MutableObjectOf LifeState)
  -> Effect f (MutableObjectOf BoundScratch)
  -> Effect f ('MutableObject ())
  -> Effect f ('Map 'Number ('Array ('Array 'Number)))
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
applyClick state editScratch toolRef toolsMap gx gy = do
  sid <- getProp toolRef "sid"
  ifS
    (sid .== number (fromIntegral toggleToolSid))
    (flipCell state gx gy)
    ( ifS
        (sid .== number (fromIntegral eraserToolSid))
        done
        ( do
            hit <- Map.lookup toolsMap sid
            toSyntax $
              optionCaseE
                hit
                noOp
                (\cells -> fromSyntax $ placePattern state editScratch cells gx gy sid)
        )
    )

applyErase ::
  Effect f (MutableObjectOf LifeState)
  -> Effect f (MutableObjectOf BoundScratch)
  -> Effect f ('MutableObject ())
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
applyErase state editScratch toolRef gx gy = do
  radius0 <- getProp toolRef "eraserRadius"
  let
    radius = Math.floor radius0
  eraseCircle state editScratch gx gy radius

wireTools ::
  Effect f ('MutableObject ())
  -> Expr f ('Array ('MutableObject Dom.DomElement))
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
wireTools toolRef btns canvas eraserSize = do
  forRange_ (number 0) (Array.length btns) $ \i -> do
    btn <- hold (expr (Array.index btns i))
    addEventListener "click" btn $ \_ ->
      stmts $ do
        raw <- Dom.getAttribute btn "data-tool"
        selectTool toolRef btns (parseInt_ raw (number 10)) canvas eraserSize
    done

selectTool ::
  Effect f ('MutableObject ())
  -> Expr f ('Array ('MutableObject Dom.DomElement))
  -> Expr f 'Number
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
selectTool toolRef btns sid canvas eraserSize = do
  _ <- setProp toolRef "sid" sid
  forRange_ (number 0) (Array.length btns) $ \i -> do
    btn <- hold (expr (Array.index btns i))
    raw <- Dom.getAttribute btn "data-tool"
    let
      on = parseInt_ raw (number 10) .== sid
    toSyntax_ $
      callMethod
        btn
        "classList.toggle"
        (arg (string "is-selected") <: arg on <: RecNil)
    _ <-
      Dom.setAttribute
        btn
        "aria-pressed"
        (if_ on (string "true") (string "false"))
    done
  syncEraserUi toolRef canvas eraserSize

syncEraserUi ::
  Effect f ('MutableObject ())
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
syncEraserUi toolRef canvas eraserSize = do
  sid <- getProp toolRef "sid"
  let
    eraserOn = sid .== number (fromIntegral eraserToolSid)
  _ <-
    ifS
      eraserOn
      ( do
          toSyntax_ $
            callMethod
              eraserSize
              "removeAttribute"
              (arg (string "hidden") <: RecNil)
          Dom.setAttribute eraserSize "aria-hidden" (string "false")
      )
      ( do
          _ <- Dom.setAttribute eraserSize "hidden" (string "")
          Dom.setAttribute eraserSize "aria-hidden" (string "true")
      )
  syncEraserCursor canvas toolRef

syncEraserCursor ::
  Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject ())
  -> EffectSyntax f (f 'Unit)
syncEraserCursor canvas toolRef = do
  sid <- getProp toolRef "sid"
  ifS
    (sid .== number (fromIntegral eraserToolSid))
    (Dom.setStyleProperty canvas "cursor" eraserCursor)
    (Dom.setStyleProperty canvas "cursor" (string "crosshair"))

wireEraserSize ::
  Effect f ('MutableObject ())
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
wireEraserSize toolRef slider valEl = do
  addEventListener "input" slider $ \_ ->
    stmts $ do
      raw <- Dom.getValue slider
      let
        radius =
          Math.max
            (number (fromIntegral eraserMinRadius))
            ( Math.min
                (number (fromIntegral eraserMaxRadius))
                (parseInt_ raw (number 10))
            )
      _ <- setProp toolRef "eraserRadius" radius
      label <- pure (toString (Math.round radius))
      _ <- Dom.setValue slider label
      _ <- Dom.setAttribute slider "aria-valuenow" label
      Dom.setTextContent valEl label
      done
  done

-- | Clear and hide the 2D ghost overlay. Hiding matters: a visible canvas
--   stacked over the WebGL board occlusion-culls the board's WebGL quad in
--   software-composited browsers, blanking the whole game.
clearEraserGhostStm ::
  Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
clearEraserGhostStm ghost = do
  toSyntax_ $
    ffi
      ( "(function(ghost){if(ghost.style.display==='none')return;"
          <> "const ctx=ghost.getContext('2d');"
          <> "ctx.clearRect(0,0,ghost.width,ghost.height);"
          <> "ghost.style.display='none';})"
      )
      (ArgEffect ghost <: RecNil)
  done

drawEraserGhostStm ::
  Effect f ('MutableObject Dom.DomElement)
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
drawEraserGhostStm ghost alive w h gx gy radius panX panY zoom px = do
  toSyntax_ $
    ffi
      ( "(function(ghost,alive,w,h,gx,gy,r,panX,panY,zoom,cellPx){"
          <> "ghost.style.display='';"
          <> "const ctx=ghost.getContext('2d');"
          <> "ctx.clearRect(0,0,ghost.width,ghost.height);"
          <> "const ri=Math.max(0,Math.floor(r))|0;"
          <> "const scale=cellPx*zoom,rr=ri*ri;"
          <> "for(let dy=-ri;dy<=ri;dy++){"
          <> "for(let dx=-ri;dx<=ri;dx++){"
          <> "if(dx*dx+dy*dy>rr)continue;"
          <> "const x=(gx+dx)|0,y=(gy+dy)|0;"
          <> "if(x<0||y<0||x>=w||y>=h)continue;"
          <> "const i=y*w+x;"
          <> "if(alive[i]&1){"
          <> "ctx.fillStyle='rgba(248,113,113,0.5)';"
          <> "ctx.fillRect(panX+x*scale,panY+y*scale,scale,scale);"
          <> "}"
          <> "}"
          <> "}"
          <> "const cx=panX+(gx+0.5)*scale,cy=panY+(gy+0.5)*scale;"
          <> "ctx.beginPath();"
          <> "ctx.arc(cx,cy,(ri+0.5)*scale,0,Math.PI*2);"
          <> "ctx.strokeStyle='rgba(248,113,113,0.85)';"
          <> "ctx.lineWidth=Math.max(1,scale*0.15);"
          <> "ctx.stroke();"
          <> "})"
      )
      ( ArgEffect ghost
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

tickEraserGhost ::
  Effect f ('MutableObject ())
  -> Effect f ('MutableObject ())
  -> Effect f (MutableObjectOf LifeState)
  -> Effect f ('MutableObject ())
  -> Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
tickEraserGhost toolRef tipRef state viewport ghost = do
  sid <- getProp toolRef "sid"
  glLost <- getProp viewport "glLost"
  app <- getProp viewport "app"
  ifS
    (sid .== number (fromIntegral eraserToolSid))
    ( do
        over <- getProp tipRef "over"
        ifS
          (over .== 1)
          ( do
              gx <- getProp tipRef "gx"
              gy <- getProp tipRef "gy"
              radius <- getProp toolRef "eraserRadius"
              panX <- getProp viewport "panX"
              panY <- getProp viewport "panY"
              zoom <- getProp viewport "zoom"
              alive <- state.alive
              w <- state.worldW
              h <- state.worldH
              let
                px = number (fromIntegral cellPx)
              ifS
                (glLost .== 0)
                ( do
                    clearEraserGhostStm ghost
                    Pixi.drawEraserGhost app viewport alive w h gx gy radius panX panY zoom px
                )
                ( do
                    Pixi.clearEraserGhost app viewport
                    drawEraserGhostStm ghost alive w h gx gy radius panX panY zoom px
                )
          )
          ( do
              clearEraserGhostStm ghost
              Pixi.clearEraserGhost app viewport
          )
    )
    ( do
        clearEraserGhostStm ghost
        Pixi.clearEraserGhost app viewport
    )

eraserCursor :: Expr f 'String
eraserCursor =
  string
    ( "url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' "
        <> "width='24' height='24' viewBox='0 0 24 24'%3E%3Ccircle cx='12' "
        <> "cy='12' r='8' fill='none' stroke='%23f87171' stroke-width='2'/%3E"
        <> "%3C/svg%3E\") 12 12, crosshair"
    )

wireCollapse ::
  Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Expr f 'String
  -> Expr f 'String
  -> Expr f 'String
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
wireCollapse tray collapseBtn collapseLabel expandLabel openMark closedMark = do
  addEventListener "click" collapseBtn $ \_ ->
    stmts $ do
      toSyntax_ $
        callMethod
          tray
          "classList.toggle"
          (arg (string "is-collapsed") <: RecNil)
      collapsed <-
        bindExpr $
          ffi
            "((el) => el.classList.contains('is-collapsed'))"
            (ArgEffect tray <: RecNil)
      let
        expanded = not_ collapsed
      _ <-
        Dom.setAttribute
          collapseBtn
          "aria-expanded"
          (if_ expanded (string "true") (string "false"))
      _ <-
        Dom.setTextContent
          collapseBtn
          (if_ expanded openMark closedMark)
      _ <-
        Dom.setAttribute
          collapseBtn
          "aria-label"
          (if_ expanded collapseLabel expandLabel)
      done
  done

wireSettings ::
  Effect f ('MutableObject ())
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
wireSettings viewport zoomInBtn zoomOutBtn resetBtn = do
  addEventListener "click" zoomInBtn $ \_ ->
    stmts $ do
      zoomIn viewport
      done
  addEventListener "click" zoomOutBtn $ \_ ->
    stmts $ do
      zoomOut viewport
      done
  addEventListener "click" resetBtn $ \_ ->
    stmts $ do
      resetViewport viewport
      done
  done

wireSimSettings ::
  Effect f (MutableObjectOf LifeState)
  -> Effect f ('MutableObject ())
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
wireSimSettings state viewport gridSel tickSlider tickVal = do
  addEventListener "change" gridSel $ \_ ->
    stmts $ do
      raw <- Dom.getValue gridSel
      let
        parts = String.split raw (string "x")
        wParsed =
          if_
            (Array.length parts .>= number 1)
            (parseInt_ (Array.index parts (number 0)) (number 10))
            (number (fromIntegral gridW))
        hParsed =
          if_
            (Array.length parts .>= number 2)
            (parseInt_ (Array.index parts (number 1)) (number 10))
            (number (fromIntegral gridH))
        w =
          if_ (wParsed .> 0) wParsed (number (fromIntegral gridW))
        h =
          if_ (hParsed .> 0) hParsed (number (fromIntegral gridH))
      whenS (w .> 0 .&& h .> 0) $ resizeWorld state viewport w h
      done
  addEventListener "input" tickSlider $ \_ ->
    stmts $ do
      raw <- Dom.getValue tickSlider
      let
        ms =
          Math.max
            (number (fromIntegral tickMinMs))
            ( Math.min
                (number (fromIntegral tickMaxMs))
                (parseInt_ raw (number 10))
            )
      set @"tickMs" state ms
      ifS
        (ms .<= 0)
        (Dom.setTextContent tickVal (string "max"))
        ( do
            _ <-
              Dom.setTextContent
                tickVal
                (toString (Math.round ms) <> string " ms")
            done
        )
      _ <- Dom.setAttribute tickSlider "aria-valuenow" (toString ms)
      done
  done

resetViewport ::
  Effect f ('MutableObject ()) -> EffectSyntax f (f 'Unit)
resetViewport viewport = do
  w <- getProp viewport "worldW"
  h <- getProp viewport "worldH"
  let
    px = number (fromIntegral cellPx)
  _ <- setProp viewport "zoom" (number 1)
  _ <- setProp viewport "panX" (number (canvasW / 2) - (w / number 2) * px)
  _ <- setProp viewport "panY" (number (canvasH / 2) - (h / number 2) * px)
  clampPan viewport
  invalidateViewportRender viewport

syncPauseOverlay ::
  Effect f (MutableObjectOf LifeState)
  -> Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
syncPauseOverlay state overlay = do
  paused <- state.paused
  toSyntax_ $
    callMethod
      overlay
      "classList.toggle"
      (arg (string "is-visible") <: arg paused <: RecNil)
  Dom.setAttribute
    overlay
    "aria-hidden"
    (if_ paused (string "false") (string "true"))

updateHud ::
  Effect f (MutableObjectOf LifeState)
  -> Effect f (MutableObjectOf Fps)
  -> Effect f ('MutableObject ())
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> EffectSyntax f (f 'Unit)
updateHud
  state
  meter
  viewport
  statGen
  statCells
  statFps
  statZoom
  statTick
  statEngine
  settingsZoom = do
    gen <- state.gen
    pop <- state.pop
    fpsN <- meter.fps
    tickMs <- engineTickMs
    mode <- engineModeLabel
    levels <- getProp viewport "zoomLevels"
    labels <- getProp viewport "zoomLabels"
    indices <- getProp viewport "zoomIndices"
    zoom <- getProp viewport "zoom"
    let
      zoomIdx = nearestZoomIndex levels indices zoom
      zoomLabel = Array.index labels zoomIdx
    _ <- Dom.setTextContent statGen (toString gen)
    _ <- Dom.setTextContent statCells (toString pop)
    _ <-
      Dom.setTextContent
        statFps
        (toString (Math.round fpsN))
    _ <- Dom.setTextContent statZoom (zoomLabel <> string "%")
    _ <-
      Dom.setTextContent
        statTick
        (toString (Math.round tickMs) <> string "ms")
    _ <- Dom.setTextContent statEngine mode
    _ <- Dom.setTextContent settingsZoom (zoomLabel <> string "%")
    done

tickFps ::
  Effect f (MutableObjectOf Fps) -> Expr f 'Number -> EffectSyntax f (f 'Unit)
tickFps meter now = do
  prev <- meter.lastMs
  let
    dt = now - prev
  whenS (prev .>= 0 .&& dt .<= 250) $
    set @"fps" meter (Math.round (number 1000 / Math.max 1 dt))
  set @"lastMs" meter now

stepLifeBudget ::
  Effect f (MutableObjectOf LifeState)
  -> Effect f ('MutableObject ())
  -> Effect f ('MutableObject Registry)
  -> Effect f (MutableObjectOf StepCtx)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
stepLifeBudget state viewport registry stepCtx frameStart = do
  interval <- state.tickMs
  lastStep <- getProp viewport "lastStepMs"
  let
    due = lastStep .< number 0 .|| frameStart - lastStep .>= interval
    budget = number (fromIntegral simBudgetMs)
  whenS due $ do
    _ <- setProp viewport "lastStepMs" frameStart
    stepLife state registry stepCtx
    t1 <- performanceNow
    whenS (interval .<= number 0 .&& t1 - frameStart .< budget) $ do
      stepLife state registry stepCtx
      t2 <- performanceNow
      whenS (t2 - frameStart .< budget) $
        stepLife state registry stepCtx

tickIndex ::
  Effect f (MutableObjectOf LifeState)
  -> Effect f ('MutableObject Registry)
  -> Effect f ('MutableObject IndexTracker)
  -> Effect f ('Set Number)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
tickIndex state registry tracker seen listEl now = do
  pending <- getProp tracker "pending"
  indexLastMs <- getProp tracker "lastMs"
  let
    refresh = number (fromIntegral indexRefreshMs)
  whenS (not_ pending .&& (indexLastMs .== 0 .|| now - indexLastMs .>= refresh)) $ do
    alive <- state.alive
    species <- state.species
    pal <- state.palette
    liveX0 <- state.boundX0
    liveY0 <- state.boundY0
    liveX1 <- state.boundX1
    liveY1 <- state.boundY1
    w <- state.worldW
    h <- state.worldH
    stepIndexTracker
      alive
      species
      pal
      registry
      tracker
      seen
      listEl
      now
      liveX0
      liveY0
      liveX1
      liveY1
      w
      h
