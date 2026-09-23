{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module JShark.Example.Life.Client (mainJS) where

import Control.Monad (forM_)
import qualified Data.Text as T
import GHC.Generics (Generic)
import JShark.Api
import JShark.Dom (Event, addEventListener, eventButton, eventClientX, eventClientY, eventCode, eventOffsetX, eventOffsetY, eventShiftKey, window)
import JShark.Generic (MutableObjectOf, toObject)
import qualified JShark.Generic as G
import JShark.Api.Types (Effect (LambdaE, Lift), Expr (Literal, Var))
import qualified JShark.Api.Types as Ts
import qualified JShark.Array as Array
import qualified JShark.Dom as Dom
import JShark.Example.Life.Catalog (buildDisturbMap)
import JShark.Example.Life.Discover
  ( IndexTracker
  , IndexUi (..)
  , Registry
  , Scan (..)
  , initIndexContainer
  , initIndexTotal
  , initIndexTracker
  , initRegistry
  , initSeenSpecies
  , purgeEmergentDiscoveries
  , stepIndexTracker
  )
import JShark.Example.Life.Engine
import JShark.Example.Life.Grid
  ( BoundScratch (..)
  , RenderDirty (..)
  , StepCtx (StepCtx)
  , cellIdx
  , packedIsAlive
  , u8Get
  )
import JShark.Example.Life.GridApi (setProps)
import JShark.Example.Life.Names (lookupDisplayName)
import JShark.Example.Life.Patterns (gliderOrientationCells, gliderSpeciesSid)
import qualified JShark.Example.Life.Pixi as Pixi
import qualified JShark.Example.Life.Profile as Profile
import JShark.Example.Life.Types
import qualified JShark.Map as Map
import qualified JShark.Math as Math
import JShark.Promise (promiseThen)
import qualified JShark.Set as Set
import qualified JShark.String as String
import qualified JShark.Timers as Timers
import JShark.Worker (performanceNow)

data Fps = Fps
  { lastMs :: Double
  , fps :: Double
  }
  deriving Generic

type El f = Effect f ('MutableObject Dom.DomElement)

-- | Handles the frame's event handlers and frame loop share.
data Ui f = Ui
  { canvas :: El f
  , viewport :: Effect f ('MutableObject ())
  , state :: Effect f (MutableObjectOf LifeState)
  , stepCtx :: Effect f (MutableObjectOf StepCtx)
  , editScratch :: Effect f (MutableObjectOf BoundScratch)
  , registry :: Effect f ('MutableObject Registry)
  , indexTracker :: Effect f ('MutableObject IndexTracker)
  , seenSpecies :: Effect f ('Set 'Number)
  , meter :: Effect f (MutableObjectOf Fps)
  , tipRef :: Effect f ('MutableObject ())
  , hits :: Effect f ('Set 'Number)
  , sidsScratch :: Expr f ('Array 'Number)
  , toolRef :: Effect f ('MutableObject ())
  , toolsMap :: Effect f ('Map 'Number ('Array ('Array 'Number)))
  , toolBtnsE :: Expr f ('Array ('MutableObject Dom.DomElement))
  , typesList, indexTotal, tooltip, swatchEl, nameEl, statGen, statCells :: El f
  , statFps, statZoom, statRender, settingsZoom, settingsZoomIn :: El f
  , settingsZoomOut, settingsReset, settingsPurge, settingsGrid :: El f
  , settingsTick, settingsTickVal, pauseOverlay, eraserGhost, eraserSize :: El f
  , eraserRadius, eraserRadiusVal :: El f
  }

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
  El f
  -> Expr f ('MutableObject Pixi.Application)
  -> EffectSyntax f (f 'Unit)
boot canvas app = do
  appH <- hold (expr app)
  -- Register a dispose hook so a hot reload destroys the old Pixi renderer
  -- (and its textures) instead of leaking a WebGL context per reload.
  toSyntax_ $
    ffi
      "app => { window.__JSHARK_DISPOSE__ = function () { try { app.destroy(true, { children: true, texture: true, baseTexture: true }); } catch (_) {} }; }"
      (arg app <: RecNil)
  viewport <- initViewport
  Profile.install canvas viewport
  renderDirty <- hold (toObject (RenderDirty 0 0 0 0 False False))
  let
    shaderP = Pixi.prefetchLifeShader viewport (string Pixi.lifeCellShaderUrl)
  promiseThen shaderP $ \_ ->
    stmts (bootLoaded canvas app appH viewport renderDirty)
  done

bootLoaded ::
  El f
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
  indexTotal <- initIndexTotal
  tooltip <- Dom.lookupId (string lifeTooltipId)
  swatchEl <- Dom.lookupId (string lifeTooltipSwatchId)
  nameEl <- Dom.lookupId (string lifeTooltipNameId)
  statGen <- Dom.lookupId (string lifeStatGenId)
  statCells <- Dom.lookupId (string lifeStatCellsId)
  statFps <- Dom.lookupId (string lifeStatFpsId)
  statZoom <- Dom.lookupId (string lifeStatZoomId)
  statRender <- Dom.lookupId (string lifeStatRenderId)
  settingsZoom <- Dom.lookupId (string lifeSettingsZoomId)
  meter <- hold (G.toObject (Fps (-1) 0))
  tipSym <- toSyntax newObject
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
  settingsPurge <- Dom.lookupId (string lifeSettingsPurgeId)
  settingsGrid <- Dom.lookupId (string lifeSettingsGridId)
  settingsTick <- Dom.lookupId (string lifeSettingsTickId)
  settingsTickVal <- Dom.lookupId (string lifeSettingsTickValId)
  pauseOverlay <- Dom.lookupId (string lifePauseOverlayId)
  eraserGhost <- Dom.lookupId (string lifeEraserGhostId)
  fallback2d <- Dom.lookupId (string lifeBoard2dId)
  eraserSize <- Dom.lookupId (string lifeEraserSizeId)
  eraserRadius <- Dom.lookupId (string lifeEraserRadiusId)
  eraserRadiusVal <- Dom.lookupId (string lifeEraserRadiusValId)
  let
    ui = Ui {..}
  wire ui
  wireTools ui
  wireEraserSize ui
  syncEraserUi ui
  wireCollapse toolsTray toolsCollapse "Collapse tools" "Expand tools" "−" "+"
  wireCollapse debugTray debugCollapse "Collapse stats" "Expand stats" "−" "Stats"
  wireCollapse
    settingsTray
    settingsCollapse
    "Collapse settings"
    "Expand settings"
    "−"
    "Settings"
  wireSettings ui
  wirePurgeDiscoveries ui
  wireSimSettings ui
  renderLife viewport renderDirty state fallback2d
  Timers.foreverFrame $ \now -> do
    tickFps meter now
    paused <- state.paused
    stepT0 <- performanceNow
    whenS (not_ paused) $
      stepLifeFrame ui now
    stepT1 <- performanceNow
    tickIndex ui now
    Pixi.tickGlRecovery canvas viewport state
    otherEnd <- performanceNow
    renderStart <- performanceNow
    renderLife viewport renderDirty state fallback2d
    renderEnd <- performanceNow
    fpsN <- meter.fps
    genN <- state.gen
    popN <- state.pop
    glLost <- getProp viewport "glLost"
    Profile.sampleFrame
      viewport
      now
      fpsN
      genN
      popN
      (if_ paused (number 0) (stepT1 - stepT0))
      (renderEnd - renderStart)
      (otherEnd - stepT1)
      glLost
    syncPauseOverlay ui
    lastHud <- getProp viewport "lastHudMs"
    whenS (now - lastHud .>= number (fromIntegral hudRefreshMs)) $ do
      updateHud ui (renderEnd - renderStart)
      _ <- setProp viewport "lastHudMs" now
      done
    tickHover ui
    tickEraserGhost ui
    tickGliderGhost ui
    tickPanInertia viewport now

mouseToolN :: forall f. Expr f 'Number
mouseToolN = number (fromIntegral mouseToolSid)

eraserToolN :: forall f. Expr f 'Number
eraserToolN = number (fromIntegral eraserToolSid)

gliderToolN :: forall f. Expr f 'Number
gliderToolN = number (fromIntegral gliderToolSid)

isMouseToolSid :: Expr f 'Number -> Expr f 'Bool
isMouseToolSid sid = sid .== mouseToolN

isEraserToolSid :: Expr f 'Number -> Expr f 'Bool
isEraserToolSid sid = sid .== eraserToolN

isGliderToolSid :: Expr f 'Number -> Expr f 'Bool
isGliderToolSid sid = sid .== gliderToolN

isNonPaintToolSid :: Expr f 'Number -> Expr f 'Bool
isNonPaintToolSid sid =
  isEraserToolSid sid .|| isMouseToolSid sid .|| isGliderToolSid sid

finishGliderAim :: Ui f -> EffectSyntax f (f 'Unit)
finishGliderAim Ui {..} = do
  sid <- getProp toolRef "sid"
  aiming <- getProp viewport "gliderAiming"
  whenS (isGliderToolSid sid .&& aiming .== 1) $ do
    gx <- getProp viewport "gliderGx"
    gy <- getProp viewport "gliderGy"
    dir <- getProp viewport "gliderDir"
    let
      cells = Array.index gliderOrientationsLit dir
    placePattern
      state
      editScratch
      cells
      gx
      gy
      (number (fromIntegral gliderSpeciesSid))
  app <- getProp viewport "app"
  Pixi.clearEraserGhost app viewport
  setProp viewport "gliderAiming" (number 0)

handlePointerUp ::
  Ui f -> Expr f ('MutableObject Event) -> EffectSyntax f (f 'Unit)
handlePointerUp ui@Ui {..} e = do
  btn <- eventButton e
  whenS (btn .== 0) $ do
    aiming <- getProp viewport "gliderAiming"
    whenS (aiming .== 1) $ finishGliderAim ui
    _ <- setProp viewport "dragging" (number 0)
    setProp viewport "erasing" (number 0)
  whenS (btn .== 2) $ setProp viewport "rightPanning" (number 0)
  syncToolCursor ui

syncPointerAtEvent ::
  Ui f
  -> Expr f 'Bool
  -> Expr f ('MutableObject Event)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
syncPointerAtEvent ui doErase e cx cy = do
  ox <- eventOffsetX e
  oy <- eventOffsetY e
  (gx, gy) <- gridFromPointer ui ox oy
  syncPointerTip ui cx cy gx gy
  whenS doErase $ applyErase ui gx gy

handleCanvasMouseMove ::
  Ui f -> Expr f ('MutableObject Event) -> EffectSyntax f (f 'Unit)
handleCanvasMouseMove ui@Ui {..} e = do
  cx <- eventClientX e
  cy <- eventClientY e
  dragging <- getProp viewport "dragging"
  rightPanning <- getProp viewport "rightPanning"
  gliderAiming <- getProp viewport "gliderAiming"
  erasing <- getProp viewport "erasing"
  clientW <- getProp canvas "clientWidth"
  let
    bufScale = number canvasW / clientW
  ifS
    (dragging .== 1)
    ( do
        dragX <- getProp viewport "dragX"
        dragY <- getProp viewport "dragY"
        panX <- getProp viewport "panX"
        panY <- getProp viewport "panY"
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
        (rightPanning .== 1)
        ( do
            dragX <- getProp viewport "dragX"
            dragY <- getProp viewport "dragY"
            panX <- getProp viewport "panX"
            panY <- getProp viewport "panY"
            panLastMs <- getProp viewport "panLastMs"
            moveNow <- performanceNow
            let
              dx = (cx - dragX) * bufScale
              dy = (cy - dragY) * bufScale
              dt =
                Math.max ((moveNow - panLastMs) / number 1000) (number 0.001)
              instVx = dx / dt
              instVy = dy / dt
            velX <- getProp viewport "panVelX"
            velY <- getProp viewport "panVelY"
            _ <- setProp viewport "panX" (panX + dx)
            _ <- setProp viewport "panY" (panY + dy)
            _ <-
              setProp viewport "panVelX" $
                velX * number 0.65 + instVx * number 0.35
            _ <-
              setProp viewport "panVelY" $
                velY * number 0.65 + instVy * number 0.35
            clampPan viewport
            invalidateViewportRender viewport
            _ <- setProp viewport "dragX" cx
            _ <- setProp viewport "dragY" cy
            setProp viewport "panLastMs" moveNow
        )
        ( ifS
            (gliderAiming .== 1)
            ( do
                startX <- getProp viewport "dragStartX"
                startY <- getProp viewport "dragStartY"
                ox <- eventOffsetX e
                oy <- eventOffsetY e
                let
                  dx = cx - startX
                  dy = cy - startY
                  aimX = ox * bufScale
                  aimY = oy * bufScale
                _ <- setProp viewport "gliderCx" aimX
                _ <- setProp viewport "gliderCy" aimY
                _ <- setProp viewport "gliderDir" (gliderDirFromDrag dx dy)
                whenS (dx * dx + dy * dy .> 9) $
                  setProp viewport "moved" (number 1)
            )
            (syncPointerAtEvent ui (erasing .== 1) e cx cy)
        )
    )

wire :: Ui f -> EffectSyntax f (f 'Unit)
wire ui@Ui {..} = do
  _ <- Dom.setStyleProperty tooltip "visibility" (string "hidden")
  _ <- Dom.setStyleProperty tooltip "pointerEvents" (string "none")
  _ <- Dom.setAttribute tooltip "aria-hidden" (string "true")
  setProps tipRef [("over", 0), ("gx", -1), ("gy", -1), ("cx", 0), ("cy", 0)]
  setProps tipRef [("shownGx", -2), ("shownGy", -2)]
  _ <- setProp tipRef "fp" (string "")
  _ <- setProp tipRef "swatchSid" (number (-1))
  win <- hold window
  let
    endPointer = do
      setProps viewport [("dragging", 0), ("erasing", 0), ("rightPanning", 0)]
      finishGliderAim ui
      syncToolCursor ui
  toSyntax_ $
    ffi
      ( "(function(canvas, toolRef, mouseSid) {"
          <> " canvas.addEventListener('contextmenu', function(e) {"
          <> "   if (toolRef.sid === mouseSid) e.preventDefault();"
          <> " });"
          <> "})"
      )
      (ArgEffect canvas <: ArgEffect toolRef <: arg mouseToolN <: RecNil)
  addEventListener "keydown" win $ \e ->
    stmts $ do
      code <- eventCode e
      let
        key k act =
          ( k
          , discard $ stmts $ do
              toSyntax_ $ callMethod (expr e) "preventDefault" RecNil
              act
              done
          )
      toSyntax $
        stringCaseE
          code
          [ key "Escape" (togglePause state)
          , key "Equal" (zoomIn viewport)
          , key "Minus" (zoomOut viewport)
          , key "NumpadAdd" (zoomIn viewport)
          , key "NumpadSubtract" (zoomOut viewport)
          ]
          noOp
  addEventListener "mousedown" canvas $ \e ->
    stmts $ do
      toSyntax_ $ callMethod canvas "focus" RecNil
      btn <- eventButton e
      shift <- eventShiftKey e
      sid <- getProp toolRef "sid"
      whenS (btn .== 0 .&& (shift .|| isMouseToolSid sid)) $ do
        toSyntax_ $ callMethod (expr e) "preventDefault" RecNil
        cx <- eventClientX e
        cy <- eventClientY e
        setProps viewport [("dragging", 1), ("dragX", cx), ("dragY", cy)]
        setProps viewport [("dragStartX", cx), ("dragStartY", cy), ("moved", 0)]
        setProps viewport [("panVelX", 0), ("panVelY", 0)]
        syncToolCursor ui
      whenS (btn .== 2 .&& isMouseToolSid sid) $ do
        toSyntax_ $ callMethod (expr e) "preventDefault" RecNil
        cx <- eventClientX e
        cy <- eventClientY e
        moveNow <- performanceNow
        setProps viewport [("rightPanning", 1), ("panVelX", 0), ("panVelY", 0)]
        setProps viewport [("panLastMs", moveNow), ("dragX", cx), ("dragY", cy)]
        syncToolCursor ui
      whenS (not_ shift .&& btn .== 0 .&& sid .== eraserToolN) $ do
        _ <- setProp viewport "erasing" (number 1)
        cx <- eventClientX e
        cy <- eventClientY e
        ox <- eventOffsetX e
        oy <- eventOffsetY e
        (gx, gy) <- gridFromPointer ui ox oy
        syncPointerTip ui cx cy gx gy
        applyErase ui gx gy
      whenS (not_ shift .&& btn .== 0 .&& isGliderToolSid sid) $ do
        ox <- eventOffsetX e
        oy <- eventOffsetY e
        (gx, gy) <- gridFromPointer ui ox oy
        clientW <- getProp canvas "clientWidth"
        let
          bufScale = number canvasW / clientW
          ax = ox * bufScale
          ay = oy * bufScale
        cx <- eventClientX e
        cy <- eventClientY e
        setProps viewport [("gliderAiming", 1), ("gliderGx", gx), ("gliderGy", gy)]
        setProps viewport [("gliderDir", 0), ("dragStartX", cx), ("dragStartY", cy)]
        setProps
          viewport
          [("gliderAx", ax), ("gliderAy", ay), ("gliderCx", ax), ("gliderCy", ay)]
        setProp viewport "moved" (number 0)
  addEventListener "mouseup" canvas $ \e ->
    stmts $ handlePointerUp ui e
  addEventListener "mouseup" win $ \e ->
    stmts $ handlePointerUp ui e
  addEventListener "click" canvas $ \e ->
    stmts $ do
      moved <- getProp viewport "moved"
      whenS (moved .== 0) $ do
        ox <- eventOffsetX e
        oy <- eventOffsetY e
        (gx, gy) <- gridFromPointer ui ox oy
        applyClick ui gx gy
      _ <- setProp viewport "moved" (number 0)
      done
  addEventListener "mousemove" canvas $ \e ->
    stmts $
      handleCanvasMouseMove ui e
  addEventListener "mouseleave" canvas $ \_ ->
    stmts $ do
      _ <- setProp tipRef "over" (number 0)
      endPointer
  toSyntax_
    $ discard
    $ ffi
      ( "(canvas, h) => canvas.addEventListener('wheel', (e) => {"
          <> " e.preventDefault(); h(e);"
          <> " }, {passive: false})"
      )
      ( ArgEffect canvas
          <: ArgEffect
            ( LambdaE $ \(e :: f ('MutableObject ())) ->
                stmts $ do
                  delta <- getProp' (var e) "deltaY"
                  ox <- getProp' (var e) "offsetX"
                  oy <- getProp' (var e) "offsetY"
                  clientW <- getProp canvas "clientWidth"
                  let
                    bufScale = number canvasW / clientW
                    fx = ox * bufScale
                    fy = oy * bufScale
                  whenS (delta .!= 0) $ wheelZoomAt viewport delta fx fy
            )
          <: RecNil
      )
  toSyntax_ $ callMethod canvas "focus" RecNil
  done

hideTooltip :: Ui f -> EffectSyntax f (f 'Unit)
hideTooltip Ui {..} = do
  _ <- setProp tipRef "fp" (string "")
  _ <- setProp tipRef "shownGx" (number (-2))
  _ <- setProp tipRef "shownGy" (number (-2))
  _ <- Dom.setAttribute tooltip "aria-hidden" (string "true")
  Dom.setStyleProperty tooltip "visibility" (string "hidden")

syncPointerTip ::
  Ui f
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
syncPointerTip Ui {..} cx cy gx gy = do
  w <- getProp viewport "worldW"
  h <- getProp viewport "worldH"
  setProps tipRef [("cx", cx), ("cy", cy), ("gx", gx), ("gy", gy)]
  setProp
    tipRef
    "over"
    (if_ (gx .>= 0 .&& gy .>= 0 .&& gx .< w .&& gy .< h) (number 1) (number 0))

tickHover :: Ui f -> EffectSyntax f (f 'Unit)
tickHover ui@Ui {..} = do
  sid <- getProp toolRef "sid"
  ifS
    (isEraserToolSid sid)
    (hideTooltip ui)
    ( do
        over <- getProp tipRef "over"
        ifS
          (over .== 0)
          ( do
              fp <- getProp tipRef "fp"
              whenS (fp .!= string "") (hideTooltip ui)
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
                applyHover ui w h gx gy cx cy
          )
    )

-- | Grid index lookup, not a board-wide collision scan. A live cursor
--   cell is O(1). Empty cells search the Chebyshev square of
--   'hoverRadius' (25 cells at r=2). DOM writes only when the species
--   set changes: the tooltip does not follow the cursor inside a cell.
applyHover ::
  Ui f
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
applyHover ui@Ui {..} w h gx gy cx cy = do
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
    (hideTooltip ui)
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
  Ui f
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number, Expr f 'Number)
gridFromPointer Ui {..} localX localY = do
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
  forM_
    [ "dragging dragX dragY dragStartX dragStartY moved erasing rightPanning"
    , "panVelX panVelY panLastMs panInertiaLastMs"
    , "gliderAiming gliderGx gliderGy gliderDir gliderAx gliderAy gliderCx gliderCy"
    ]
    $ \ks -> setProps viewport [(k, 0) | k <- words ks]
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

gliderOrientationsLit :: forall f. Expr f ('Array ('Array ('Array 'Number)))
gliderOrientationsLit =
  Literal $
    Ts.ValueArray
      [ Ts.ValueArray
          ( map
              ( \(x, y) ->
                  Ts.ValueArray
                    [ Ts.ValueNumber (fromIntegral x)
                    , Ts.ValueNumber (fromIntegral y)
                    ]
              )
              cells
          )
      | cells <- gliderOrientationCells
      ]

-- | Screen Y is down. Indices: 0 SE, 1 NE, 2 NW, 3 SW.
gliderDirFromDrag :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
gliderDirFromDrag dx dy =
  if_
    (dx * dx + dy * dy .< number 64)
    (number 0)
    ( if_
        (dx .>= 0)
        (if_ (dy .< 0) (number 1) (number 0))
        (if_ (dy .< 0) (number 2) (number 3))
    )

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

clampZoomIndex :: Expr f ('Array 'Number) -> Expr f 'Number -> Expr f 'Number
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
  let
    cx = number (canvasW / 2)
    cy = number (canvasH / 2)
  stepZoomAt viewport delta cx cy

stepZoomAt ::
  Effect f ('MutableObject ())
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
stepZoomAt viewport delta fx fy = do
  levels <- getProp viewport "zoomLevels"
  indices <- getProp viewport "zoomIndices"
  z0 <- getProp viewport "zoom"
  let
    idx = nearestZoomIndex levels indices z0
    nextIdx = clampZoomIndex indices (idx + delta)
    z1 = Array.index levels nextIdx
  applyZoomAt viewport z0 z1 fx fy

applyZoomAt ::
  Effect f ('MutableObject ())
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
applyZoomAt viewport z0 z1 fx fy = do
  whenS (z1 .!= z0) $ do
    panX0 <- getProp viewport "panX"
    panY0 <- getProp viewport "panY"
    _ <- setProp viewport "zoom" z1
    _ <- setProp viewport "panX" (fx - (fx - panX0) * z1 / z0)
    _ <- setProp viewport "panY" (fy - (fy - panY0) * z1 / z0)
    clampPan viewport
    invalidateViewportRender viewport
    done

-- | Scroll wheel: continuous zoom toward cursor (trackpad-friendly).
wheelZoomAt ::
  Effect f ('MutableObject ())
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
wheelZoomAt viewport deltaY fx fy = do
  z0 <- getProp viewport "zoom"
  factor <- wheelZoomFactor deltaY
  let
    minZ = Array.index zoomLevelsLit (number 0)
    maxZ =
      Array.index zoomLevelsLit (number (fromIntegral (length zoomLevels - 1)))
    z1raw = z0 * factor
    z1 = Math.max minZ (Math.min maxZ z1raw)
  applyZoomAt viewport z0 z1 fx fy

wheelZoomFactor :: Expr f 'Number -> EffectSyntax f (Expr f 'Number)
wheelZoomFactor deltaY =
  fmap
    var
    ( toSyntax
        ( ffi
            ( "(d) => Math.exp(-d * "
                <> T.pack (show wheelZoomRate)
                <> ")"
            )
            (arg deltaY <: RecNil)
        )
    )

zoomIn :: Effect f ('MutableObject ()) -> EffectSyntax f (f 'Unit)
zoomIn viewport = stepZoom viewport (number 1)

zoomOut :: Effect f ('MutableObject ()) -> EffectSyntax f (f 'Unit)
zoomOut viewport = stepZoom viewport (number (-1))

invalidateViewportRender ::
  Effect f ('MutableObject ())
  -> EffectSyntax f (f 'Unit)
invalidateViewportRender viewport =
  setProp viewport "renderPanValid" false_

clampPan :: Effect f ('MutableObject ()) -> EffectSyntax f (f 'Unit)
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
  _ <- setProp toolRef "sid" (number (fromIntegral mouseToolSid))
  _ <-
    setProp toolRef "eraserRadius" (number (fromIntegral eraserDefaultRadius))
  pure toolRef

initDisturbCatalog ::
  EffectSyntax f (Effect f ('Map 'Number ('Array ('Array 'Number))))
initDisturbCatalog = buildDisturbMap

applyClick ::
  Ui f -> Expr f 'Number -> Expr f 'Number -> EffectSyntax f (f 'Unit)
applyClick Ui {..} gx gy = do
  sid <- getProp toolRef "sid"
  whenS (not_ (isNonPaintToolSid sid)) $ do
    hit <- Map.lookup toolsMap sid
    toSyntax $
      optionCaseE
        hit
        noOp
        (\cells -> fromSyntax $ placePattern state editScratch cells gx gy sid)

applyErase ::
  Ui f -> Expr f 'Number -> Expr f 'Number -> EffectSyntax f (f 'Unit)
applyErase Ui {..} gx gy = do
  radius0 <- getProp toolRef "eraserRadius"
  let
    radius = Math.floor radius0
  eraseCircle state editScratch gx gy radius

wireTools :: Ui f -> EffectSyntax f (f 'Unit)
wireTools ui@Ui {..} = do
  forRange_ (number 0) (Array.length toolBtnsE) $ \i -> do
    btn <- hold (expr (Array.index toolBtnsE i))
    addEventListener "click" btn $ \_ ->
      stmts $ do
        raw <- Dom.getAttribute btn "data-tool"
        selectTool ui (parseInt_ (orElse raw (string "0")) (number 10))
    done

selectTool :: Ui f -> Expr f 'Number -> EffectSyntax f (f 'Unit)
selectTool ui@Ui {..} sid = do
  _ <- setProp toolRef "sid" sid
  forRange_ (number 0) (Array.length toolBtnsE) $ \i -> do
    btn <- hold (expr (Array.index toolBtnsE i))
    raw <- Dom.getAttribute btn "data-tool"
    let
      on = parseInt_ (orElse raw (string "0")) (number 10) .== sid
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
  syncEraserUi ui

syncEraserUi :: Ui f -> EffectSyntax f (f 'Unit)
syncEraserUi ui@Ui {..} = do
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
  syncToolCursor ui

syncToolCursor :: Ui f -> EffectSyntax f (f 'Unit)
syncToolCursor Ui {..} = do
  sid <- getProp toolRef "sid"
  dragging <- getProp viewport "dragging"
  rightPanning <- getProp viewport "rightPanning"
  ifS
    (isMouseToolSid sid)
    ( ifS
        (dragging .== 1 .|| rightPanning .== 1)
        (Dom.setStyleProperty canvas "cursor" (string "grabbing"))
        (Dom.setStyleProperty canvas "cursor" (string "grab"))
    )
    ( ifS
        (isEraserToolSid sid)
        (Dom.setStyleProperty canvas "cursor" eraserCursor)
        (Dom.setStyleProperty canvas "cursor" (string "crosshair"))
    )

tickPanInertia ::
  Effect f ('MutableObject ())
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
tickPanInertia viewport now = do
  inertiaLastMs <- getProp viewport "panInertiaLastMs"
  ifS
    (inertiaLastMs .== 0)
    (setProp viewport "panInertiaLastMs" now)
    ( do
        rightPanning <- getProp viewport "rightPanning"
        whenS (rightPanning .== 0) $ do
          let
            dt = Math.min ((now - inertiaLastMs) / number 1000) (number 0.05)
          velX <- getProp viewport "panVelX"
          velY <- getProp viewport "panVelY"
          let
            speed = Math.hypot velX velY
          ifS
            (speed .> number 0.5)
            ( do
                panX <- getProp viewport "panX"
                panY <- getProp viewport "panY"
                _ <- setProp viewport "panX" (panX + velX * dt)
                _ <- setProp viewport "panY" (panY + velY * dt)
                _ <- setProp viewport "panVelX" (velX * number 0.92)
                _ <- setProp viewport "panVelY" (velY * number 0.92)
                clampPan viewport
                invalidateViewportRender viewport
            )
            ( do
                _ <- setProp viewport "panVelX" (number 0)
                setProp viewport "panVelY" (number 0)
            )
        setProp viewport "panInertiaLastMs" now
    )

wireEraserSize :: Ui f -> EffectSyntax f (f 'Unit)
wireEraserSize Ui {..} = do
  addEventListener "input" eraserRadius $ \_ ->
    stmts $ do
      raw <- Dom.getValue eraserRadius
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
      _ <- Dom.setValue eraserRadius label
      _ <- Dom.setAttribute eraserRadius "aria-valuenow" label
      Dom.setTextContent eraserRadiusVal label
      done
  done

-- | Clear and hide the 2D ghost overlay. Hiding matters: a visible canvas
--   stacked over the WebGL board occlusion-culls the board's WebGL quad in
--   software-composited browsers, blanking the whole game.
clearEraserGhostStm :: El f -> EffectSyntax f (f 'Unit)
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
  El f
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

tickGliderGhost :: Ui f -> EffectSyntax f (f 'Unit)
tickGliderGhost Ui {..} = do
  sid <- getProp toolRef "sid"
  aiming <- getProp viewport "gliderAiming"
  app <- getProp viewport "app"
  ifS
    ( sid
        .== number (fromIntegral gliderToolSid)
        .&& aiming
        .== 1
    )
    ( do
        gx <- getProp viewport "gliderGx"
        gy <- getProp viewport "gliderGy"
        dir <- getProp viewport "gliderDir"
        cx <- getProp viewport "gliderCx"
        cy <- getProp viewport "gliderCy"
        panX <- getProp viewport "panX"
        panY <- getProp viewport "panY"
        zoom <- getProp viewport "zoom"
        let
          px = number (fromIntegral cellPx)
          cells = Array.index gliderOrientationsLit dir
        Pixi.drawGliderGhost app viewport cells gx gy cx cy panX panY zoom px
    )
    ( whenS (sid .== number (fromIntegral gliderToolSid)) $
        Pixi.clearEraserGhost app viewport
    )

tickEraserGhost :: Ui f -> EffectSyntax f (f 'Unit)
tickEraserGhost Ui {..} = do
  sid <- getProp toolRef "sid"
  glLost <- getProp viewport "glLost"
  app <- getProp viewport "app"
  let
    clearGhosts = do
      clearEraserGhostStm eraserGhost
      Pixi.clearEraserGhost app viewport
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
                    clearEraserGhostStm eraserGhost
                    Pixi.drawEraserGhost app viewport alive w h gx gy radius panX panY zoom px
                )
                ( do
                    Pixi.clearEraserGhost app viewport
                    drawEraserGhostStm eraserGhost alive w h gx gy radius panX panY zoom px
                )
          )
          clearGhosts
    )
    ( do
        aiming <- getProp viewport "gliderAiming"
        ifS (aiming .== 1) done clearGhosts
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
  El f
  -> El f
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

wireSettings :: Ui f -> EffectSyntax f (f 'Unit)
wireSettings Ui {..} = do
  addEventListener "click" settingsZoomIn $ \_ -> stmts (zoomIn viewport *> done)
  addEventListener "click" settingsZoomOut $ \_ -> stmts (zoomOut viewport *> done)
  addEventListener "click" settingsReset $ \_ -> stmts (resetViewport viewport *> done)
  done

wirePurgeDiscoveries :: Ui f -> EffectSyntax f (f 'Unit)
wirePurgeDiscoveries Ui {..} = do
  addEventListener "click" settingsPurge $ \_ ->
    stmts $ do
      now <- performanceNow
      purgeEmergentDiscoveries state viewport IndexUi {..} now
      done
  done

wireSimSettings :: Ui f -> EffectSyntax f (f 'Unit)
wireSimSettings Ui {..} = do
  addEventListener "change" settingsGrid $ \_ ->
    stmts $ do
      raw <- Dom.getValue settingsGrid
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
  addEventListener "input" settingsTick $ \_ ->
    stmts $ do
      raw <- Dom.getValue settingsTick
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
        (Dom.setTextContent settingsTickVal (string "max"))
        ( do
            _ <-
              Dom.setTextContent
                settingsTickVal
                (toString (Math.round ms) <> string " ms")
            done
        )
      _ <- Dom.setAttribute settingsTick "aria-valuenow" (toString ms)
      done
  done

resetViewport :: Effect f ('MutableObject ()) -> EffectSyntax f (f 'Unit)
resetViewport viewport = do
  w <- getProp viewport "worldW"
  h <- getProp viewport "worldH"
  let
    px = number (fromIntegral cellPx)
  _ <- setProp viewport "zoom" (number 1)
  _ <- setProp viewport "panX" (number (canvasW / 2) - (w / number 2) * px)
  _ <- setProp viewport "panY" (number (canvasH / 2) - (h / number 2) * px)
  _ <- setProp viewport "panVelX" (number 0)
  _ <- setProp viewport "panVelY" (number 0)
  clampPan viewport
  invalidateViewportRender viewport

syncPauseOverlay :: Ui f -> EffectSyntax f (f 'Unit)
syncPauseOverlay Ui {..} = do
  paused <- state.paused
  toSyntax_ $
    callMethod
      pauseOverlay
      "classList.toggle"
      (arg (string "is-visible") <: arg paused <: RecNil)
  Dom.setAttribute
    pauseOverlay
    "aria-hidden"
    (if_ paused (string "false") (string "true"))

updateHud :: Ui f -> Expr f 'Number -> EffectSyntax f (f 'Unit)
updateHud Ui {..} renderMs = do
  gen <- state.gen
  pop <- state.pop
  fpsN <- meter.fps
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
      statRender
      (toString (Math.round renderMs) <> string "ms")
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

stepLifeFrame :: Ui f -> Expr f 'Number -> EffectSyntax f (f 'Unit)
stepLifeFrame Ui {..} frameStart = do
  interval <- state.tickMs
  lastStep <- getProp viewport "lastStepMs"
  let
    due = lastStep .< number 0 .|| frameStart - lastStep .>= interval
  whenS due $ do
    _ <- setProp viewport "lastStepMs" frameStart
    stepLife state registry stepCtx

tickIndex :: Ui f -> Expr f 'Number -> EffectSyntax f (f 'Unit)
tickIndex Ui {..} now = do
  pending <- getProp indexTracker "pending"
  indexLastMs <- getProp indexTracker "lastMs"
  let
    refresh = number (fromIntegral indexRefreshMs)
  whenS (not_ pending .&& (indexLastMs .== 0 .|| now - indexLastMs .>= refresh)) $ do
    alive <- state.alive
    species <- state.species
    palette <- state.palette
    x0 <- state.boundX0
    y0 <- state.boundY0
    x1 <- state.boundX1
    y1 <- state.boundY1
    worldW <- state.worldW
    worldH <- state.worldH
    stepIndexTracker Scan {..} IndexUi {..} now
