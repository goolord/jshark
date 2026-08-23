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
import Grid (cellIdx, createImageData, u8Get)
import JShark.Api
import qualified JShark.Array as Array
import qualified JShark.Canvas as Canvas
import qualified JShark.Dom as Dom
import JShark.Generic (MutableObjectOf)
import qualified JShark.Generic as G
import qualified JShark.Json as Json
import qualified JShark.Map as Map
import qualified JShark.Math as Math
import JShark.Rec (Rec (..), (<:))
import qualified JShark.Set as Set
import qualified JShark.Timers as Timers
import JShark.Types (Effect (Lift), Expr (Var))
import Names (lookupDisplayName)
import Patterns (disturbCatalogJson)
import Types
  ( LifeState
  , boardId
  , canvasH
  , canvasW
  , cellPx
  , gridH
  , gridW
  , hoverRadius
  , ink
  , lifeTooltipId
  , lifeTooltipNameId
  , lifeTooltipSwatchId
  , toggleToolSid
  )

data Fps = Fps
  { lastMs :: Double
  , fps :: Double
  }
  deriving Generic

mainJS :: forall f. EffectSyntax f (f 'Unit)
mainJS = do
  canvas <- Dom.lookupId (string boardId)
  ctxOpt <- Canvas.getContext2d canvas
  whenSomeE ctxOpt $ \ctx -> boot canvas ctx

boot ::
  Effect f ('MutableObject Dom.DomElement)
  -> Expr f ('MutableObject Canvas.Context2D)
  -> EffectSyntax f (f 'Unit)
boot canvas ctx = do
  ctxH <- hold (expr ctx)
  _ <- Canvas.setCanvasWidth canvas (number canvasW)
  _ <- Canvas.setCanvasHeight canvas (number canvasH)
  _ <- setProp ctxH "imageSmoothingEnabled" false_
  state <- initLife ctxH
  registry <- initRegistry
  indexTracker <- initIndexTracker
  seenSpecies <- initSeenSpecies
  typesList <- initIndexContainer
  tooltip <- Dom.lookupId (string lifeTooltipId)
  swatchEl <- Dom.lookupId (string lifeTooltipSwatchId)
  nameEl <- Dom.lookupId (string lifeTooltipNameId)
  img <- bindExpr =<< createImageData ctxH (number canvasW) (number canvasH)
  meter <- hold (G.toObject (Fps (-1) 0))
  rectSym <- toSyntax emptyObject
  let
    rectRef = Lift (Var rectSym)
  tipSym <- toSyntax emptyObject
  let
    tipRef = Lift (Var tipSym)
  hitsSym <- toSyntax Set.new
  let
    hits = Lift (Var hitsSym)
  toolRef <- initTool
  toolsMap <- initDisturbCatalog
  toolBtns <- Dom.lookupSelector (string ".life-tool")
  toolBtnsE <- bindExpr toolBtns
  wire canvas state tooltip rectRef tipRef toolRef toolsMap
  wireTools toolRef toolBtnsE
  Timers.foreverFrame $ \now -> do
    tickFps meter now
    paused <- state.paused
    whenS (not_ paused) (stepLife state registry)
    tickIndex state registry indexTracker seenSpecies typesList now
    renderLife ctxH img state
    paintHud ctxH state meter
    tickHover tipRef state registry tooltip swatchEl nameEl hits

wire ::
  Effect f ('MutableObject Dom.DomElement)
  -> Effect f (MutableObjectOf LifeState)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject ())
  -> Effect f ('MutableObject ())
  -> Effect f ('MutableObject ())
  -> Effect f ('Map 'Number ('Array ('Array 'Number)))
  -> EffectSyntax f (f 'Unit)
wire canvas state tooltip rectRef tipRef toolRef toolsMap = do
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
  let
    refreshRect = do
      r <- hold $ callMethod canvas "getBoundingClientRect" RecNil
      left <- getProp r "left"
      top <- getProp r "top"
      _ <- setProp rectRef "left" left
      _ <- setProp rectRef "top" top
      done
  win <- hold window
  addEventListener "mouseenter" canvas $ \_ -> stmts refreshRect
  addEventListener "resize" win $ \_ -> stmts refreshRect
  _ <- refreshRect
  addEventListener "keydown" canvas $ \(e :: Expr f ('MutableObject ())) ->
    stmts $ do
      code <- getProp' e "code"
      toSyntax $
        stringCaseE
          code
          [
            ( "Space"
            , discard $
                stmts $ do
                  toSyntax_ $ callMethod (expr e) "preventDefault" RecNil
                  togglePause state
                  done
            )
          ]
          noOp
  addEventListener "click" canvas $ \(e :: Expr f ('MutableObject ())) ->
    stmts $ do
      cx <- getProp' e "clientX"
      cy <- getProp' e "clientY"
      (gx, gy) <- gridFromClient rectRef cx cy
      applyClick state toolRef toolsMap gx gy
  addEventListener "mousemove" canvas $ \(e :: Expr f ('MutableObject ())) ->
    stmts $ do
      cx <- getProp' e "clientX"
      cy <- getProp' e "clientY"
      (gx, gy) <- gridFromClient rectRef cx cy
      let
        w = number (fromIntegral gridW)
        h = number (fromIntegral gridH)
      _ <- setProp tipRef "cx" cx
      _ <- setProp tipRef "cy" cy
      _ <- setProp tipRef "gx" gx
      _ <- setProp tipRef "gy" gy
      setProp
        tipRef
        "over"
        (if_ (gx .>= 0 .&& gy .>= 0 .&& gx .< w .&& gy .< h) (number 1) (number 0))
  addEventListener "mouseleave" canvas $ \_ ->
    stmts $ setProp tipRef "over" (number 0)
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

tickHover ::
  Effect f ('MutableObject ())
  -> Effect f (MutableObjectOf LifeState)
  -> Effect f ('MutableObject Registry)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Effect f ('Set Number)
  -> EffectSyntax f (f 'Unit)
tickHover tipRef state registry tooltip swatchEl nameEl hits = do
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
          let
            w = number (fromIntegral gridW)
            h = number (fromIntegral gridH)
          applyHover tipRef state registry tooltip swatchEl nameEl hits w h gx gy cx cy
    )

-- | Grid index lookup, not a board-wide collision scan. A live cursor
--   cell is O(1). Empty cells search the Chebyshev square of
--   'hoverRadius' (25 cells at r=2). DOM writes only when the species
--   set changes — the tooltip does not follow the cursor inside a cell.
applyHover ::
  Effect f ('MutableObject ())
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
applyHover tipRef state registry tooltip swatchEl nameEl hits w h gx gy cx cy = do
  alive <- state.alive
  species <- state.species
  pal <- state.palette
  _ <- Set.clear hits
  let
    i = cellIdx w gx gy
  a <- u8Get alive i
  ifS
    (a .== 1)
    ( do
        sid <- u8Get species i
        _ <- Set.insert hits sid
        setProp tipRef "swatchSid" sid
    )
    (collectNearby alive species w h gx gy hits tipRef)
  n <- Set.size hits
  ifS
    (n .== 0)
    (hideTooltip tipRef tooltip)
    ( do
        sids <- bindExpr $ Array.fromEffects []
        _ <- Set.mapM_ (\sid -> Array.push_ sids sid) hits
        sortedSids <- bindExpr $ Array.sort sids (\x y -> x - y)
        _ <- setProp tipRef "fpBuild" (string "")
        _ <- setProp tipRef "label" (string "")
        _ <-
          forRange_ (number 0) (Array.length sortedSids) $ \idx -> do
            sid <- pure (Array.index sortedSids idx)
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
      aj <- u8Get alive j
      whenS (aj .== 1) $ do
        let
          dist = dx * dx + dy * dy
        sid <- u8Get species j
        best <- getProp tipRef "best"
        whenS (dist .< best) $ do
          _ <- setProp tipRef "best" dist
          _ <- setProp tipRef "swatchSid" sid
          _ <- Set.clear hits
          _ <- Set.insert hits sid
          done
        whenS (dist .== best) $ Set.insert hits sid
        done
      done
    done
  done

gridFromClient ::
  Effect f ('MutableObject ())
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'Number, Expr f 'Number)
gridFromClient rectRef cx cy = do
  left <- getProp rectRef "left"
  top <- getProp rectRef "top"
  pure
    ( Math.floor ((cx - left) / number (fromIntegral cellPx))
    , Math.floor ((cy - top) / number (fromIntegral cellPx))
    )

initTool :: EffectSyntax f (Effect f ('MutableObject ()))
initTool = do
  toolRef <- hold newObject
  _ <- setProp toolRef "sid" (number (fromIntegral toggleToolSid))
  pure toolRef

initDisturbCatalog ::
  EffectSyntax f (Effect f ('Map 'Number ('Array ('Array 'Number))))
initDisturbCatalog = do
  pairs <- bindExpr $ Json.unsafeParse (string disturbCatalogJson)
  hold $ Map.fromEntries pairs

applyClick ::
  Effect f (MutableObjectOf LifeState)
  -> Effect f ('MutableObject ())
  -> Effect f ('Map 'Number ('Array ('Array 'Number)))
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
applyClick state toolRef toolsMap gx gy = do
  sid <- getProp toolRef "sid"
  ifS
    (sid .== number (fromIntegral toggleToolSid))
    (flipCell state gx gy)
    ( do
        hit <- Map.lookup toolsMap sid
        toSyntax $
          optionCaseE
            hit
            noOp
            (\cells -> fromSyntax $ placePattern state cells gx gy sid)
    )

wireTools ::
  Effect f ('MutableObject ())
  -> Expr f ('Array ('MutableObject Dom.DomElement))
  -> EffectSyntax f (f 'Unit)
wireTools toolRef btns = do
  forRange_ (number 0) (Array.length btns) $ \i -> do
    btn <- hold (expr (Array.index btns i))
    addEventListener "click" btn $ \_ ->
      stmts $ do
        raw <- Dom.getAttribute btn "data-tool"
        selectTool toolRef btns (parseInt_ raw (number 10))
    done

selectTool ::
  Effect f ('MutableObject ())
  -> Expr f ('Array ('MutableObject Dom.DomElement))
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
selectTool toolRef btns sid = do
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

paintHud ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Effect f (MutableObjectOf LifeState)
  -> Effect f (MutableObjectOf Fps)
  -> EffectSyntax f (f 'Unit)
paintHud ctx state meter = do
  gen <- state.gen
  pop <- state.pop
  paused <- state.paused
  fpsN <- meter.fps
  set @"font" ctx (string "15px Georgia")
  fill ctx (string ink)
  _ <-
    Canvas.fillText
      ctx
      (string "Gen: " <> toString gen <> string "  Cells: " <> toString pop)
      8
      18
  set @"textAlign" ctx (string "center")
  _ <-
    Canvas.fillText ctx (string "FPS: " <> toString fpsN) (number (canvasW / 2)) 18
  set @"textAlign" ctx (string "right")
  ifS
    paused
    (Canvas.fillText ctx (string "paused") (number (canvasW - 8)) 18)
    (Canvas.fillText ctx (string "running") (number (canvasW - 8)) 18)
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

tickIndex ::
  Effect f (MutableObjectOf LifeState)
  -> Effect f ('MutableObject Registry)
  -> Effect f ('MutableObject IndexTracker)
  -> Effect f ('Set Number)
  -> Effect f ('MutableObject Dom.DomElement)
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
tickIndex state registry tracker seen listEl now = do
  alive <- state.alive
  species <- state.species
  pal <- state.palette
  stepIndexTracker alive species pal registry tracker seen listEl now

fill ::
  Effect f ('MutableObject Canvas.Context2D)
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
fill ctx col = set @"fillStyle" ctx col
