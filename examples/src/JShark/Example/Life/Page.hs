{-# LANGUAGE OverloadedStrings #-}

module JShark.Example.Life.Page (page, framePage, frameSrcFor, assetBaseFor, sourceSrcFor) where

import qualified Data.Text as T
import JShark.Example.Life.Names (patternLabel)
import JShark.Example.Life.Patterns
  ( PatternSpec (..)
  , disturbPatterns
  , glider
  )
import JShark.Example.Life.Types
  ( boardId
  , canvasH
  , canvasW
  , eraserDefaultRadius
  , eraserMaxRadius
  , eraserMinRadius
  , eraserToolSid
  , gliderToolSid
  , gridH
  , gridSizePresets
  , gridW
  , lifeBoard2dId
  , lifeDebugCollapseId
  , lifeDebugId
  , lifeEraserGhostId
  , lifeEraserRadiusId
  , lifeEraserRadiusValId
  , lifeEraserSizeId
  , lifeIndexHostId
  , lifeIndexTotalId
  , lifePauseLabelId
  , lifePauseOverlayId
  , lifeSettingsCollapseId
  , lifeSettingsGridId
  , lifeSettingsId
  , lifeSettingsPurgeId
  , lifeSettingsResetId
  , lifeSettingsTickId
  , lifeSettingsTickValId
  , lifeSettingsZoomId
  , lifeSettingsZoomInId
  , lifeSettingsZoomOutId
  , lifeStatCellsId
  , lifeStatFpsId
  , lifeStatGenId
  , lifeStatRenderId
  , lifeStatZoomId
  , lifeToolsCollapseId
  , lifeToolsId
  , lifeTooltipId
  , lifeTooltipNameId
  , lifeTooltipSwatchId
  , lifeTypesListId
  , mouseToolSid
  , tickDefaultMs
  , tickMaxMs
  , tickMinMs
  , tickStepMs
  )
import JShark.Example.Theme (githubCorner, sourceLinks, themeLinks)
import Lucid
import Lucid.Base (makeAttribute)

-- | Shell page. Game runs in an iframe at @frameSrc@ (HTTP, not blob) so
--   Firefox can fetch @app.js@, wasm, and workers under CORP headers.
--   The frame is deliberately NOT sandboxed: @sandbox=\"allow-scripts\"@
--   gives the frame an opaque origin, and browsers that isolate sandboxed
--   frames into their own process (Chromium/Electron) fail to bring up the
--   WebGL renderer there: the sim ticks but the board stays black.
--   If WebGL is unavailable in the frame the game falls back to a 2D canvas.
page :: T.Text -> T.Text -> Html ()
page staticRoot frameSrc = doctypehtml_ $
  html_ [makeAttribute "data-theme" "dark"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      title_ "Life"
      themeLinks staticRoot
      link_ [rel_ "stylesheet", href_ (staticRoot <> "/css/life-shell.css")]
    body_ [class_ "life-shell"] $ do
      iframe_
        [ id_ "life-frame"
        , class_ "life-frame"
        , title_ "Game of Life"
        , src_ frameSrc
        ]
        mempty
      script_ focusFrameScript

-- | Inner document served at @/<example>/frame/@.
framePage :: T.Text -> T.Text -> T.Text -> Html ()
framePage staticRoot scriptSrc assetBase = gameDocument staticRoot scriptSrc assetBase

-- | @/life/app.js@ → @/life/frame/@; export @app.js@ → @frame/@.
frameSrcFor :: T.Text -> T.Text
frameSrcFor script =
  if script == "app.js"
    then "frame/"
    else T.take (T.length script - 6) script <> "frame/"

-- | Base href for assets (@js/@, @app.js@) inside the frame document.
assetBaseFor :: T.Text -> T.Text
assetBaseFor script =
  if script == "app.js"
    then "../"
    else T.take (T.length script - 6) script

-- | @/life/app.js@ → @/life/source.js@; export @app.js@ → @source.js@
--   (resolved via the frame @<base href>@).
sourceSrcFor :: T.Text -> T.Text
sourceSrcFor script =
  T.take (T.length script - 6) script <> "source.js"

focusFrameScript :: T.Text
focusFrameScript =
  "(()=>{const f=document.getElementById('life-frame');"
    <> "Object.defineProperty(window,'__jsharkLifeProfile',{"
    <> "get(){try{return f.contentWindow&&f.contentWindow.__jsharkLifeProfile}"
    <> "catch(_){return undefined}},configurable:true});"
    <> "f.addEventListener('load',function(){this.focus();},{once:true});"
    <> "})();"

gameDocument :: T.Text -> T.Text -> T.Text -> Html ()
gameDocument staticRoot scriptSrc assetBase = doctypehtml_ $
  html_ [makeAttribute "data-theme" "dark"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      title_ "Life"
      base_ [href_ assetBase]
      themeLinks staticRoot
      link_ [rel_ "stylesheet", href_ (staticRoot <> "/css/life.css")]
      link_ [rel_ "stylesheet", href_ (staticRoot <> "/css/life-tool-preview.css")]
      sourceLinks staticRoot
    body_ $ do
      githubCorner
      main_ $ do
        header_ [class_ "life-header"] $ do
          h1_ "Life"
          p_ [class_ "life-meta"] $ do
            toHtml (T.pack (show gridW))
            "×"
            toHtml (T.pack (show gridH))
            " · Conway's Game of Life sandbox"
        div_ [class_ "life-stage"] $ do
          canvas_
            [ id_ boardId
            , width_ (T.pack (show (round canvasW :: Int)))
            , height_ (T.pack (show (round canvasH :: Int)))
            , tabindex_ "0"
            , autofocus_
            ]
            mempty
          canvas_
            [ id_ lifeBoard2dId
            , class_ "life-board-2d"
            , width_ (T.pack (show (round canvasW :: Int)))
            , height_ (T.pack (show (round canvasH :: Int)))
            , makeAttribute "aria-hidden" "true"
            ]
            mempty
          -- Hidden unless the 2D fallback needs it: a visible canvas stacked
          -- over the WebGL board occlusion-culls the board's quad in
          -- software-composited browsers, blanking the whole game.
          canvas_
            [ id_ lifeEraserGhostId
            , class_ "life-eraser-ghost"
            , width_ (T.pack (show (round canvasW :: Int)))
            , height_ (T.pack (show (round canvasH :: Int)))
            , makeAttribute "aria-hidden" "true"
            ]
            mempty
          div_
            [ id_ lifePauseOverlayId
            , class_ "life-pause-overlay"
            , makeAttribute "aria-hidden" "true"
            ]
            $ span_
              [ id_ lifePauseLabelId
              , class_ "life-pause-label"
              ]
              "paused"
          debugMenu
          settingsMenu
          toolsHud
        ul_ [class_ "life-hints"] $ do
          li_ "Esc pause · Scroll or +/− zoom · Shift-drag pan · empty click pauses"
        div_ [id_ lifeTooltipId, class_ "life-tooltip", role_ "tooltip"] $ do
          span_ [id_ lifeTooltipSwatchId, class_ "life-tooltip-swatch"] mempty
          span_ [id_ lifeTooltipNameId, class_ "life-tooltip-name"] mempty
        section_ [class_ "life-index"] $ do
          div_ [class_ "life-index-head"] $ do
            h2_ "Biomass Index"
            span_ [id_ lifeIndexTotalId, class_ "life-index-total"] mempty
          div_ [id_ lifeIndexHostId] $
            div_ [id_ lifeTypesListId, class_ "life-index-grid"] mempty
      lifeSourceSection
      script_ [src_ "js/pixi.min.js"] ("" :: Html ())
      script_ [src_ scriptSrc] ("" :: Html ())
      script_ [type_ "module", src_ (staticRoot <> "/js/source-pane.js")] ("" :: Html ())
      sourceLoadScript (sourceSrcFor scriptSrc)

lifeSourceSection :: Html ()
lifeSourceSection =
  details_ [class_ "life-source"] $ do
    summary_ [class_ "life-source-summary"] $ do
      span_ [class_ "life-source-summary-inner"] $ do
        span_ [class_ "life-source-title"] "Source"
        span_ [class_ "life-source-expand-hint"] "click to expand"
      button_
        [ type_ "button"
        , class_ "js-source-copy life-source-copy"
        , makeAttribute "aria-label" "Copy generated JavaScript"
        , makeAttribute "disabled" "true"
        ]
        "Copy"
    pre_ [class_ "life-source-pre"] $
      code_ [class_ "shj-lang-js life-source-code"] "Expand to load source…"

sourceLoadScript :: T.Text -> Html ()
sourceLoadScript sourceSrc =
  script_ $
    "(function(){"
      <> "var pane=document.querySelector('.life-source');"
      <> "var code=document.querySelector('.life-source-code');"
      <> "var btn=document.querySelector('.life-source-copy');"
      <> "if(!pane||!code)return;"
      <> "var loaded=false;"
      <> "function load(){"
      <> "if(loaded)return;"
      <> "loaded=true;"
      <> "code.textContent='Loading source…';"
      <> "fetch("
      <> jsString sourceSrc
      <> ")"
      <> ".then(function(r){if(!r.ok)throw new Error('fetch');return r.text();})"
      <> ".then(function(t){"
      <> "code.textContent=t;"
      <> "delete code.dataset.highlighted;"
      <> "if(btn)btn.removeAttribute('disabled');"
      <> "if(window.jsharkWhenHighlightReady){"
      <> "window.jsharkWhenHighlightReady(function(h){h(code);});"
      <> "}else if(window.jsharkHighlightCode){"
      <> "window.jsharkHighlightCode(code);"
      <> "}"
      <> "})"
      <> ".catch(function(){"
      <> "code.textContent='// Failed to load generated source';"
      <> "});"
      <> "}"
      <> "pane.addEventListener('toggle',function(){if(pane.open)load();});"
      <> "})();"

-- | JS string literal safe to embed in a @<script>@ (no raw @<@ / line separators).
jsString :: T.Text -> T.Text
jsString t =
  T.cons '"' (T.concatMap escape t <> "\"")
 where
  escape c = case c of
    '\\' -> "\\\\"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    '<' -> "\\u003c"
    '\x2028' -> "\\u2028"
    '\x2029' -> "\\u2029"
    _ -> T.singleton c

debugRow :: T.Text -> T.Text -> Bool -> Html ()
debugRow valId label wide =
  div_
    ( [class_ "life-debug-row"]
        <> [class_ "life-debug-row-wide" | wide]
    )
    $ do
      span_ [class_ "dbg-k"] (toHtml label)
      span_ [id_ valId, class_ "dbg-v"] "0"

debugMenu :: Html ()
debugMenu =
  div_
    [ id_ lifeDebugId
    , class_ "life-panel life-debug is-collapsed"
    , role_ "region"
    , makeAttribute "aria-label" "Stats"
    ]
    $ do
      button_
        [ id_ lifeDebugCollapseId
        , type_ "button"
        , class_ "life-panel-toggle"
        , makeAttribute "aria-expanded" "false"
        , makeAttribute "aria-label" "Expand stats"
        , title_ "Stats"
        ]
        "Stats"
      div_
        [ class_ "life-panel-body"
        , role_ "status"
        , makeAttribute "aria-live" "polite"
        ]
        $ do
          debugRow lifeStatGenId "Gen" False
          debugRow lifeStatCellsId "Cells" False
          debugRow lifeStatFpsId "FPS" False
          debugRow lifeStatZoomId "Zoom" False
          debugRow lifeStatRenderId "Render" True

settingsMenu :: Html ()
settingsMenu =
  div_
    [ id_ lifeSettingsId
    , class_ "life-panel life-settings"
    , role_ "region"
    , makeAttribute "aria-label" "Settings"
    ]
    $ do
      button_
        [ id_ lifeSettingsCollapseId
        , type_ "button"
        , class_ "life-panel-toggle"
        , makeAttribute "aria-expanded" "true"
        , makeAttribute "aria-label" "Collapse settings"
        , title_ "Settings"
        ]
        "Settings"
      div_ [class_ "life-panel-body"] $ do
        div_ [class_ "life-settings-row"] $ do
          span_ [class_ "life-settings-label"] "Zoom"
          span_ [id_ lifeSettingsZoomId, class_ "life-settings-value"] "100%"
        div_ [class_ "life-settings-row life-settings-actions"] $ do
          button_
            [ id_ lifeSettingsZoomOutId
            , type_ "button"
            , class_ "life-settings-btn"
            , makeAttribute "aria-label" "Zoom out"
            ]
            "−"
          button_
            [ id_ lifeSettingsZoomInId
            , type_ "button"
            , class_ "life-settings-btn"
            , makeAttribute "aria-label" "Zoom in"
            ]
            "+"
        button_
          [ id_ lifeSettingsResetId
          , type_ "button"
          , class_ "life-settings-btn life-settings-reset"
          ]
          "Reset view"
        button_
          [ id_ lifeSettingsPurgeId
          , type_ "button"
          , class_ "life-settings-btn life-settings-purge"
          , makeAttribute "aria-label" "Reset all species labels except manual stamps"
          ]
          "Reset all labels"
        label_ [class_ "life-settings-row", for_ lifeSettingsGridId] $ do
          span_ [class_ "life-settings-label"] "Grid"
          select_
            [ id_ lifeSettingsGridId
            , class_ "life-settings-select"
            , makeAttribute "aria-label" "Simulation grid size"
            ]
            $ mapM_ gridSizeOption gridSizePresets
        label_ [class_ "life-settings-row", for_ lifeSettingsTickId] $ do
          span_ [class_ "life-settings-label"] "Tick"
          span_ [id_ lifeSettingsTickValId, class_ "life-settings-value"] "max"
        input_
          [ id_ lifeSettingsTickId
          , type_ "range"
          , class_ "life-settings-range"
          , makeAttribute "min" (T.pack (show tickMinMs))
          , makeAttribute "max" (T.pack (show tickMaxMs))
          , makeAttribute "step" (T.pack (show tickStepMs))
          , makeAttribute "value" (T.pack (show tickDefaultMs))
          , makeAttribute "aria-valuemin" (T.pack (show tickMinMs))
          , makeAttribute "aria-valuemax" (T.pack (show tickMaxMs))
          , makeAttribute "aria-valuenow" (T.pack (show tickDefaultMs))
          , makeAttribute "aria-label" "Tick interval"
          ]

gridSizeOption :: (Int, Int) -> Html ()
gridSizeOption (w, h) =
  let
    val = T.pack (show w <> "x" <> show h)
    lab = T.pack (show w <> "×" <> show h)
    sel = w == gridW && h == gridH
   in
    option_
      ( [value_ val]
          <> [selected_ "selected" | sel]
      )
      (toHtml lab)

toolsHud :: Html ()
toolsHud =
  div_
    [ id_ lifeToolsId
    , class_ "life-tools"
    , role_ "toolbar"
    , makeAttribute "aria-label" "Placement tools"
    ]
    $ do
      button_
        [ id_ lifeToolsCollapseId
        , type_ "button"
        , class_ "life-tools-collapse"
        , makeAttribute "aria-expanded" "true"
        , makeAttribute "aria-label" "Collapse tools"
        , title_ "Collapse tools"
        ]
        "−"
      div_ [class_ "life-tools-body"] $ do
        toolButton mouseToolSid "Mouse" [(1, 1)] (Just (3, 3)) True
        toolButton gliderToolSid "Glider" glider (Just (3, 3)) False
        toolButton eraserToolSid "Eraser" [(1, 1)] (Just (3, 3)) False
        mapM_ disturbButton disturbPatterns
      div_
        [ id_ lifeEraserSizeId
        , class_ "life-eraser-size"
        , hidden_ ""
        , makeAttribute "aria-hidden" "true"
        ]
        $ do
          label_
            [ for_ lifeEraserRadiusId
            , class_ "life-eraser-size-label"
            ]
            $ do
              "Brush "
              input_
                [ id_ lifeEraserRadiusId
                , type_ "range"
                , class_ "life-eraser-radius"
                , makeAttribute "min" (T.pack (show eraserMinRadius))
                , makeAttribute "max" (T.pack (show eraserMaxRadius))
                , makeAttribute "step" "1"
                , makeAttribute "value" (T.pack (show eraserDefaultRadius))
                , makeAttribute "aria-valuemin" (T.pack (show eraserMinRadius))
                , makeAttribute "aria-valuemax" (T.pack (show eraserMaxRadius))
                , makeAttribute "aria-valuenow" (T.pack (show eraserDefaultRadius))
                , makeAttribute "aria-label" "Eraser brush size"
                ]
          span_
            [ id_ lifeEraserRadiusValId
            , class_ "life-eraser-radius-val"
            , makeAttribute "aria-hidden" "true"
            ]
            (toHtml (T.pack (show eraserDefaultRadius)))

disturbButton :: PatternSpec -> Html ()
disturbButton p =
  toolButton (patId p) (patternLabel (patId p)) (patCells p) Nothing False

toolButton ::
  Int -> T.Text -> [(Int, Int)] -> Maybe (Int, Int) -> Bool -> Html ()
toolButton sid label cells size selected =
  button_
    [ class_ (if selected then "life-tool is-selected" else "life-tool")
    , type_ "button"
    , makeAttribute "data-tool" (T.pack (show sid))
    , title_ label
    , makeAttribute "aria-label" label
    , makeAttribute "aria-pressed" (if selected then "true" else "false")
    ]
    $ do
      toolPreview cells size
      span_ [class_ "life-tool-name"] (toHtml label)

toolPreview :: [(Int, Int)] -> Maybe (Int, Int) -> Html ()
toolPreview cells size =
  span_
    [ class_ "life-tool-preview"
    , makeAttribute "data-tw" (T.pack (show w))
    ]
    $ mapM_
      cellSpan
      [(x, y) | y <- [minY .. minY + h - 1], x <- [minX .. minX + w - 1]]
 where
  (minX, minY, w, h) = previewBox cells size
  cellSpan :: (Int, Int) -> Html ()
  cellSpan (x, y) =
    span_
      [ class_
          ( if (x, y) `elem` cells
              then "life-tool-cell is-on"
              else "life-tool-cell"
          )
      ]
      mempty

previewBox :: [(Int, Int)] -> Maybe (Int, Int) -> (Int, Int, Int, Int)
previewBox cells size =
  case size of
    Just (w, h) -> (0, 0, w, h)
    Nothing ->
      let
        xs = map fst cells
        ys = map snd cells
        minX = minimum xs
        minY = minimum ys
       in
        (minX, minY, maximum xs - minX + 1, maximum ys - minY + 1)
