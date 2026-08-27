{-# LANGUAGE OverloadedStrings #-}

module Page (page, framePage, frameSrcFor, assetBaseFor) where

import qualified Data.Text as T
import Lucid
import Lucid.Base (makeAttribute)
import Names (patternLabel)
import Patterns
  ( PatternSpec (..)
  , disturbPatterns
  , glider
  , gliderSpeciesSid
  , speciesColor
  )
import Types
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
  , ink
  , lifeBoard2dId
  , lifeDebugCollapseId
  , lifeDebugId
  , lifeEraserGhostId
  , lifeEraserRadiusId
  , lifeEraserRadiusValId
  , lifeEraserSizeId
  , lifeIndexHostId
  , lifePauseLabelId
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
  , lifeTypesListId
  , mouseToolSid
  , tickDefaultMs
  , tickMaxMs
  , tickMinMs
  , tickStepMs
  )

-- | Shell page. Game runs in an iframe at @frameSrc@ (HTTP, not blob) so
--   Firefox can fetch @app.js@, wasm, and workers under CORP headers.
--   The frame is deliberately NOT sandboxed: @sandbox=\"allow-scripts\"@
--   gives the frame an opaque origin, and browsers that isolate sandboxed
--   frames into their own process (Chromium/Electron) fail to bring up the
--   WebGL renderer there: the sim ticks but the board stays black.
--   If WebGL is unavailable in the frame the game falls back to a 2D canvas.
page :: T.Text -> T.Text -> T.Text -> Html ()
page _staticRoot _scriptSrc frameSrc = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "JShark • Game of Life"
    style_ shellCss
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

focusFrameScript :: T.Text
focusFrameScript =
  "document.getElementById('life-frame').addEventListener('load',function(){this.focus();},{once:true});"

gameDocument :: T.Text -> T.Text -> T.Text -> Html ()
gameDocument staticRoot scriptSrc assetBase = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "JShark • Game of Life"
    base_ [href_ assetBase]
    link_ [rel_ "stylesheet", href_ (staticRoot <> "/life.css")]
    link_ [rel_ "stylesheet", href_ (staticRoot <> "/github-dark.min.css")]
    style_ toolsCss
    style_ sourceCss
  body_ $ do
    main_ $ do
      h1_ "Conway's Game of Life"
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
          , style_ "display:none"
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
          , style_ "display:none"
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
      div_ [id_ lifeTooltipId, class_ "life-tooltip", role_ "tooltip"] $ do
        span_ [id_ lifeTooltipSwatchId, class_ "life-tooltip-swatch"] mempty
        span_ [id_ lifeTooltipNameId, class_ "life-tooltip-name"] mempty
      p_ [class_ "help"] $
        "Esc pauses. Shift+drag pans. +/− zoom. Clicking an empty cell pauses. Debug and settings work while paused. Eraser (drag; slider sets size) or stamp a pattern. Hover a cell for its name."
      p_ $ do
        toHtml (T.pack (show gridW))
        "×"
        toHtml (T.pack (show gridH))
        " grid. Catalog patterns and soup in the center."
      section_ [class_ "life-index"] $ do
        h2_ "Species"
        div_ [id_ lifeIndexHostId] $
          div_ [id_ lifeTypesListId, class_ "life-index-grid"] mempty
    lifeSourceSection
    script_ [src_ "js/pixi.min.js"] ("" :: Html ())
    script_ [src_ "js/LifeSimd.js"] ("" :: Html ())
    script_ [src_ scriptSrc] ("" :: Html ())
    script_ [src_ (staticRoot <> "/highlight.min.js")] ("" :: Html ())
    sourceLoadScript scriptSrc
    copyScript

lifeSourceSection :: Html ()
lifeSourceSection =
  details_ [class_ "life-source"] $ do
    summary_ [class_ "life-source-summary"] $ do
      span_ [class_ "life-source-chevron", makeAttribute "aria-hidden" "true"] "▸"
      h2_ [class_ "life-source-title"] "Generated JavaScript"
      button_
        [ type_ "button"
        , class_ "js-source-copy life-source-copy"
        , makeAttribute "aria-label" "Copy generated JavaScript"
        , makeAttribute "disabled" "true"
        ]
        "Copy source"
    pre_ [class_ "life-source-pre"] $
      code_ [class_ "language-javascript life-source-code"] "Expand to load source…"

sourceLoadScript :: T.Text -> Html ()
sourceLoadScript scriptSrc =
  script_ $
    "(function(){"
      <> "var pane=document.querySelector('.life-source');"
      <> "var code=document.querySelector('.life-source-code');"
      <> "var btn=document.querySelector('.life-source-copy');"
      <> "if(!pane||!code)return;"
      <> "var loaded=false;"
      <> "function maybeHighlight(el){"
      <> "if(!el||el.dataset.highlighted)return;"
      <> "var t=el.textContent||'';"
      <> "if(t.length>32768){el.dataset.highlighted='skip';return;}"
      <> "if(window.hljs){hljs.highlightElement(el);}"
      <> "el.dataset.highlighted='1';"
      <> "}"
      <> "function load(){"
      <> "if(loaded)return;"
      <> "loaded=true;"
      <> "code.textContent='Loading source…';"
      <> "fetch("
      <> jsString scriptSrc
      <> ")"
      <> ".then(function(r){if(!r.ok)throw new Error('fetch');return r.text();})"
      <> ".then(function(t){"
      <> "code.textContent=t;"
      <> "if(btn)btn.removeAttribute('disabled');"
      <> "requestAnimationFrame(function(){maybeHighlight(code);});"
      <> "})"
      <> ".catch(function(){"
      <> "code.textContent='// Failed to load generated source';"
      <> "});"
      <> "}"
      <> "pane.addEventListener('toggle',function(){if(pane.open)load();});"
      <> "})();"

copyScript :: Html ()
copyScript =
  script_ $
    "(function(){"
      <> "document.querySelectorAll('.life-source').forEach(function(pane){"
      <> "var btn=pane.querySelector('.js-source-copy');"
      <> "var code=pane.querySelector('code');"
      <> "if(!btn||!code)return;"
      <> "btn.addEventListener('click',function(e){"
      <> "e.preventDefault();"
      <> "e.stopPropagation();"
      <> "var text=code.textContent||'';"
      <> "if(!text||text==='Loading source…'||text==='Expand to load source…')return;"
      <> "function ok(){"
      <> "btn.textContent='Copied';"
      <> "btn.disabled=true;"
      <> "setTimeout(function(){btn.textContent='Copy source';btn.disabled=false;},1500);"
      <> "}"
      <> "function fail(){"
      <> "btn.textContent='Failed';"
      <> "setTimeout(function(){btn.textContent='Copy source';},1500);"
      <> "}"
      <> "if(navigator.clipboard&&navigator.clipboard.writeText){"
      <> "navigator.clipboard.writeText(text).then(ok).catch(function(){"
      <> "try{var ta=document.createElement('textarea');"
      <> "ta.value=text;ta.style.position='fixed';ta.style.left='-9999px';"
      <> "document.body.appendChild(ta);ta.select();"
      <> "document.execCommand('copy');document.body.removeChild(ta);ok();"
      <> "}catch(e){fail();}});"
      <> "}else{fail();}"
      <> "});"
      <> "});"
      <> "})();"

shellCss :: T.Text
shellCss =
  "html,body.life-shell{margin:0;padding:0;background:#0f172a;height:100%;overflow:hidden}\
  \body.life-shell{display:block}\
  \.life-frame{display:block;width:100%;height:100vh;border:0;background:#0f172a}"

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
    , makeAttribute "aria-label" "Debug"
    ]
    $ do
      button_
        [ id_ lifeDebugCollapseId
        , type_ "button"
        , class_ "life-panel-toggle"
        , makeAttribute "aria-expanded" "false"
        , makeAttribute "aria-label" "Expand debug"
        , title_ "Debug"
        ]
        "debug"
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
          debugRow lifeStatTickId "Tick" False
          debugRow lifeStatEngineId "Engine" True

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
        "−"
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
      toolPreview sid cells size
      span_ [class_ "life-tool-name"] (toHtml label)

toolPreview :: Int -> [(Int, Int)] -> Maybe (Int, Int) -> Html ()
toolPreview sid cells size =
  span_
    [ class_ "life-tool-preview"
    , style_
        ("--tw:" <> T.pack (show w) <> ";--th:" <> T.pack (show h) <> ";--on:" <> onColor)
    ]
    $ mapM_
      cellSpan
      [(x, y) | y <- [minY .. minY + h - 1], x <- [minX .. minX + w - 1]]
 where
  (minX, minY, w, h) = previewBox cells size
  onColor
    | sid == eraserToolSid = "#f87171"
    | sid == mouseToolSid = "#60a5fa"
    | sid == gliderToolSid = rgbCss (speciesColor gliderSpeciesSid)
    | otherwise = rgbCss (speciesColor sid)
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

rgbCss :: (Int, Int, Int) -> T.Text
rgbCss (r, g, b) =
  "rgb("
    <> T.pack (show r)
    <> ","
    <> T.pack (show g)
    <> ","
    <> T.pack (show b)
    <> ")"

-- | Inlined so the HUD still paints when /static/life.css is missing
--   (cabal data-files 404 in some run paths).
toolsCss :: T.Text
toolsCss =
  ".life-stage{position:relative;width:768px;max-width:100%;margin:1.5rem auto;overflow:visible}\
  \.life-stage canvas{display:block;margin:0;width:768px;max-width:100%;height:auto;aspect-ratio:768/576;\
  \image-rendering:pixelated;image-rendering:crisp-edges;cursor:crosshair}\
  \#life-board{image-rendering:auto}\
  \.life-board-2d{position:absolute;inset:0;width:100%;height:auto;aspect-ratio:768/576;\
  \pointer-events:none;z-index:1;image-rendering:pixelated;image-rendering:crisp-edges}\
  \.life-eraser-ghost{position:absolute;inset:0;width:100%;height:auto;aspect-ratio:768/576;\
  \pointer-events:none;z-index:2;image-rendering:pixelated;image-rendering:crisp-edges}\
  \.life-eraser-size{display:flex;align-items:center;gap:0.45rem;pointer-events:auto;padding:0.35rem 0.5rem;\
  \border-radius:6px;background:rgba(15,23,42,0.88);border:1px solid #334155;\
  \box-shadow:0 2px 8px rgba(0,0,0,0.35);font:0.72rem system-ui,sans-serif;color:#cbd5e1}\
  \.life-eraser-size[hidden]{display:none!important}\
  \.life-eraser-size-label{display:flex;align-items:center;gap:0.35rem;margin:0;cursor:default}\
  \.life-eraser-radius{width:5.5rem;accent-color:#f87171}\
  \.life-eraser-radius-val{min-width:1.1rem;text-align:right;color:#f87171;font-variant-numeric:tabular-nums}\
  \.life-pause-overlay{position:absolute;inset:0;z-index:3;pointer-events:none;\
  \display:flex;align-items:flex-start;justify-content:center;padding-top:0.7rem;\
  \background:rgba(56,189,248,0.1);box-shadow:inset 0 0 0 1px rgba(56,189,248,0.18);\
  \opacity:0;transition:opacity 0.15s ease}\
  \.life-pause-overlay.is-visible{opacity:1}\
  \.life-pause-label{font:15px Georgia,serif;color:"
    <> ink
    <> ";letter-spacing:0.04em}\
       \.life-panel{position:absolute;z-index:6;display:flex;flex-direction:column;gap:0.25rem;\
       \pointer-events:none;user-select:none}\
       \.life-debug{left:8px;top:8px;align-items:flex-start}\
       \.life-settings{right:8px;top:8px;align-items:flex-end}\
       \.life-panel-toggle{appearance:none;pointer-events:auto;display:flex;align-items:center;\
       \justify-content:center;min-width:1.65rem;height:1.65rem;padding:0 0.4rem;border:1px solid #334155;\
       \border-radius:4px;background:#0f172a;color:#94a3b8;font:600 0.95rem system-ui,sans-serif;\
       \cursor:pointer;line-height:1;box-shadow:0 2px 8px rgba(0,0,0,0.35)}\
       \.life-panel-toggle:hover{border-color:#64748b;color:#e2e8f0}\
       \.life-panel-body{pointer-events:auto;display:flex;flex-direction:column;gap:0.2rem;min-width:8.5rem;\
       \padding:0.4rem 0.5rem;border-radius:6px;background:rgba(15,23,42,0.92);border:1px solid #334155;\
       \box-shadow:0 2px 8px rgba(0,0,0,0.35);font:0.78rem system-ui,sans-serif;color:#cbd5e1}\
       \.life-panel.is-collapsed .life-panel-body{display:none}\
       \.life-debug-row{display:grid;grid-template-columns:4.25rem minmax(5ch,1fr);\
       \gap:0.35rem;align-items:baseline;white-space:nowrap;font-variant-numeric:tabular-nums}\
       \.life-debug-row-wide .dbg-v{text-align:left;overflow:hidden;text-overflow:ellipsis}\
       \.dbg-k{color:#64748b}\
       \.dbg-v{color:#cbd5e1;text-align:right}\
       \.life-settings-row{display:flex;align-items:center;justify-content:space-between;gap:0.6rem}\
       \.life-settings-label{color:#94a3b8}\
       \.life-settings-value{font-variant-numeric:tabular-nums}\
       \.life-settings-actions{justify-content:flex-end}\
       \.life-settings-btn{appearance:none;pointer-events:auto;border:1px solid #334155;border-radius:4px;\
       \background:#0f172a;color:#cbd5e1;font:600 0.78rem system-ui,sans-serif;padding:0.22rem 0.45rem;cursor:pointer}\
       \.life-settings-btn:hover{border-color:#64748b;color:#e2e8f0}\
       \.life-settings-reset{width:100%}\
       \.life-settings-select{appearance:none;pointer-events:auto;border:1px solid #334155;\
       \border-radius:4px;background:#0f172a;color:#cbd5e1;font:0.72rem system-ui,sans-serif;\
       \padding:0.18rem 0.3rem;max-width:7.2rem}\
       \.life-settings-range{pointer-events:auto;width:100%;accent-color:#38bdf8}\
       \.life-tools{position:absolute;right:8px;bottom:8px;z-index:5;display:flex;\
       \flex-direction:column;align-items:flex-end;gap:0.25rem;user-select:none;pointer-events:none}\
       \.life-tools-collapse{appearance:none;pointer-events:auto;display:flex;align-items:center;\
       \justify-content:center;width:1.65rem;height:1.65rem;padding:0;border:1px solid #334155;\
       \border-radius:4px;background:#0f172a;color:#94a3b8;font:600 0.95rem system-ui,sans-serif;\
       \cursor:pointer;line-height:1;box-shadow:0 2px 8px rgba(0,0,0,0.35)}\
       \.life-tools-collapse:hover{border-color:#64748b;color:#e2e8f0}\
       \.life-tools-body{display:flex;flex-wrap:wrap;justify-content:flex-end;gap:0.28rem;max-width:22rem;\
       \padding:0.35rem;border-radius:6px;background:rgba(15,23,42,0.88);border:1px solid #334155;\
       \box-shadow:0 2px 8px rgba(0,0,0,0.35)}\
       \.life-tools.is-collapsed .life-tools-body{display:none}\
       \.life-tool{appearance:none;display:flex;flex-direction:column;align-items:center;\
       \gap:0.2rem;width:3.35rem;padding:0.28rem 0.2rem 0.22rem;border:1px solid #334155;\
       \border-radius:4px;background:#0f172a;color:#cbd5e1;cursor:pointer;pointer-events:auto}\
       \.life-tool:hover{border-color:#64748b}\
       \.life-tool.is-selected{border-color:#38bdf8;box-shadow:0 0 0 1px #38bdf8}\
       \.life-tool-preview{display:grid;grid-template-columns:repeat(var(--tw),3px);\
       \grid-auto-rows:3px;gap:1px;justify-content:center}\
       \.life-tool-cell{width:3px;height:3px;background:#1e293b}\
       \.life-tool-cell.is-on{background:var(--on,#e2e8f0)}\
       \.life-tool-name{font-size:0.58rem;line-height:1.1;max-width:3.1rem;overflow:hidden;\
       \text-overflow:ellipsis;white-space:nowrap;color:#94a3b8}"

sourceCss :: T.Text
sourceCss =
  ".life-source{position:static;box-sizing:border-box;max-width:768px;margin:2.5rem auto 3rem;\
  \padding:0 1rem 0;border-top:1px solid #334155;color:#cbd5e1;font-family:Georgia,serif;\
  \text-align:left;clear:both}\
  \.life-source-summary{display:flex;align-items:center;gap:0.65rem;cursor:pointer;list-style:none;\
  \padding:1.25rem 0 0.85rem}\
  \.life-source-summary::-webkit-details-marker{display:none}\
  \.life-source-chevron{flex:0 0 auto;color:#94a3b8;transition:transform 0.15s ease}\
  \.life-source[open]>.life-source-summary .life-source-chevron{transform:rotate(90deg)}\
  \.life-source-title{margin:0;flex:1 1 auto;min-width:0;font-size:1.15rem;font-weight:400;color:#e2e8f0}\
  \.life-source-copy{appearance:none;flex:0 0 auto;padding:0.25rem 0.55rem;border:1px solid #334155;\
  \border-radius:4px;background:#0f172a;color:#cbd5e1;font:0.78rem system-ui,sans-serif;cursor:pointer}\
  \.life-source-copy:hover:not(:disabled){border-color:#64748b;color:#e2e8f0}\
  \.life-source-copy:disabled{opacity:0.6;cursor:default}\
  \.life-source-pre{margin:0 0 3rem;max-height:32rem;overflow:auto;overscroll-behavior:contain;\
  \contain:content;content-visibility:auto;border-radius:8px;border:1px solid #334155;background:#0b1220}\
  \.life-source-code{font-size:0.82rem;line-height:1.45}"
