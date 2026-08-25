{-# LANGUAGE OverloadedStrings #-}

module Page (page) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Lucid
import Lucid.Base (makeAttribute)
import Names (patternLabel)
import Patterns (PatternSpec (..), disturbPatterns, speciesColor)
import Types
  ( boardId
  , canvasH
  , canvasW
  , cellPx
  , gridH
  , gridW
  , hoverRadius
  , ink
  , lifeIndexHostId
  , lifeToolsId
  , lifeToolsCollapseId
  , lifeStatCellsId
  , lifeStatEngineId
  , lifeStatFpsId
  , lifeStatGenId
  , lifeStatStatusId
  , lifeStatTickId
  , lifeStatZoomId
  , lifeTooltipId
  , lifeTooltipNameId
  , lifeTooltipSwatchId
  , lifeTypesListId
  , toggleToolSid
  , eraserToolSid
  )

-- | Shell page. The live board, tooltip, and index run inside a @blob:@
--   iframe so extension content scripts (Bitwarden, video scanners) that
--   match @http(s)://*@ never attach to the mutating document. Page JS
--   cannot revoke those listeners; an opaque origin is what actually
--   keeps them off the game. The iframe is @sandbox=\"allow-scripts\"@
--   without @allow-same-origin@ so the parent (and extension worlds
--   injected there) cannot walk @contentDocument@. @<base href>@ is
--   filled in at boot so relative @app.js@ / static URLs still resolve
--   against the host page.
--
--   Fetched at runtime from @app.js@ so the blob iframe stays small.
page :: T.Text -> T.Text -> Html ()
page staticRoot scriptSrc = doctypehtml_ $ do
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
      , sandbox_ "allow-scripts"
      ]
      mempty
    script_ (bootJs (gameHtml staticRoot scriptSrc))

gameHtml :: T.Text -> T.Text -> T.Text
gameHtml staticRoot scriptSrc =
  TL.toStrict (renderText (gameDocument staticRoot scriptSrc))

gameDocument :: T.Text -> T.Text -> Html ()
gameDocument staticRoot scriptSrc = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "JShark • Game of Life"
    toHtmlRaw ("<base href=\"%%LIFE_BASE%%\">" :: T.Text)
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
        statsHud
        toolsHud
      div_ [id_ lifeTooltipId, class_ "life-tooltip", role_ "tooltip"] $ do
        span_ [id_ lifeTooltipSwatchId, class_ "life-tooltip-swatch"] mempty
        span_ [id_ lifeTooltipNameId, class_ "life-tooltip-name"] mempty
      p_ [class_ "help"] $
        "Space to pause or resume. Shift+drag to pan; +/− (or numpad) to zoom. Toggling an empty cell pauses so it stays. The HUD picks the left-click tool: toggle, eraser, or drop a spaceship or methuselah. Hover within "
          <> toHtml (T.pack (show (hoverRadius * cellPx + cellPx)))
          <> "px of a species for its name. "
          <> "Emergent soup patterns are scanned every 45 generations: "
          <> "known shapes inherit catalog hues; novel ones get procedural names and colors."
      p_ $
        "A "
          <> toHtml (T.pack (show (gridW * gridH :: Int)))
          <> "-cell universe ("
          <> toHtml (T.pack (show gridW))
          <> "×"
          <> toHtml (T.pack (show gridH))
          <> ") — still lifes, oscillators, spaceships, methuselah seeds, eaters, and misc patterns seeded in the central region alongside random soup. Pan and zoom to explore."
      section_ [class_ "life-index"] $ do
        h2_ "Biomass Index"
        div_ [id_ lifeIndexHostId] $
          div_ [id_ lifeTypesListId, class_ "life-index-grid"] mempty
    lifeSourceSection
    script_ [src_ "js/pixi.min.js"] ("" :: Html ())
    script_ [src_ "js/catalog.js"] ("" :: Html ())
    script_ [src_ "js/LUTGenerator.js"] ("" :: Html ())
    script_ [src_ "js/LifeSimd.js"] ("" :: Html ())
    script_ [src_ "js/Main.js"] ("" :: Html ())
    script_ [src_ scriptSrc] ("" :: Html ())
    script_ [src_ (staticRoot <> "/highlight.min.js")] ("" :: Html ())
    sourceLoadScript scriptSrc
    copyScript

lifeSourceSection :: Html ()
lifeSourceSection =
  section_ [class_ "life-source"] $ do
    div_ [class_ "life-source-header"] $ do
      h2_ [class_ "life-source-title"] "Generated JavaScript"
      button_
        [ type_ "button"
        , class_ "js-source-copy life-source-copy"
        , makeAttribute "aria-label" "Copy generated JavaScript"
        , makeAttribute "disabled" "true"
        ]
        $ do
          span_ [class_ "life-source-copy-icon", makeAttribute "aria-hidden" "true"] "⎘"
          span_ [class_ "life-source-copy-label"] "Copy source"
    pre_ [class_ "life-source-pre"] $
      code_ [class_ "language-javascript life-source-code"] "Loading source…"

sourceLoadScript :: T.Text -> Html ()
sourceLoadScript scriptSrc =
  script_ $
    "(function(){"
      <> "var code=document.querySelector('.life-source-code');"
      <> "var btn=document.querySelector('.life-source-copy');"
      <> "if(!code)return;"
      <> "fetch("
      <> jsString scriptSrc
      <> ")"
      <> ".then(function(r){if(!r.ok)throw new Error('fetch');return r.text();})"
      <> ".then(function(t){"
      <> "code.textContent=t;"
      <> "if(btn)btn.removeAttribute('disabled');"
      <> "if(window.hljs){requestAnimationFrame(function(){hljs.highlightElement(code);});}"
      <> "})"
      <> ".catch(function(){"
      <> "code.textContent='// Failed to load generated source';"
      <> "});"
      <> "})();"

copyScript :: Html ()
copyScript =
  script_ $
    "(function(){"
      <> "document.querySelectorAll('.life-source').forEach(function(pane){"
      <> "var btn=pane.querySelector('.js-source-copy');"
      <> "var code=pane.querySelector('code');"
      <> "if(!btn||!code)return;"
      <> "var label=btn.querySelector('.life-source-copy-label');"
      <> "btn.addEventListener('click',function(e){"
      <> "e.preventDefault();"
      <> "e.stopPropagation();"
      <> "var text=code.textContent||'';"
      <> "if(!text||text==='Loading source…')return;"
      <> "function ok(){"
      <> "if(label){label.textContent='Copied!';}else{btn.textContent='Copied!';}"
      <> "btn.disabled=true;"
      <> "btn.classList.add('is-copied');"
      <> "setTimeout(function(){"
      <> "if(label){label.textContent='Copy source';}else{btn.textContent='Copy';}"
      <> "btn.disabled=false;btn.classList.remove('is-copied');"
      <> "},1500);"
      <> "}"
      <> "function fail(){"
      <> "if(label){label.textContent='Copy failed';}else{btn.textContent='Copy failed';}"
      <> "setTimeout(function(){"
      <> "if(label){label.textContent='Copy source';}else{btn.textContent='Copy source';}"
      <> "},1500);"
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

bootJs :: T.Text -> T.Text
bootJs inner =
  T.concat
    [ "(()=>{"
    , "const frame=document.getElementById('life-frame');"
    , "const pageUrl=new URL(document.baseURI);"
    , "if(!pageUrl.pathname.endsWith('/')){pageUrl.pathname+='/';}"
    , "const base=pageUrl.href;"
    , "const html="
    , jsString inner
    , ".split('%%LIFE_BASE%%').join(base);"
    , "const url=URL.createObjectURL(new Blob([html],{type:'text/html'}));"
    , "frame.src=url;"
    , "frame.addEventListener('load',()=>{URL.revokeObjectURL(url);frame.focus();},{once:true});"
    , "})();"
    ]

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

statsHud :: Html ()
statsHud =
  div_
    [ class_ "life-stats"
    , role_ "status"
    , makeAttribute "aria-live" "polite"
    ]
    $ do
      span_ [id_ lifeStatGenId, class_ "life-stat life-stat-gen"] mempty
      span_ [id_ lifeStatCellsId, class_ "life-stat life-stat-cells"] mempty
      span_ [id_ lifeStatFpsId, class_ "life-stat life-stat-fps"] mempty
      span_ [id_ lifeStatStatusId, class_ "life-stat life-stat-status"] mempty
      span_ [id_ lifeStatZoomId, class_ "life-stat life-stat-zoom"] mempty
      span_ [id_ lifeStatTickId, class_ "life-stat life-stat-tick"] mempty
      span_ [id_ lifeStatEngineId, class_ "life-stat life-stat-engine"] mempty

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
        toolButton toggleToolSid "Toggle" [(1, 1)] (Just (3, 3)) True
        toolButton eraserToolSid "Eraser" [(1, 1)] (Just (3, 3)) False
        mapM_ disturbButton disturbPatterns

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
    | sid == toggleToolSid = ink
    | sid == eraserToolSid = "#f87171"
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
  \.life-stats{position:absolute;inset:0 0 auto 0;height:90px;pointer-events:none;z-index:4;\
  \font:15px Georgia,serif;color:"
    <> ink
    <> ";}\
  \.life-stat{position:absolute;top:0;line-height:1.2;white-space:nowrap}\
  \.life-stat-gen{left:8px;top:18px}\
  \.life-stat-cells{left:8px;top:36px}\
  \.life-stat-fps{left:50%;transform:translateX(-50%);top:18px}\
  \.life-stat-status{right:8px;top:18px;text-align:right}\
  \.life-stat-zoom{right:8px;top:36px;text-align:right}\
  \.life-stat-tick{left:50%;transform:translateX(-50%);top:54px}\
  \.life-stat-engine{left:50%;transform:translateX(-50%);top:72px}\
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
  \padding:1.25rem 1rem 0;border-top:1px solid #334155;color:#cbd5e1;font-family:Georgia,serif;\
  \text-align:left;clear:both}\
  \.life-source-header{display:flex;align-items:center;justify-content:space-between;\
  \gap:1rem;margin-bottom:0.85rem;flex-wrap:wrap}\
  \.life-source-title{margin:0;font-size:1.15rem;font-weight:400;color:#e2e8f0}\
  \.life-source-copy{display:inline-flex;align-items:center;gap:0.4rem;padding:0.45rem 0.95rem;\
  \border:1px solid #0ea5e9;border-radius:6px;background:linear-gradient(180deg,#0ea5e9 0%,#0284c7 100%);\
  \color:#f0f9ff;font:600 0.82rem system-ui,sans-serif;cursor:pointer;\
  \box-shadow:0 1px 2px rgba(0,0,0,0.35),inset 0 1px 0 rgba(255,255,255,0.15);transition:filter 0.15s,transform 0.15s}\
  \.life-source-copy:hover:not(:disabled){filter:brightness(1.08);transform:translateY(-1px)}\
  \.life-source-copy:active:not(:disabled){transform:translateY(0)}\
  \.life-source-copy:disabled{opacity:0.72;cursor:default;transform:none}\
  \.life-source-copy.is-copied{background:linear-gradient(180deg,#22c55e 0%,#16a34a 100%);\
  \border-color:#4ade80;color:#f0fdf4}\
  \.life-source-copy-icon{font-size:1rem;line-height:1;opacity:0.92}\
  \.life-source-pre{margin:0;max-height:32rem;overflow:auto;border-radius:8px;\
  \border:1px solid #334155;background:#0b1220}\
  \.life-source-code{font-size:0.82rem;line-height:1.45}"
