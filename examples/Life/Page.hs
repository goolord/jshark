{-# LANGUAGE OverloadedStrings #-}

module Page (page) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Lucid
import Types (boardId, canvasH, canvasW, cellPx, gridH, gridW, hoverRadius, lifeIndexHostId, lifeTooltipId, lifeTooltipNameId, lifeTooltipSwatchId, lifeTypesListId)

-- | Shell page. The live board, tooltip, and index run inside a @blob:@
--   iframe so extension content scripts (Bitwarden, video scanners) that
--   match @http(s)://*@ never attach to the mutating document. Page JS
--   cannot revoke those listeners; an opaque origin is what actually
--   keeps them off the game. The iframe is @sandbox=\"allow-scripts\"@
--   without @allow-same-origin@ so the parent (and extension worlds
--   injected there) cannot walk @contentDocument@. @<base href>@ is
--   filled in at boot so relative @app.js@ / static URLs still resolve
--   against the host page.
page :: T.Text -> Html () -> Html () -> T.Text -> Html ()
page staticRoot headExtra source scriptSrc = doctypehtml_ $ do
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
    script_ (bootJs (gameHtml staticRoot headExtra source scriptSrc))

gameHtml :: T.Text -> Html () -> Html () -> T.Text -> T.Text
gameHtml staticRoot headExtra source scriptSrc =
  TL.toStrict (renderText (gameDocument staticRoot headExtra source scriptSrc))

gameDocument :: T.Text -> Html () -> Html () -> T.Text -> Html ()
gameDocument staticRoot headExtra source scriptSrc = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "JShark • Game of Life"
    toHtmlRaw ("<base href=\"%%LIFE_BASE%%\">" :: T.Text)
    link_ [rel_ "stylesheet", href_ (staticRoot <> "/life.css")]
    headExtra
  body_ $ do
    main_ $ do
      h1_ "Conway's Game of Life"
      canvas_
        [ id_ boardId
        , width_ (T.pack (show (round canvasW :: Int)))
        , height_ (T.pack (show (round canvasH :: Int)))
        , tabindex_ "0"
        , autofocus_
        ]
        mempty
      div_ [id_ lifeTooltipId, class_ "life-tooltip", role_ "tooltip"] $ do
        span_ [id_ lifeTooltipSwatchId, class_ "life-tooltip-swatch"] mempty
        span_ [id_ lifeTooltipNameId, class_ "life-tooltip-name"] mempty
      p_ [class_ "help"] $
        "Space to pause. Click to toggle cells. Hover within "
          <> toHtml (T.pack (show (hoverRadius * cellPx + cellPx)))
          <> "px of a species for its name. "
          <> "Emergent soup patterns are scanned every 45 generations: "
          <> "known shapes inherit catalog hues; novel ones get procedural names and colors."
      p_ $
        "A "
          <> toHtml (T.pack (show (gridW * gridH :: Int)))
          <> "-cell toroidal universe — still lifes, oscillators, spaceships, methuselah seeds, eaters, and misc patterns seeded alongside random soup."
      source
      section_ [class_ "life-index"] $ do
        h2_ "Biomass Index"
        div_ [id_ lifeIndexHostId] $
          div_ [id_ lifeTypesListId, class_ "life-index-grid"] mempty
    script_ [src_ scriptSrc] ("" :: Html ())

shellCss :: T.Text
shellCss =
  "html,body.life-shell{margin:0;height:100%;background:#0f172a;overflow:hidden}\
  \.life-frame{display:block;width:100%;height:100%;border:0;background:#0f172a}"

bootJs :: T.Text -> T.Text
bootJs inner =
  T.concat
    [ "(function(){"
    , "const frame=document.getElementById('life-frame');"
    , "const base=new URL('.',document.baseURI).href;"
    , "const html="
    , jsString inner
    , ".split('%%LIFE_BASE%%').join(base);"
    , "const url=URL.createObjectURL(new Blob([html],{type:'text/html'}));"
    , "frame.src=url;"
    , "frame.addEventListener('load',function(){frame.focus();});"
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
