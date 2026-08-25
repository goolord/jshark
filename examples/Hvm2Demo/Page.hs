{-# LANGUAGE OverloadedStrings #-}

module Page
  ( page
  , boardId
  , statusId
  , modeWasmId
  , modeJsId
  , benchId
  )
where

import qualified Data.Text as T
import Kernels (blockPx, canvasH, canvasW, maxIter)
import Lucid
import Lucid.Base (makeAttribute)

boardId, statusId, modeWasmId, modeJsId, benchId :: T.Text
boardId = "hvm2-canvas"
statusId = "hvm2-status"
modeWasmId = "hvm2-mode-wasm"
modeJsId = "hvm2-mode-js"
benchId = "hvm2-bench"

-- | HVM2 lab shell. @staticRoot@ is shared assets; @headExtra@ / @source@ are
-- the highlighter and JS pane on the examples index.
page :: T.Text -> Html () -> Html () -> T.Text -> Html ()
page staticRoot headExtra source scriptSrc = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "JShark / hvm2 mandelbrot"
    link_ [rel_ "stylesheet", href_ (staticRoot <> "/hvm2-demo.css")]
    headExtra
  body_ $ do
    main_ [class_ "hvm2"] $ do
      div_ [class_ "hvm2-header"] $ do
        h1_ "mandelbrot / hvm2"
        p_ [class_ "lede"] $ do
          code_ "mandel(cr, ci)"
          " in JShark, compiled to Bend then WASM."
        p_ [class_ "path"] "Expr → Bend → WASM → canvas"
      div_ [class_ "panel"] $ do
        div_ [class_ "controls"] $ do
          fieldset_ $ do
            legend_ "backend"
            div_ [class_ "btn-row"] $ do
              button_
                [ id_ modeWasmId
                , type_ "button"
                , class_ "mode active"
                ]
                "wasm"
              button_
                [ id_ modeJsId
                , type_ "button"
                , class_ "mode"
                ]
                "js"
          button_ [id_ benchId, type_ "button", class_ "bench"] "bench 50k"
        div_ [class_ "hvm2-viewport"] $
          canvas_
            [ id_ boardId
            , width_ (T.pack (show canvasW))
            , height_ (T.pack (show canvasH))
            , makeAttribute "data-wasm" (staticRoot <> "/hvm2-demo.wasm")
            ]
            mempty
        div_ [class_ "hvm2-telemetry"] $ do
          p_ [id_ statusId, class_ "status"] "loading wasm…"
          p_ [class_ "hint"] $ do
            toHtml (show blockPx)
            "×"
            toHtml (show blockPx)
            " px blocks, "
            toHtml (show maxIter)
            " iters, one grid sample per frame. fps is smoothed. bench times the kernel."
    footer_ [class_ "info"] $
      p_ $ do
        "wasm: "
        code_ "cabal run build-hvm2-demo-wasm"
    source
    script_ [src_ scriptSrc] ("" :: Html ())
