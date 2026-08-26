{-# LANGUAGE OverloadedStrings #-}

module Page
  ( page
  , boardId
  , statusId
  , metricKernelId
  , metricFrameMsId
  , metricFpsId
  , metricFrameNumId
  , metricScaleId
  , metricCenterId
  , metricBackendId
  , modeWasmId
  , modeHvm2Id
  , modeJsId
  , benchId
  , resSelectId
  , pauseId
  )
where

import Control.Monad (forM_)
import qualified Data.Text as T
import Kernels (blockPx, canvasH, canvasW, maxIter, resolutionPresets)
import Lucid
import Lucid.Base (makeAttribute)

boardId
  , statusId
  , modeWasmId
  , modeHvm2Id
  , modeJsId
  , benchId
  , resSelectId
  , pauseId ::
    T.Text
metricKernelId
  , metricFrameMsId
  , metricFpsId
  , metricFrameNumId
  , metricScaleId
  , metricCenterId
  , metricBackendId ::
    T.Text
boardId = "hvm2-canvas"
statusId = "hvm2-status"

metricKernelId = "hvm2-metric-kernel"

metricFrameMsId = "hvm2-metric-frame"

metricFpsId = "hvm2-metric-fps"

metricFrameNumId = "hvm2-metric-frame-n"

metricScaleId = "hvm2-metric-scale"

metricCenterId = "hvm2-metric-center"

metricBackendId = "hvm2-metric-backend"

modeWasmId = "hvm2-mode-wasm"

modeHvm2Id = "hvm2-mode-hvm2"

modeJsId = "hvm2-mode-js"

benchId = "hvm2-bench"

resSelectId = "hvm2-res"

pauseId = "hvm2-pause"

metricRow :: T.Text -> T.Text -> Maybe T.Text -> Html ()
metricRow label valId unit =
  div_ [class_ "hvm2-metric"] $ do
    span_ [class_ "hvm2-metric-k"] (toHtml label)
    span_ [id_ valId, class_ "hvm2-metric-v"] "—"
    case unit of
      Just u -> span_ [class_ "hvm2-metric-u"] (toHtml u)
      Nothing -> pure ()

-- | HVM2 lab shell. @staticRoot@ is shared assets; @demoBase@ prefixes wasm
-- | assets (@""@ for export, @"/hvm2-demo"@ on Scotty); @headExtra@ / @source@
-- | are the highlighter and source panes.
page :: T.Text -> T.Text -> Html () -> Html () -> T.Text -> Html ()
page staticRoot demoBase headExtra source scriptSrc = doctypehtml_ $ do
  let
    demoAsset name =
      if T.null demoBase then name else demoBase <> "/" <> name
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
                [ id_ modeHvm2Id
                , type_ "button"
                , class_ "mode"
                ]
                "hvm2"
              button_
                [ id_ modeJsId
                , type_ "button"
                , class_ "mode"
                ]
                "js"
          fieldset_ $ do
            legend_ "resolution"
            select_ [id_ resSelectId, class_ "res-select"] $
              forM_ resolutionPresets $ \(presetW, presetH, presetLabel) -> do
                let
                  val = T.pack (show presetW <> "x" <> show presetH)
                  isDefault = presetW == canvasW && presetH == canvasH
                option_
                  ( [value_ val]
                      <> [selected_ "selected" | isDefault]
                  )
                  (toHtml presetLabel)
          button_ [id_ pauseId, type_ "button", class_ "pause"] "pause"
          button_ [id_ benchId, type_ "button", class_ "bench"] "bench all backends"
        div_ [class_ "hvm2-viewport"] $
          canvas_
            [ id_ boardId
            , width_ (T.pack (show canvasW))
            , height_ (T.pack (show canvasH))
            , makeAttribute "data-wasm" (demoAsset "hvm2-demo.wasm")
            , makeAttribute "data-worker" (demoAsset "hvm2-worker.js")
            ]
            mempty
        div_ [class_ "hvm2-telemetry"] $ do
          div_ [class_ "hvm2-metrics", role_ "status"] $ do
            metricRow "kernel" metricKernelId (Just "ms")
            metricRow "frame" metricFrameMsId (Just "ms")
            metricRow "fps" metricFpsId Nothing
            metricRow "frame #" metricFrameNumId Nothing
            metricRow "scale" metricScaleId Nothing
            metricRow "center" metricCenterId Nothing
            div_ [class_ "hvm2-metric hvm2-metric-wide"] $ do
              span_ [class_ "hvm2-metric-k"] "backend"
              span_ [id_ metricBackendId, class_ "hvm2-metric-v"] "…"
          p_ [id_ statusId, class_ "status status-alert"] ""
          p_ [class_ "hint"] $
            do
              toHtml (show blockPx)
              "×"
              toHtml (show blockPx)
              " px blocks, "
              toHtml (show maxIter)
              " iters. Telemetry uses desync canvas + wall-clock ms (not vsync). wasm = SIMD128 grid; hvm2 = Bend book on HVM2 evaluator ("
              <> toHtml (show (4 :: Int))
              <> " wasm threads via Web Workers + shared memory, needs COOP/COEP); js = mandelJsSource. Bench times all backends."
      footer_ [class_ "info"] $
        p_ $ do
          "wasm: "
          code_ "cabal run build-hvm2-demo-wasm"
    script_ [src_ (demoAsset "hvm2-wasm.js")] ("" :: Html ())
    source
    script_ [src_ scriptSrc] ("" :: Html ())
