{-# LANGUAGE OverloadedStrings #-}

module Page (page) where

import qualified Data.Text as T
import Lucid
import Types (boardId, canvasH, canvasW, cellPx, gridH, gridW, hoverRadius, lifeIndexHostId, lifeTooltipId, lifeTooltipNameId, lifeTooltipSwatchId)

page :: T.Text -> Html () -> Html () -> T.Text -> Html ()
page staticRoot headExtra source scriptSrc = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "JShark • Game of Life"
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
        div_ [id_ lifeIndexHostId] mempty
    script_ [src_ scriptSrc] ("" :: Html ())
