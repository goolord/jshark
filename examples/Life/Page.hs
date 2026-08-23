{-# LANGUAGE OverloadedStrings #-}

module Page (page) where

import qualified Data.Text as T
import Lucid
import Types (boardId, canvasH, canvasW, gridH, gridW)

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
      p_ [class_ "help"] $
        "Space to pause. Click to toggle cells. "
        <> "Blues = still lifes, greens = oscillators, warm = spaceships, pale = manual edits."
      p_ $
        "A "
        <> toHtml (T.pack (show (gridW * gridH :: Int)))
        <> "-cell toroidal universe seeded with random soup plus stamped still lifes, oscillators, and spaceships — each pattern type gets its own hue."
      div_ [class_ "legend"] $ do
        span_ $ do
          span_ [class_ "swatch swatch--still"] ""
          "Still lifes"
        span_ $ do
          span_ [class_ "swatch swatch--osc"] ""
          "Oscillators"
        span_ $ do
          span_ [class_ "swatch swatch--ship"] ""
          "Spaceships"
        span_ $ do
          span_ [class_ "swatch swatch--soup"] ""
          "Random soup"
        span_ $ do
          span_ [class_ "swatch swatch--manual"] ""
          "Manual edits"
    source
    script_ [src_ scriptSrc] ("" :: Html ())
