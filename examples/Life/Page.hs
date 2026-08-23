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
        <> "Emergent soup patterns are scanned every 45 generations: "
        <> "known shapes inherit catalog hues; novel ones get procedural names and colors."
      p_ $
        "A "
        <> toHtml (T.pack (show (gridW * gridH :: Int)))
        <> "-cell toroidal universe — still lifes, oscillators, spaceships, methuselah seeds, eaters, and misc patterns seeded alongside random soup."
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
          span_ [class_ "swatch swatch--meth"] ""
          "Methuselahs"
        span_ $ do
          span_ [class_ "swatch swatch--eater"] ""
          "Eaters"
        span_ $ do
          span_ [class_ "swatch swatch--misc"] ""
          "Misc"
        span_ $ do
          span_ [class_ "swatch swatch--soup"] ""
          "Soup"
        span_ $ do
          span_ [class_ "swatch swatch--manual"] ""
          "Manual"
        span_ $ do
          span_ [class_ "swatch swatch--discover"] ""
          "Discovered"
    source
    script_ [src_ scriptSrc] ("" :: Html ())
