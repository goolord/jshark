{-# LANGUAGE OverloadedStrings #-}

module Page (page) where

import qualified Data.Text as T
import Lucid
import Types (boardId, canvasH, canvasW)

-- | Breakout shell. @staticRoot@ is the shared assets prefix; @headExtra@ /
-- @source@ are the highlighter and JS pane.
page :: T.Text -> Html () -> Html () -> T.Text -> Html ()
page staticRoot headExtra source scriptSrc = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "JShark • Breakout"
    link_ [rel_ "stylesheet", href_ (staticRoot <> "/breakout.css")]
    headExtra
  body_ $ do
    main_ $ do
      h1_ "Breakout"
      canvas_
        [ id_ boardId
        , width_ (T.pack (show (round canvasW :: Int)))
        , height_ (T.pack (show (round canvasH :: Int)))
        ]
        mempty
      p_ "Arrows or mouse to move. Space to restart after win or loss."
      p_ $ do
        "JShark port of the "
        a_
          [href_ "https://github.com/end3r/Gamedev-Canvas-workshop"]
          "MDN canvas workshop"
        " with types from "
        a_ [href_ "https://github.com/akhesaCaro/haskell-breakout"] "haskell-breakout"
    source
    script_ [src_ scriptSrc] ("" :: Html ())
