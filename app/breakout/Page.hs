{-# LANGUAGE OverloadedStrings #-}

module Page (page) where

import Data.Text (pack)
import Lucid
import Types (boardId, canvasH, canvasW)

-- | Breakout shell. Client script is loaded from @/app.js@.
page :: Html ()
page = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "JShark • Breakout"
    style_ $
      "html,body{margin:0;min-height:100%;background:#0f172a;color:#e2e8f0;"
        <> "font-family:Georgia,serif}"
        <> "main{max-width:32rem;margin:2rem auto;padding:0 1rem;text-align:center}"
        <> "h1{font-weight:400;letter-spacing:.04em}"
        <> "canvas{background:#e2e8f0;display:block;margin:1.5rem auto;border-radius:2px}"
        <> "p{color:#94a3b8}"
        <> "a{color:#38bdf8}"
  body_ $
    main_ $ do
      h1_ "Breakout"
      canvas_
        [ id_ boardId
        , width_ (pack (show (round canvasW :: Int)))
        , height_ (pack (show (round canvasH :: Int)))
        ]
        mempty
      p_ "Arrows or mouse to move. Space to restart after win or loss."
      p_ $ do
        "JShark port of the "
        a_ [href_ "https://github.com/end3r/Gamedev-Canvas-workshop"] "MDN canvas workshop"
        " with types from "
        a_ [href_ "https://github.com/akhesaCaro/haskell-breakout"] "haskell-breakout"
      script_ [src_ "/app.js"] ("" :: Html ())
