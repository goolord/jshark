{-# LANGUAGE OverloadedStrings #-}

module JShark.Example.Breakout.Page (page) where

import qualified Data.Text as T
import JShark.Example.Breakout.Types (boardId, canvasH, canvasW)
import JShark.Example.Theme (githubCorner, themeLinks)
import Lucid
import Lucid.Base (makeAttribute)

-- | Breakout shell.
page :: T.Text -> Html () -> Html () -> T.Text -> Html ()
page staticRoot headExtra source scriptSrc = doctypehtml_ $
  html_ [makeAttribute "data-theme" "dark"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      title_ "Breakout"
      themeLinks staticRoot
      link_ [rel_ "stylesheet", href_ (staticRoot <> "/breakout.css")]
      headExtra
    body_ $ do
      githubCorner
      main_ [class_ "page"] $ do
        header_ [class_ "page-header"] $ do
          h1_ "Breakout"
          p_ [class_ "page-hint"] "Arrows or mouse · space to restart"
        canvas_
          [ id_ boardId
          , width_ (T.pack (show (round canvasW :: Int)))
          , height_ (T.pack (show (round canvasH :: Int)))
          ]
          mempty
        footer_ [class_ "page-footer"] $ do
          p_ $ do
            "Port of the "
            a_
              [href_ "https://github.com/end3r/Gamedev-Canvas-workshop"]
              "MDN canvas workshop"
            " · types from "
            a_ [href_ "https://github.com/akhesaCaro/haskell-breakout"] "haskell-breakout"
      source
      script_ [src_ scriptSrc] ("" :: Html ())
