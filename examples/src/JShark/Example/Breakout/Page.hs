{-# LANGUAGE OverloadedStrings #-}

module JShark.Example.Breakout.Page (page) where

import qualified Data.Text as T
import JShark.Example.Breakout.Types (boardId, canvasH, canvasW)
import JShark.Example.Theme (ExamplePage, themedPage)
import Lucid

-- | Breakout shell.
page :: ExamplePage
page = themedPage "Breakout" ["/css/breakout.css"] $ main_ [class_ "page"] $ do
  header_ [class_ "page-header"] $ do
    h1_ "Breakout"
    p_ [class_ "page-hint"] "Arrows or mouse · space to restart"
  canvas_ [id_ boardId, width_ (px canvasW), height_ (px canvasH)] mempty
  footer_ [class_ "page-footer"] $ p_ $ do
    "Port of the "
    a_
      [href_ "https://github.com/end3r/Gamedev-Canvas-workshop"]
      "MDN canvas workshop"
    " · types from "
    a_ [href_ "https://github.com/akhesaCaro/haskell-breakout"] "haskell-breakout"
 where
  px d = T.pack (show (round d :: Int))
