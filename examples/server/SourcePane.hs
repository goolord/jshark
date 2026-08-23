{-# LANGUAGE OverloadedStrings #-}

module SourcePane (sourceHead, sourcePane) where

import qualified Data.Text as T
import Lucid

-- | Highlight CSS. Put in @head@ so it is ready before the pane opens.
sourceHead :: T.Text -> Html ()
sourceHead staticRoot = do
  link_ [rel_ "stylesheet", href_ (staticRoot <> "/github-dark.min.css")]
  link_ [rel_ "stylesheet", href_ (staticRoot <> "/source.css")]

-- | Collapsed pane of compiled client JS. Scripts follow the markup.
sourcePane :: T.Text -> T.Text -> Html ()
sourcePane staticRoot js = do
  details_ [class_ "js-source"] $ do
    summary_ "JavaScript source"
    pre_ $ code_ [class_ "language-javascript"] (toHtml js)
  script_ [src_ (staticRoot <> "/highlight.min.js")] ("" :: Html ())
  script_ ("hljs.highlightAll();" :: T.Text)
