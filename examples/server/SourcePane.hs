{-# LANGUAGE OverloadedStrings #-}

module SourcePane (sourceHead, sourcePane) where

import qualified Data.Text as T
import Lucid

-- | Highlight CSS. Put in @head@ so it is ready before the pane opens.
sourceHead :: Html ()
sourceHead = do
  link_ [rel_ "stylesheet", href_ "/static/github-dark.min.css"]
  style_ sourceCss

-- | Collapsed pane of compiled client JS. Scripts follow the markup.
sourcePane :: T.Text -> Html ()
sourcePane js = do
  details_ [class_ "js-source"] $ do
    summary_ "JavaScript source"
    pre_ $ code_ [class_ "language-javascript"] (toHtml js)
  script_ [src_ "/static/highlight.min.js"] ("" :: Html ())
  script_ ("hljs.highlightAll();" :: T.Text)

sourceCss :: T.Text
sourceCss =
  ".js-source{box-sizing:border-box;max-width:48rem;margin:2rem auto;padding:0 1rem;"
    <> "color:#e2e8f0;font-family:system-ui,sans-serif;text-align:left}"
    <> ".js-source summary{cursor:pointer;padding:.4rem 0;color:#94a3b8}"
    <> ".js-source pre{margin:.5rem 0 0;max-height:28rem;overflow:auto;"
    <> "border-radius:6px}"
    <> ".js-source code{font-size:.85rem}"
