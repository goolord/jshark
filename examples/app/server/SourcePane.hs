{-# LANGUAGE OverloadedStrings #-}

module SourcePane (sourcePane) where

import qualified Data.Text as T
import Lucid
import Lucid.Base (makeAttribute)

-- | Collapsed, highlighted pane of compiled client JS. Scripts follow the
-- markup.
sourcePane :: T.Text -> T.Text -> Html ()
sourcePane staticRoot js = do
  div_ [class_ "source-stack"] . details_ [class_ "js-source"] $ do
    summary_ [class_ "js-source-summary"] $ do
      span_ [class_ "js-source-summary-inner"] $ do
        span_ [class_ "js-source-label"] "Source"
        span_ [class_ "js-source-expand-hint"] "click to expand"
      button_
        [ type_ "button"
        , class_ "js-source-copy"
        , makeAttribute "aria-label" "Copy Source"
        ]
        "Copy"
    pre_ $ code_ [class_ "shj-lang-js"] (toHtml js)
  script_
    [type_ "module", src_ (staticRoot <> "/js/source-pane.js")]
    ("" :: Html ())
