{-# LANGUAGE OverloadedStrings #-}

module SourcePane
  ( SourcePaneSpec (..)
  , sourceHead
  , sourceHeadLite
  , sourcePane
  , hvm2SourcePanes
  , sourcePanes
  )
where

import qualified Data.Text as T
import Lucid
import Lucid.Base (makeAttribute)
import ThemeHead (sourceLinks, sourceLinksLite)

data SourcePaneSpec = SourcePaneSpec
  { paneLabel :: T.Text
  , paneLang :: T.Text
  , paneBody :: T.Text
  }

-- | Highlight + source pane CSS. Themed pages link 'ThemeHead.themeLinks' first.
sourceHead :: T.Text -> Html ()
sourceHead = sourceLinks

-- | TodoMVC skips Pico; includes 'tokens.css' for pane chrome.
sourceHeadLite :: T.Text -> Html ()
sourceHeadLite = sourceLinksLite

-- | Collapsed pane of compiled client JS. Scripts follow the markup.
sourcePane :: T.Text -> T.Text -> Html ()
sourcePane staticRoot js =
  sourcePanes
    staticRoot
    [SourcePaneSpec "Source" "javascript" js]

-- | HVM2 demo: client JS, Bend kernel, and per-pixel JS fallback.
hvm2SourcePanes :: T.Text -> T.Text -> T.Text -> T.Text -> Html ()
hvm2SourcePanes staticRoot js bend mandelJs =
  sourcePanes
    staticRoot
    [ SourcePaneSpec "Client" "javascript" js
    , SourcePaneSpec "Bend" "python" bend
    , SourcePaneSpec "JS fallback" "javascript" mandelJs
    ]

-- | Stack of collapsible highlighted source panes (one highlight/copy setup).
sourcePanes :: T.Text -> [SourcePaneSpec] -> Html ()
sourcePanes staticRoot specs = do
  div_ [class_ "source-stack"] $
    mapM_ (pane staticRoot) specs
  script_ [type_ "module", src_ (staticRoot <> "/source-pane.js")] ("" :: Html ())

pane :: T.Text -> SourcePaneSpec -> Html ()
pane _ (SourcePaneSpec label lang body) = do
  details_ [class_ "js-source"] $ do
    summary_ [class_ "js-source-summary"] $ do
      span_ [class_ "js-source-summary-inner"] $ do
        span_ [class_ "js-source-label"] (toHtml label)
        span_ [class_ "js-source-expand-hint"] "click to expand"
      button_
        [ type_ "button"
        , class_ "js-source-copy"
        , makeAttribute "aria-label" ("Copy " <> label)
        ]
        "Copy"
    pre_ $
      code_ [class_ ("shj-lang-" <> shjLang lang)] (toHtml body)

shjLang :: T.Text -> T.Text
shjLang "javascript" = "js"
shjLang "python" = "py"
shjLang "plaintext" = "plain"
shjLang other = other
