{-# LANGUAGE OverloadedStrings #-}

module SourcePane
  ( SourcePaneSpec (..)
  , sourceHead
  , sourcePane
  , hvm2SourcePanes
  , sourcePanes
  )
where

import qualified Data.Text as T
import Lucid
import Lucid.Base (makeAttribute)

data SourcePaneSpec = SourcePaneSpec
  { paneLabel :: T.Text
  , paneLang :: T.Text
  , paneBody :: T.Text
  }

-- | Highlight CSS. Put in @head@ so it is ready before the pane opens.
sourceHead :: T.Text -> Html ()
sourceHead staticRoot = do
  link_ [rel_ "stylesheet", href_ (staticRoot <> "/github-dark.min.css")]
  link_ [rel_ "stylesheet", href_ (staticRoot <> "/source.css")]

-- | Collapsed pane of compiled client JS. Scripts follow the markup.
sourcePane :: T.Text -> T.Text -> Html ()
sourcePane staticRoot js =
  sourcePanes
    staticRoot
    [SourcePaneSpec "JavaScript source" "javascript" js]

-- | HVM2 demo: client JS, Bend kernel, and per-pixel JS fallback.
hvm2SourcePanes :: T.Text -> T.Text -> T.Text -> T.Text -> Html ()
hvm2SourcePanes staticRoot js bend mandelJs =
  sourcePanes
    staticRoot
    [ SourcePaneSpec "JavaScript (client)" "javascript" js
    , SourcePaneSpec "Bend (kernel)" "plaintext" bend
    , SourcePaneSpec "JavaScript (mandel fallback)" "javascript" mandelJs
    ]

-- | Stack of collapsible highlighted source panes (one highlight/copy setup).
sourcePanes :: T.Text -> [SourcePaneSpec] -> Html ()
sourcePanes staticRoot specs = do
  div_ [class_ "source-stack"] $
    mapM_ (pane staticRoot) specs
  script_ [src_ (staticRoot <> "/highlight.min.js")] ("" :: Html ())
  highlightScript
  copyScript

pane :: T.Text -> SourcePaneSpec -> Html ()
pane _ (SourcePaneSpec label lang body) = do
  details_ [class_ "js-source"] $ do
    summary_ [class_ "js-source-summary"] $ do
      span_ [class_ "js-source-chevron", makeAttribute "aria-hidden" "true"] "▸"
      span_ [class_ "js-source-label"] (toHtml label)
      button_
        [ type_ "button"
        , class_ "js-source-copy"
        , makeAttribute "aria-label" ("Copy " <> label)
        ]
        "Copy"
    pre_ $
      code_ [class_ ("language-" <> lang)] (toHtml body)

-- | Highlight only when a pane opens; skip plaintext and large bodies.
highlightScript :: Html ()
highlightScript =
  script_ $
    "(function(){"
      <> "function maybeHighlight(code){"
      <> "if(!code||code.dataset.highlighted)return;"
      <> "var lang=(code.className||'').match(/language-(\\S+)/);"
      <> "if(lang&&lang[1]==='plaintext'){code.dataset.highlighted='1';return;}"
      <> "var t=code.textContent||'';"
      <> "if(t.length>32768){code.dataset.highlighted='skip';return;}"
      <> "if(window.hljs){hljs.highlightElement(code);}"
      <> "code.dataset.highlighted='1';"
      <> "}"
      <> "function highlightPane(pane){"
      <> "var code=pane.querySelector('code');"
      <> "if(code)maybeHighlight(code);"
      <> "}"
      <> "document.querySelectorAll('.js-source').forEach(function(pane){"
      <> "pane.addEventListener('toggle',function(){"
      <> "if(pane.open)highlightPane(pane);"
      <> "});"
      <> "if(pane.open)highlightPane(pane);"
      <> "});"
      <> "})();"

copyScript :: Html ()
copyScript =
  script_ $
    "(function(){"
      <> "document.querySelectorAll('.js-source').forEach(function(pane){"
      <> "var btn=pane.querySelector('.js-source-copy');"
      <> "var code=pane.querySelector('code');"
      <> "if(!btn||!code)return;"
      <> "btn.addEventListener('click',function(e){"
      <> "e.preventDefault();"
      <> "e.stopPropagation();"
      <> "var text=code.textContent||'';"
      <> "navigator.clipboard.writeText(text).then(function(){"
      <> "btn.textContent='Copied!';"
      <> "btn.disabled=true;"
      <> "setTimeout(function(){btn.textContent='Copy';btn.disabled=false;},1500);"
      <> "});"
      <> "});"
      <> "});"
      <> "})();"
