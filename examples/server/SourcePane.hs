{-# LANGUAGE OverloadedStrings #-}

module SourcePane (sourceHead, sourcePane) where

import qualified Data.Text as T
import Lucid
import Lucid.Base (makeAttribute)

-- | Highlight CSS. Put in @head@ so it is ready before the pane opens.
sourceHead :: T.Text -> Html ()
sourceHead staticRoot = do
  link_ [rel_ "stylesheet", href_ (staticRoot <> "/github-dark.min.css")]
  link_ [rel_ "stylesheet", href_ (staticRoot <> "/source.css")]

-- | Collapsed pane of compiled client JS. Scripts follow the markup.
sourcePane :: T.Text -> T.Text -> Html ()
sourcePane staticRoot js = do
  details_ [class_ "js-source"] $ do
    summary_ [class_ "js-source-summary"] $ do
      span_ [class_ "js-source-label"] "JavaScript source"
      button_
        [ type_ "button"
        , class_ "js-source-copy"
        , makeAttribute "aria-label" "Copy JavaScript source"
        ]
        "Copy"
    pre_ $ code_ [class_ "language-javascript"] (toHtml js)
  script_ [src_ (staticRoot <> "/highlight.min.js")] ("" :: Html ())
  script_ ("hljs.highlightAll();" :: T.Text)
  copyScript

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
