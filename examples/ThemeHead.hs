{-# LANGUAGE OverloadedStrings #-}

-- | Shared stylesheet links for example pages.
module ThemeHead (themeLinks, sourceLinks, sourceLinksLite) where

import qualified Data.Text as T
import Lucid

-- | Pico + theme tokens + base layout. Example shells link this in @head_@.
themeLinks :: T.Text -> Html ()
themeLinks staticRoot = do
  link_ [rel_ "stylesheet", href_ (staticRoot <> "/pico/pico.min.css")]
  link_ [rel_ "stylesheet", href_ (staticRoot <> "/tokens.css")]
  link_ [rel_ "stylesheet", href_ (staticRoot <> "/base.css")]

-- | Source pane + highlighter. Link after 'themeLinks' on themed pages.
sourceLinks :: T.Text -> Html ()
sourceLinks staticRoot = do
  link_
    [ rel_ "stylesheet"
    , href_ (staticRoot <> "/speed-highlight/themes/github-dark.css")
    ]
  link_ [rel_ "stylesheet", href_ (staticRoot <> "/source.css")]

-- | TodoMVC and other pages that skip 'themeLinks'.
sourceLinksLite :: T.Text -> Html ()
sourceLinksLite staticRoot = do
  link_ [rel_ "stylesheet", href_ (staticRoot <> "/tokens.css")]
  sourceLinks staticRoot
