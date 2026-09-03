{-# LANGUAGE OverloadedStrings #-}

-- | Shared stylesheet links for example pages.
module ThemeHead
  ( themeLinks
  , sourceLinks
  , sourceLinksLite
  , githubCorner
  , hotReloadClient
  )
where

import qualified Data.Text as T
import JShark.Lucid.HotReload (hotReloadClient)
import Lucid
import Lucid.Base (makeAttribute)

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

-- | Floating corner link back to the JShark repo. Put it at the top of
--   @body_@ on every example shell. Self-contained (inline style + SVG
--   dimensions) so it renders as a compact fixed chip even if no stylesheet
--   loads on the page.
githubCorner :: Html ()
githubCorner =
  a_
    [ class_ "github-corner"
    , href_ githubRepo
    , title_ "JShark source on GitHub"
    , makeAttribute "target" "_blank"
    , makeAttribute "rel" "noopener noreferrer"
    , makeAttribute "aria-label" "JShark source on GitHub"
    , makeAttribute "style" githubCornerStyle
    ]
    $ do
      toHtmlRaw githubMarkSvg
      span_ "GitHub"

githubRepo :: T.Text
githubRepo = "https://github.com/goolord/jshark"

-- | Fixed dark pill that reads on both the dark themed shells and the light
--   TodoMVC page.
githubCornerStyle :: T.Text
githubCornerStyle =
  "position:fixed;top:0.6rem;right:0.6rem;z-index:1000;"
    <> "display:inline-flex;align-items:center;gap:0.4rem;"
    <> "padding:0.3rem 0.6rem;border:1px solid #404040;border-radius:3px;"
    <> "background:#111;color:#f0f0f0;"
    <> "font:600 0.8125rem/1 ui-sans-serif,system-ui,sans-serif;"
    <> "text-decoration:none;box-shadow:0 0 0 1px rgba(0,0,0,0.25)"

-- | GitHub mark (@mark-github@, 16×16). Explicit width/height and
--   @fill='currentColor'@ (inherits the anchor's light text color) so an
--   unstyled page still gets a small white icon, not a 300×150 black SVG.
githubMarkSvg :: T.Text
githubMarkSvg =
  "<svg viewBox='0 0 16 16' width='16' height='16' fill='currentColor' aria-hidden='true'><path fill-rule='evenodd' \
    \d='M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 \
    \0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 \
    \1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 \
    \1.32-.27 2-.27s1.36.09 2 .27c1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 \
    \1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.01 8.01 0 0 0 16 8c0-4.42-3.58-8-8-8Z'/></svg>"
