{-# LANGUAGE OverloadedStrings #-}

-- | The synthesizer shell. Static markup only; 'Synth.Client' wires it.
module Page (page) where

import qualified Data.Text as T
import Keys
import Lucid
import Lucid.Base (makeAttribute)
import Numeric (showFFloat)

-- | @headExtra@ / @source@ are the highlighter and the JS pane.
page :: Html () -> Html () -> T.Text -> Html ()
page headExtra source scriptSrc = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "JShark • Synthesizer"
    style_ css
    headExtra
  body_ $ do
    main_ [class_ "synth"] $ do
      h1_ "synth"
      p_ [class_ "hint"] $ do
        "Click the keys or use your keyboard ("
        code_ "a"
        "–"
        code_ "k"
        "). Notes are held until you let go."
      div_ [class_ "controls"] $ do
        fieldset_ $ do
          legend_ "wave"
          div_ [class_ "waves"] (mapM_ waveButton waves)
        fieldset_ $ do
          legend_ "filter"
          slider idCutoff "cutoff" "200" "8000" "2200"
          slider idResonance "resonance" "0" "20" "6"
        fieldset_ $ do
          legend_ "envelope"
          slider idRelease "release" "0.05" "1.5" "0.35"
      div_ [class_ "meter"] $ div_ [id_ idMeterBar, class_ "meter-bar"] mempty
      div_ [id_ idKeyboard, class_ "keyboard"] (mapM_ keyButton keys)
      p_ [id_ idStatus, class_ "status"] "click a key to start audio"
    footer_ [class_ "info"] $
      p_ $ do
        "Web Audio via JShark's "
        code_ "ffi"
        ". Envelopes and pitch are "
        code_ "AudioParam"
        " automation, so timing lives on the audio thread."
    source
    script_ [src_ scriptSrc] ("" :: Html ())

{- | A key. A black key is absolutely positioned, and its offset comes
from 'blackLeft' rather than the stylesheet, so the layout has one source
of truth.
-}
keyButton :: Key -> Html ()
keyButton k =
  button_
    ( [ id_ (noteId k)
      , class_ (if black k then "key black" else "key white")
      , makeAttribute dataNote (noteId k)
      , type_ "button"
      ]
        ++ [style_ ("left:" <> pct (blackLeft k)) | black k]
    )
    $ do
      span_ [class_ "note"] (toHtml (label k))
      span_ [class_ "kbd"] (toHtml (keyChar k))

-- | A percentage, for a style attribute.
pct :: Double -> T.Text
pct x = T.pack (showFFloat (Just 2) x "") <> "%"

waveButton :: Wave -> Html ()
waveButton w =
  button_
    [ id_ ("wave-" <> waveName w)
    , class_ classWave
    , makeAttribute "data-wave" (waveName w)
    , type_ "button"
    ]
    (toHtml (waveLabel w))

slider :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> Html ()
slider sliderId caption lo hi initial =
  label_ [class_ "slider"] $ do
    span_ (toHtml caption)
    input_
      [ id_ sliderId
      , type_ "range"
      , min_ lo
      , max_ hi
      , step_ "any"
      , value_ initial
      ]

css :: T.Text
css =
  T.intercalate
    "\n"
    [ ":root{--ink:#e7e7ea;--bg:#131317;--dim:#8b8b96;--hot:#8ef0c8}"
    , "*{box-sizing:border-box}"
    , "body{margin:0;background:var(--bg);color:var(--ink);"
        <> "font:15px/1.5 ui-sans-serif,system-ui,sans-serif}"
    , ".synth{max-width:720px;margin:0 auto;padding:32px 20px}"
    , "h1{font-size:20px;letter-spacing:.18em;text-transform:uppercase;margin:0 0 4px}"
    , ".hint{color:var(--dim);margin:0 0 20px}"
    , "code{background:#20202a;padding:1px 5px;border-radius:4px;font-size:13px}"
    , ".controls{display:flex;gap:16px;flex-wrap:wrap;margin-bottom:18px}"
    , "fieldset{border:1px solid #2a2a35;border-radius:8px;padding:10px 14px;margin:0}"
    , "legend{color:var(--dim);font-size:11px;letter-spacing:.14em;text-transform:uppercase}"
    , ".waves{display:flex;gap:6px}"
    , ".wave{background:#20202a;color:var(--ink);border:1px solid #2f2f3b;"
        <> "border-radius:6px;padding:5px 10px;cursor:pointer;font:inherit;font-size:13px}"
    , ".wave.on{background:var(--hot);color:#10241c;border-color:var(--hot)}"
    , ".slider{display:flex;align-items:center;gap:8px;font-size:12px;color:var(--dim)}"
    , ".slider span{min-width:68px}"
    , ".slider input{width:150px}"
    , ".meter{height:6px;background:#20202a;border-radius:3px;overflow:hidden;margin-bottom:22px}"
    , ".meter-bar{height:100%;width:0;background:var(--hot);transition:width 60ms linear}"
    , ".keyboard{position:relative;display:flex;height:180px;user-select:none;"
        <> "touch-action:none}"
    , ".key{position:relative;border:0;cursor:pointer;font:inherit;display:flex;"
        <> "flex-direction:column;justify-content:flex-end;align-items:center;"
        <> "gap:6px;padding-bottom:10px}"
    , -- A 1px separator rather than a margin, so a white key is exactly
      -- an eighth of the keyboard and 'blackLeft' lands where it says.
      ".key.white{flex:1;background:#f2f2f5;color:#2a2a35;border-radius:0 0 5px 5px;"
        <> "border-right:1px solid #d3d3dc}"
    , ".key.black{position:absolute;z-index:2;width:"
        <> pct blackWidth
        <> ";height:62%;background:#1b1b22;"
        <> "color:#d8d8e0;border-radius:0 0 4px 4px;border:1px solid #33333f}"
    , ".key.held{background:var(--hot);color:#10241c}"
    , ".note{font-size:12px;font-weight:600}"
    , ".kbd{font-size:10px;opacity:.55;text-transform:uppercase}"
    , ".status{color:var(--dim);font-size:12px;margin-top:16px}"
    , ".info{max-width:720px;margin:0 auto;padding:0 20px 40px;color:var(--dim);"
        <> "font-size:13px}"
    ]
