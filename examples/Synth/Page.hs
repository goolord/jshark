{-# LANGUAGE OverloadedStrings #-}

-- | The synthesizer shell. Static markup only; 'Synth.Client' wires it.
module Page (page) where

import qualified Data.Text as T
import Keys
import Lucid
import Lucid.Base (makeAttribute)
import Numeric (showFFloat)

-- | @staticRoot@ is the shared assets prefix; @headExtra@ / @source@ are the
-- highlighter and the JS pane.
page :: T.Text -> Html () -> Html () -> T.Text -> Html ()
page staticRoot headExtra source scriptSrc = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "JShark • Synthesizer"
    link_ [rel_ "stylesheet", href_ (staticRoot <> "/synth.css")]
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

-- | A key. A black key is absolutely positioned, and its offset comes
-- from 'blackLeft' rather than the stylesheet, so the layout has one source
-- of truth.
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
