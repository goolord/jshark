{-# LANGUAGE OverloadedStrings #-}

-- | The synthesizer shell. Static markup only; 'Synth.Client' wires it.
module Page (page) where

import qualified Data.Text as T
import Keys
import Lucid
import Lucid.Base (makeAttribute)
import ThemeHead (themeLinks)

idWaveLabel, idFilterLabel, idEnvelopeLabel :: T.Text
idWaveLabel = "synth-wave-label"
idFilterLabel = "synth-filter-label"
idEnvelopeLabel = "synth-envelope-label"

-- | @staticRoot@ is the shared assets prefix; @headExtra@ / @source@ are the
-- highlighter and the JS pane.
page :: T.Text -> Html () -> Html () -> T.Text -> Html ()
page staticRoot headExtra source scriptSrc = doctypehtml_ $
  html_ [makeAttribute "data-theme" "dark"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      title_ "Synth"
      themeLinks staticRoot
      link_ [rel_ "stylesheet", href_ (staticRoot <> "/synth.css")]
      link_ [rel_ "stylesheet", href_ (staticRoot <> "/synth-keys.css")]
      headExtra
    body_ $ do
      main_ [class_ "page synth"] $ do
        header_ [class_ "page-header"] $ do
          h1_ "Synth"
          p_ [class_ "page-hint"] $
            "z–/ then q–] on keyboard · click keys · held until release"
        div_ [class_ "controls"] $ do
          div_ [class_ "control-group"] $ do
            p_ [class_ "control-label", id_ idWaveLabel] "Wave"
            div_
              [ class_ "waves"
              , role_ "group"
              , makeAttribute "aria-labelledby" idWaveLabel
              ]
              (mapM_ waveButton waves)
          div_ [class_ "control-group"] $ do
            p_ [class_ "control-label", id_ idFilterLabel] "Filter"
            div_
              [ class_ "sliders"
              , role_ "group"
              , makeAttribute "aria-labelledby" idFilterLabel
              ]
              $ do
                slider idCutoff "Cutoff" "200" "8000" "2200"
                slider idResonance "Res" "0" "20" "6"
          div_ [class_ "control-group"] $ do
            p_ [class_ "control-label", id_ idEnvelopeLabel] "Envelope"
            div_
              [ class_ "sliders"
              , role_ "group"
              , makeAttribute "aria-labelledby" idEnvelopeLabel
              ]
              $ slider idRelease "Release" "0.05" "1.5" "0.35"
        div_ [class_ "meter"] $ div_ [id_ idMeterBar, class_ "meter-bar"] mempty
        div_ [id_ idKeyboard, class_ "keyboard"] (mapM_ keyButton keys)
        p_ [id_ idStatus, class_ "status"] "Click a key to start audio"
        footer_ [class_ "page-footer"] $
          p_ "Web Audio via JShark FFI · envelopes on the audio thread"
      source
      script_ [src_ scriptSrc] ("" :: Html ())

-- | Black key offsets live in @synth-keys.css@ (must match 'blackLeft').
keyButton :: Key -> Html ()
keyButton k =
  button_
    [ id_ (noteId k)
    , class_ (if black k then "key black" else "key white")
    , makeAttribute dataNote (noteId k)
    , type_ "button"
    ]
    $ do
      span_ [class_ "note"] (toHtml (label k))
      span_ [class_ "kbd"] (toHtml (keyChar k))

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
