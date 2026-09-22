{-# LANGUAGE OverloadedStrings #-}

-- | Synth shell. Static markup; 'Synth.Client' wires it.
module JShark.Example.Synth.Page (page) where

import qualified Data.Text as T
import JShark.Example.Synth.Keys
import JShark.Example.Theme (ExamplePage, themedPage)
import Lucid
import Lucid.Base (makeAttribute)

page :: ExamplePage
page = themedPage "Synth" ["/css/synth.css", "/css/synth-keys.css"] $
  main_ [class_ "page synth"] $ do
    header_ [class_ "page-header"] $ do
      h1_ "Synth"
      p_ [class_ "page-hint"] $
        "z–/ then q–] on keyboard · click keys · held until release"
    div_ [class_ "controls"] $ do
      controlGroup "wave" "Wave" "waves" (mapM_ waveButton waves)
      controlGroup "filter" "Filter" "sliders" $ do
        slider idCutoff "Cutoff" "200" "8000" "2200"
        slider idResonance "Res" "0" "20" "6"
      controlGroup "envelope" "Envelope" "sliders" $ do
        slider idAttack "Attack" "0.001" "3" (showT defaultAttack)
        slider idDecay "Decay" "0.01" "1.5" (showT defaultDecay)
        slider idSustain "Sustain" "0" "1" (showT defaultSustain)
        slider idRelease "Release" "0.05" "9" (showT defaultRelease)
    div_ [class_ "meter"] $ div_ [id_ idMeterBar, class_ "meter-bar"] mempty
    div_ [id_ idKeyboard, class_ "keyboard"] (mapM_ keyButton keys)
    p_ [id_ idStatus, class_ "status"] "Click a key to start audio"
    footer_ [class_ "page-footer"] $ p_ "Web Audio via JShark FFI"
 where
  showT = T.pack . show

-- | A captioned group of controls, labelled by its caption for assistive tech.
controlGroup :: T.Text -> T.Text -> T.Text -> Html () -> Html ()
controlGroup name caption cls controls = div_ [class_ "control-group"] $ do
  p_ [class_ "control-label", id_ ref] (toHtml caption)
  div_ [class_ cls, role_ "group", makeAttribute "aria-labelledby" ref] controls
 where
  ref = "synth-" <> name <> "-label"

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
slider i caption lo hi v = label_ [class_ "slider"] $ do
  span_ (toHtml caption)
  input_ [id_ i, type_ "range", min_ lo, max_ hi, step_ "any", value_ v]
