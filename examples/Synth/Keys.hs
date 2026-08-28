{-# LANGUAGE OverloadedStrings #-}

-- | The keyboard: which notes exist, what they sound like, and which
-- computer key plays them. Page and Client both read this, so neither
-- restates a note name or a frequency.
module Keys
  ( Key (..)
  , keys
  , keyBindings
  , primaryKey
  , whiteKeys
  , Wave (..)
  , waves
  , defaultWave
  , blackLeft
  , blackWidth
  , idKeyboard
  , idMeterBar
  , idStatus
  , idCutoff
  , idResonance
  , idAttack
  , idDecay
  , idSustain
  , idRelease
  , dataNote
  , classHeld
  , classWave
  , ampFloor
  , peakAmp
  , defaultAttack
  , defaultDecay
  , defaultSustain
  , defaultRelease
  , defaultSlider
  , lookahead
  )
where

import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

-- | One playable note. 'keyChar' is the computer key; 'noteId' doubles as
-- the @data-note@ value and the DOM id.
data Key = Key
  { noteId :: Text
  , label :: Text
  , keyChar :: Text
  , midi :: Int
  , black :: Bool
  }

-- | Bottom row from C: @z s x d c v g b h n j m , l . ; /@, then top row
-- from the next C: @q 2 w 3 e r 5 t 6 y 7 u i 9 o 0 p [ = ]@. Earlier
-- entries win when a computer key is shared.
keyBindings :: [(Text, Text)]
keyBindings =
  bottomRowBindings
    ++ topRowBindings
 where
  bottomRowBindings =
    [ ("z", "C4")
    , ("s", "Cs4")
    , ("x", "D4")
    , ("d", "Ds4")
    , ("c", "E4")
    , ("v", "F4")
    , ("g", "Fs4")
    , ("b", "G4")
    , ("h", "Gs4")
    , ("n", "A4")
    , ("j", "As4")
    , ("m", "B4")
    , (",", "C5")
    , ("l", "Cs5")
    , (".", "D5")
    , (";", "Ds5")
    , ("/", "E5")
    ]
  topRowBindings =
    [ ("q", "C5")
    , ("2", "Cs5")
    , ("w", "D5")
    , ("3", "Ds5")
    , ("e", "E5")
    , ("r", "F5")
    , ("5", "Fs5")
    , ("t", "G5")
    , ("6", "Gs5")
    , ("y", "A5")
    , ("7", "As5")
    , ("u", "B5")
    , ("i", "C6")
    , ("9", "Cs6")
    , ("o", "D6")
    , ("0", "Ds6")
    , ("p", "E6")
    , ("[", "F6")
    , ("=", "Fs6")
    , ("]", "G6")
    ]

-- | On-screen keys, low to high. 'keyChar' is the first entry in
-- 'keyBindings' for that note (bottom row wins over top).
keys :: [Key]
keys =
  [ Key note noteLabel (primaryKey note) noteMidi isBlack
  | (note, noteLabel, noteMidi, isBlack) <- keySpecs
  ]

primaryKey :: Text -> Text
primaryKey note =
  maybe "" fst (find ((== note) . snd) keyBindings)

keySpecs :: [(Text, Text, Int, Bool)]
keySpecs =
  [ ("C4", "C", 60, False)
  , ("Cs4", "C#", 61, True)
  , ("D4", "D", 62, False)
  , ("Ds4", "D#", 63, True)
  , ("E4", "E", 64, False)
  , ("F4", "F", 65, False)
  , ("Fs4", "F#", 66, True)
  , ("G4", "G", 67, False)
  , ("Gs4", "G#", 68, True)
  , ("A4", "A", 69, False)
  , ("As4", "A#", 70, True)
  , ("B4", "B", 71, False)
  , ("C5", "C", 72, False)
  , ("Cs5", "C#", 73, True)
  , ("D5", "D", 74, False)
  , ("Ds5", "D#", 75, True)
  , ("E5", "E", 76, False)
  , ("F5", "F", 77, False)
  , ("Fs5", "F#", 78, True)
  , ("G5", "G", 79, False)
  , ("Gs5", "G#", 80, True)
  , ("A5", "A", 81, False)
  , ("As5", "A#", 82, True)
  , ("B5", "B", 83, False)
  , ("C6", "C", 84, False)
  , ("Cs6", "C#", 85, True)
  , ("D6", "D", 86, False)
  , ("Ds6", "D#", 87, True)
  , ("E6", "E", 88, False)
  , ("F6", "F", 89, False)
  , ("Fs6", "F#", 90, True)
  , ("G6", "G", 91, False)
  ]

-- | The naturals, which carry the layout; black keys are positioned over them.
whiteKeys :: [Key]
whiteKeys = filter (not . black) keys

-- | Width of a black key, as a percentage of the keyboard.
blackWidth :: Double
blackWidth = 4.6

-- | Left edge of a black key, percent of the keyboard width.
--
-- A black key straddles the boundary after the naturals that precede it.
-- Derived from 'keys' so that adding or removing a note cannot leave a
-- hand-written offset pointing at the wrong gap.
blackLeft :: Key -> Double
blackLeft k = whitesBefore * whiteWidth - blackWidth / 2
 where
  whitesBefore =
    fromIntegral
      (length (filter (not . black) (takeWhile ((/= noteId k) . noteId) keys)))
  whiteWidth = 100 / fromIntegral (length whiteKeys)

-- | An oscillator shape offered by the UI.
data Wave = Wave
  { waveName :: Text
  , waveLabel :: Text
  }

-- | The shape a fresh page starts on.
defaultWave :: Wave
defaultWave = Wave "sawtooth" "saw"

waves :: [Wave]
waves =
  [ defaultWave
  , Wave "square" "square"
  , Wave "triangle" "tri"
  , Wave "sine" "sine"
  ]

idKeyboard, idMeterBar, idStatus :: Text
idKeyboard = "keyboard"
idMeterBar = "meter-bar"
idStatus = "status"

idCutoff, idResonance, idAttack, idDecay, idSustain, idRelease :: Text
idCutoff = "cutoff"
idResonance = "resonance"
idAttack = "attack"
idDecay = "decay"
idSustain = "sustain"
idRelease = "release"

-- | Attribute holding a key's note, read back by the click handler.
dataNote :: Text
dataNote = "data-note"

classHeld, classWave :: Text
classHeld = "held"
classWave = "wave"

-- | Envelope defaults — attack/decay/release in seconds, sustain as level.
defaultAttack, defaultDecay, defaultSustain, defaultRelease :: Double
defaultAttack = 0.012
defaultDecay = 0.15
defaultSustain = 1.0
defaultRelease = 0.35

-- | Initial @type=\"range\"@ value string for a Haskell default.
defaultSlider :: Double -> Text
defaultSlider = T.pack . show

-- | Peak level at the end of attack before decay.
peakAmp :: Double
peakAmp = 1.0

-- | Floor for exponential amplitude ramps — Web Audio cannot ramp to zero.
ampFloor :: Double
ampFloor = 0.001

-- | How far ahead of @currentTime@ a note is scheduled. One frame of
-- slack, so an event that lands mid-frame still starts cleanly.
lookahead :: Double
lookahead = 0.015
