{-# LANGUAGE OverloadedStrings #-}

{- | The keyboard: which notes exist, what they sound like, and which
computer key plays them. Page and Client both read this, so neither
restates a note name or a frequency.
-}
module Keys
  ( Key (..)
  , keys
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
  , idRelease
  , dataNote
  , classHeld
  , classWave
  , attack
  , sustain
  , lookahead
  )
where

import Data.Text (Text)

{- | One playable note. 'keyChar' is the computer key; 'noteId' doubles as
the @data-note@ value and the DOM id.
-}
data Key = Key
  { noteId :: Text
  , label :: Text
  , keyChar :: Text
  , midi :: Int
  , black :: Bool
  }

-- | An octave and a bit of C major, laid out like a tracker keyboard.
keys :: [Key]
keys =
  [ Key "C4" "C" "a" 60 False
  , Key "Cs4" "C#" "w" 61 True
  , Key "D4" "D" "s" 62 False
  , Key "Ds4" "D#" "e" 63 True
  , Key "E4" "E" "d" 64 False
  , Key "F4" "F" "f" 65 False
  , Key "Fs4" "F#" "t" 66 True
  , Key "G4" "G" "g" 67 False
  , Key "Gs4" "G#" "y" 68 True
  , Key "A4" "A" "h" 69 False
  , Key "As4" "A#" "u" 70 True
  , Key "B4" "B" "j" 71 False
  , Key "C5" "C" "k" 72 False
  ]

-- | The naturals, which carry the layout; black keys are positioned over them.
whiteKeys :: [Key]
whiteKeys = filter (not . black) keys

-- | Width of a black key, as a percentage of the keyboard.
blackWidth :: Double
blackWidth = 5.4

{- | Left edge of a black key, percent of the keyboard width.

A black key straddles the boundary after the naturals that precede it.
Derived from 'keys' so that adding or removing a note cannot leave a
hand-written offset pointing at the wrong gap.
-}
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

idCutoff, idResonance, idRelease :: Text
idCutoff = "cutoff"
idResonance = "resonance"
idRelease = "release"

-- | Attribute holding a key's note, read back by the click handler.
dataNote :: Text
dataNote = "data-note"

classHeld, classWave :: Text
classHeld = "held"
classWave = "wave"

{- | Envelope shape, in seconds. Attack is short enough to feel immediate
and long enough to avoid a click; 'sustain' is the level a held note
settles at.
-}
attack :: Double
attack = 0.012

sustain :: Double
sustain = 0.22

{- | How far ahead of @currentTime@ a note is scheduled. One frame of
slack, so an event that lands mid-frame still starts cleanly.
-}
lookahead :: Double
lookahead = 0.015
