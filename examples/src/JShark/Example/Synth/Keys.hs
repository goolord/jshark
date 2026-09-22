{-# LANGUAGE OverloadedStrings #-}

-- | The keyboard: which notes exist, what they sound like, and which
-- computer key plays them. Page and Client both read this, so neither
-- restates a note name or a frequency.
module JShark.Example.Synth.Keys (module JShark.Example.Synth.Keys) where

import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

-- | One playable note. 'keyChar' is the computer key; 'noteId' doubles as
-- the @data-note@ value and the DOM id.
data Key = Key {noteId, label, keyChar :: Text, midi :: Int, black :: Bool}

-- | The note id of a MIDI number: @60@ is @C4@, @61@ is @Cs4@.
midiNote :: Int -> Text
midiNote m = pitches !! (m `mod` 12) <> T.pack (show (m `div` 12 - 1))
 where
  pitches = ["C", "Cs", "D", "Ds", "E", "F", "Fs", "G", "Gs", "A", "As", "B"]

-- | Bottom row from C4: @z s x d c v g b h n j m , l . ; /@, then top row
-- from C5: @q 2 w 3 e r 5 t 6 y 7 u i 9 o 0 p [ = ]@. Earlier entries win
-- when a computer key is shared.
keyBindings :: [(Text, Text)]
keyBindings = row "zsxdcvgbhnjm,l.;/" 60 <> row "q2w3er5t6y7ui9o0p[=]" 72
 where
  row chars from = zip (map T.singleton chars) (map midiNote [from ..])

-- | On-screen keys C4 to G6, low to high. 'keyChar' is the first entry in
-- 'keyBindings' for that note (bottom row wins over top).
keys :: [Key]
keys = [key m (midiNote m) | m <- [60 .. 91]]
 where
  key m n = Key n (T.replace "s" "#" (T.init n)) (primaryKey n) m (T.elem 's' n)

primaryKey :: Text -> Text
primaryKey note = maybe "" fst (find ((== note) . snd) keyBindings)

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
  naturals = fromIntegral . length . filter (not . black)
  whitesBefore = naturals (takeWhile ((/= noteId k) . noteId) keys)
  whiteWidth = 100 / naturals keys

-- | An oscillator shape offered by the UI.
data Wave = Wave {waveName, waveLabel :: Text}

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

-- | Peak level at the end of attack before decay; floor for exponential
-- amplitude ramps (Web Audio cannot ramp to zero).
peakAmp, ampFloor :: Double
peakAmp = 1.0
ampFloor = 0.001

-- | How far ahead of @currentTime@ a note is scheduled. One frame of
-- slack, so an event that lands mid-frame still starts cleanly.
lookahead :: Double
lookahead = 0.015
