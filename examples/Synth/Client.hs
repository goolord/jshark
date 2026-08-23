{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | A polyphonic synthesizer.
--
-- The graph is built once; every voice feeds a shared filter, which feeds a
-- master gain and an analyser.
--
-- @
-- osc --> vca --\\
--                filter --> master --> analyser --> speakers
-- osc --> vca --/
-- @
--
-- Handlers only start and release voices. Nothing about the sound is timed
-- from JavaScript: pitch and the amplitude envelope are @AudioParam@
-- automation, so they run on the audio thread whatever the main thread is
-- doing. The only per-frame work is repainting the meter, where a dropped
-- frame costs a frame of animation rather than a click in the audio.
module Client (mainJS, Settings) where

import qualified Audio
import Data.Text (Text)
import GHC.Generics (Generic)
import JShark.Api
import qualified JShark.Dom as Dom
import JShark.Generic (newRecord)
import JShark.Object (field, obj)
import qualified JShark.Object as Object
import JShark.Rec (Rec (..), (<:))
import qualified JShark.Timers as Timers
import Keys

-- | What the controls hold. Cutoff and resonance are absent on purpose:
-- they go straight into the filter when a slider moves, so the graph is
-- their state.
data Settings = Settings
  { wave :: Text
  , release :: Double
  }
  deriving Generic

-- | A DOM event. Its fields are read with 'getProp'', which needs the
-- universe pinned, so handlers name this rather than leaving it open.
type Event f = Expr f ('MutableObject ())

-- | Absence of a note, as the key table reports it.
noNote :: Expr f 'String
noNote = ""

-- | Analyser bins, and so the size of the meter's buffer.
meterBins :: Int
meterBins = 32

byId :: Text -> EffectSyntax f (Effect f ('MutableObject Dom.DomElement))
byId = Dom.lookupId . string

-- | @Number(x)@: a slider's @value@ arrives as a string.
numberOf :: Expr f 'String -> EffectSyntax f (Expr f 'Number)
numberOf s = fmap var (toSyntax (ffi "Number" (arg s <: RecNil)))

-- | Run the block only when the option is empty.
whenNoneS ::
  Expr f ('Option u) -> EffectSyntax f (f 'Unit) -> EffectSyntax f (f 'Unit)
whenNoneS opt body = toSyntax (optionCaseE opt (discard (stmts body)) (\_ -> noOp))

-- | Which note a computer key plays, or @""@. A switch over the table the
-- Haskell side already has, so the mapping cannot drift from 'Keys.keys'.
noteForKey :: Expr f 'String -> Effect f 'String
noteForKey k =
  stringCaseE
    k
    [(keyChar key, expr (string (noteId key))) | key <- keys]
    (expr noNote)

-- | Equal temperament, worked out in Haskell and emitted as a switch.
freqForNote :: Expr f 'String -> Effect f 'Number
freqForNote n =
  stringCaseE
    n
    [(noteId key, expr (number (hz key))) | key <- keys]
    (expr (number 0))
 where
  hz key = 440 * (2 ** ((fromIntegral (midi key) - 69) / 12))

mainJS :: forall f. EffectSyntax f (f 'Unit)
mainJS = do
  keyboard <- byId idKeyboard
  meterBar <- byId idMeterBar
  status <- byId idStatus
  cutoffEl <- byId idCutoff
  resonanceEl <- byId idResonance
  releaseEl <- byId idRelease
  waveEls <- traverse (\w -> (,) w <$> byId ("wave-" <> waveName w)) waves

  st <- hold (newRecord @Settings)
  set @"wave" st (string (waveName defaultWave))
  set @"release" st (number 0.35)

  -- One voice table for the session, keyed by note. Pointers are tracked
  -- separately, by pointerId, so two fingers hold two notes.
  voices <- hold (Object.newObject :: Effect f ('MutableObject ()))
  pointers <- hold (Object.newObject :: Effect f ('MutableObject ()))

  ctx <- Audio.newAudioContext
  filt <- Audio.biquadFilter ctx
  master <- Audio.gain ctx
  comp <- Audio.compressor ctx
  ana <- Audio.analyser ctx
  spectrum <- Audio.analysisBuffer meterBins

  Audio.setType filt (string "lowpass")
  Audio.connect filt master
  Audio.connect master comp
  Audio.connect comp ana
  Audio.connect
    ana
    (Audio.destination ctx :: Effect f ('MutableObject Audio.Node))
  Audio.setValue (Audio.param master "gain") (number 0.8)
  -- Sized to the buffer, so the meter sees the whole spectrum rather than
  -- the bottom of it.
  Audio.setFftSize ana (Audio.fftSizeFor meterBins)

  cutoff0 <- Dom.getValue cutoffEl >>= numberOf
  Audio.setValue (Audio.param filt "frequency") cutoff0
  resonance0 <- Dom.getValue resonanceEl >>= numberOf
  Audio.setValue (Audio.param filt "Q") resonance0

  let
    voiceOf :: Expr f 'String -> Effect f ('Option ('MutableObject Audio.Voice))
    voiceOf = Audio.dictGet voices

    -- A context starts suspended, and only a gesture may resume it. The
    -- browser drops a @once@ listener after it fires, so this costs
    -- nothing per note.
    startAudio :: EffectSyntax f (f 'Unit)
    startAudio = do
      Audio.resume ctx
      state <- Audio.contextState ctx
      Dom.setInnerText status (string "audio " <> state)

    noteOn :: Expr f 'String -> EffectSyntax f (f 'Unit)
    noteOn note = do
      existing <- toSyntax (voiceOf note)
      whenNoneS (var existing) $ do
        now <- Audio.currentTime ctx
        let
          t0 = now + number lookahead
        osc <- Audio.oscillator ctx
        w <- st.wave
        Audio.setType osc w
        hz <- toSyntax (freqForNote note)
        Audio.setValueAt (Audio.param osc "frequency") (var hz) t0

        vca <- Audio.gain ctx
        let
          amp = Audio.param vca "gain"
        Audio.setValueAt amp (number 0) t0
        Audio.rampTo amp (number sustain) (t0 + number attack)

        Audio.connect osc vca
        Audio.connect vca filt
        Audio.startAt osc t0
        -- The oscillator is collected once stopped; its amplifier is not,
        -- so take it out of the graph when the note is really over.
        Audio.onEnded osc (Audio.disconnect vca >> done)

        voice <-
          toSyntax
            ( obj [field @"osc" osc, field @"vca" vca] ::
                Effect f ('MutableObject Audio.Voice)
            )
        Audio.dictSet voices note (var voice)

        el <- Dom.lookupId note
        Dom.classAdd el (string classHeld)

    noteOff :: Expr f 'String -> EffectSyntax f (f 'Unit)
    noteOff note = do
      found <- toSyntax (voiceOf note)
      whenSomeS (var found) $ \voice -> do
        now <- Audio.currentTime ctx
        rel <- st.release
        osc <- Object.get @"osc" (expr voice)
        vca <- Object.get @"vca" (expr voice)
        let
          amp = Audio.param vca "gain"
        -- Drop the queued attack and restart from wherever the ramp got
        -- to; without the hold the level would jump before falling.
        current <- Audio.paramValue amp
        Audio.cancelFrom amp now
        Audio.setValueAt amp current now
        Audio.rampTo amp (number 0) (now + rel)
        Audio.stopAt osc (now + rel + number 0.05)
        toSyntax_ (Object.delete voices note)
        el <- Dom.lookupId note
        Dom.classRemove el (string classHeld)

  Audio.listenOnce "pointerdown" keyboard startAudio
  Audio.listenOnce "keydown" window startAudio

  -- Pointer: press a key, release wherever the pointer ends up. Keyed by
  -- pointerId so a second finger does not evict the first note.
  addEventListener "pointerdown" keyboard $ \(ev :: Event f) -> stmts $ do
    target <- getProp' ev "target"
    hit <-
      toSyntax (callMethod (expr target) "closest" (arg (string ".key") <: RecNil))
    whenSomeS (unsafeNullable (var hit)) $ \el -> do
      note <-
        toSyntax
          (callMethod (expr el) "getAttribute" (arg (string dataNote) <: RecNil))
      pid <- getProp' ev "pointerId"
      Audio.dictSet pointers (toString pid) (var note)
      noteOn (var note)

  let
    -- A pointer can also be cancelled (gesture taken over, touch lost),
    -- which must release the note as surely as a clean release does.
    releasePointer :: Event f -> Effect f 'Unit
    releasePointer ev = stmts $ do
      pid <- getProp' ev "pointerId"
      found <-
        toSyntax (Audio.dictGet pointers (toString pid) :: Effect f ('Option 'String))
      whenSomeS (var found) $ \note -> do
        toSyntax_ (Object.delete pointers (toString pid))
        noteOff note

  addEventListener "pointerup" window releasePointer
  addEventListener "pointercancel" window releasePointer

  -- Losing focus means no keyup is coming, which would leave notes on.
  addEventListener_ "blur" window $ do
    Audio.forEachKey voices noteOff
    done

  -- Auto-repeat would retrigger a note that is already sounding.
  addEventListener "keydown" window $ \(ev :: Event f) -> stmts $ do
    repeated <- getProp' ev "repeat"
    key <- getProp' ev "key"
    note <- toSyntax (noteForKey key)
    whenS (repeated .!= true_ .&& var note .!= noNote) $ do
      toSyntax_ (callMethod (expr ev) "preventDefault" RecNil)
      noteOn (var note)

  addEventListener "keyup" window $ \(ev :: Event f) -> stmts $ do
    key <- getProp' ev "key"
    note <- toSyntax (noteForKey key)
    whenS (var note .!= noNote) (noteOff (var note))

  -- Live edits land on the shared filter, so held notes follow them.
  addEventListener_ "input" cutoffEl $ do
    v <- Dom.getValue cutoffEl >>= numberOf
    Audio.setValue (Audio.param filt "frequency") v

  addEventListener_ "input" resonanceEl $ do
    v <- Dom.getValue resonanceEl >>= numberOf
    Audio.setValue (Audio.param filt "Q") v

  addEventListener_ "input" releaseEl $ do
    v <- Dom.getValue releaseEl >>= numberOf
    set @"release" st v

  mapM_
    ( \(w, el) -> addEventListener_ "click" el $ do
        set @"wave" st (string (waveName w))
        mapM_ (markWave (waveName w)) waveEls
        done
    )
    waveEls

  -- The only per-frame work: read the analyser, resize one bar.
  Timers.foreverFrame $ \_ -> do
    Audio.byteFrequencyData ana spectrum
    level <- Audio.meanByte spectrum
    setProp meterBar "style.width" (toString (level * number 100) <> string "%")

-- | @el.classList.toggle("on", isChosen)@ — one call per button, no
-- branch, so the emitted JS stays flat.
markWave ::
  Text
  -> (Wave, Effect f ('MutableObject Dom.DomElement))
  -> EffectSyntax f (f 'Unit)
markWave chosen (w, el) =
  toSyntax
    ( callMethod
        el
        "classList.toggle"
        (arg (string "on") <: arg (bool (waveName w == chosen)) <: RecNil)
    )
