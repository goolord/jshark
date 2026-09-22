{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
module JShark.Example.Synth.Client (mainJS, Settings) where

import Control.Monad (forM_)
import Data.Text (Text)
import GHC.Generics (Generic)
import JShark.Api
import JShark.Api.Generic (newRecord)
import JShark.Api.Rec (Rec (..), (<:))
import qualified JShark.Dom as Dom
import qualified JShark.Example.Synth.Audio as Audio
import JShark.Example.Synth.Keys
import JShark.Object (field, obj)
import qualified JShark.Object as Object
import qualified JShark.Timers as Timers

-- | What the controls hold. Cutoff and resonance are absent on purpose:
-- they go straight into the filter when a slider moves, so the graph is
-- their state.
data Settings = Settings
  { wave :: Text
  , attack, decay, sustainLevel, release :: Double
  }
  deriving Generic

type Element f = Effect f ('MutableObject Dom.DomElement)

type Stmt f = EffectSyntax f (f 'Unit)

-- | Absence of a note, as the key table reports it.
noNote :: Expr f 'String
noNote = ""

-- | Analyser bins, and so the size of the meter's buffer.
meterBins :: Int
meterBins = 32

peak, minAmp :: Expr f 'Number
peak = number peakAmp
minAmp = number ampFloor

-- | Paint the filled portion of a range input via @--range-fill@.
syncRangeFill :: Element f -> Stmt f
syncRangeFill el = do
  v <- Dom.getValue el >>= toNumber
  lo <- getProp el "min" >>= toNumber
  hi <- getProp el "max" >>= toNumber
  whenS (hi .== lo) (setProp el "style.--range-fill" (string "0%"))
  whenS (hi .!= lo)
    $ setProp el "style.--range-fill"
    $ toString (((v - lo) / (hi - lo)) * number 100) <> string "%"

-- | A slider's current value, handed to whatever it controls.
readSlider :: (Element f, Expr f 'Number -> Stmt f) -> Stmt f
readSlider (el, k) = Dom.getValue el >>= toNumber >>= k

-- | Which note a computer key plays, or @""@. A switch over the table the
-- Haskell side already has, so the mapping cannot drift from 'Keys.keys'.
noteForKey :: Expr f 'String -> Effect f 'String
noteForKey k =
  stringCaseE k [(char, expr (string note)) | (char, note) <- keyBindings] $
    expr noNote

-- | Equal temperament, worked out in Haskell and emitted as a switch.
freqForNote :: Expr f 'String -> Effect f 'Number
freqForNote n =
  stringCaseE n [(noteId key, expr (number (hz key))) | key <- keys] $
    expr (number 0)
 where
  hz key = 440 * (2 ** ((fromIntegral (midi key) - 69) / 12))

mainJS :: forall f. Stmt f
mainJS = do
  keyboard <- Dom.byId idKeyboard
  meterBar <- Dom.byId idMeterBar
  status <- Dom.byId idStatus
  sliderEls <- traverse Dom.byId sliderIds
  waveEls <- traverse (\w -> (,) w <$> Dom.byId ("wave-" <> waveName w)) waves

  st <- hold (newRecord @Settings)
  set @"wave" st (string (waveName defaultWave))
  set @"attack" st (number defaultAttack)
  set @"decay" st (number defaultDecay)
  set @"sustainLevel" st (number defaultSustain)
  set @"release" st (number defaultRelease)

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
  Audio.connect ana (Audio.destination ctx)
  Audio.setValue (Audio.param master "gain") (number 0.8)
  -- Sized to the buffer, so the meter sees the whole spectrum rather than
  -- the bottom of it.
  Audio.setFftSize ana (Audio.fftSizeFor meterBins)

  let
    sliders =
      zip
        sliderEls
        [ Audio.setValue (Audio.param filt "frequency")
        , Audio.setValue (Audio.param filt "Q")
        , set @"attack" st
        , set @"decay" st
        , set @"sustainLevel" st
        , set @"release" st
        ]
  -- Browsers restore range values; Settings must match the sliders, not
  -- the HTML defaults, or the envelope ignores the restored thumbs.
  mapM_ readSlider sliders

  let
    voiceOf :: Expr f 'String -> Effect f ('Option ('MutableObject Audio.Voice))
    voiceOf = Audio.dictGet voices

    -- A context starts suspended, and only a gesture may resume it. The
    -- browser drops a @once@ listener after it fires, so this costs
    -- nothing per note.
    startAudio :: Stmt f
    startAudio = do
      Audio.resume ctx
      state <- Audio.contextState ctx
      Dom.setInnerText status (string "audio " <> state)

    noteOn :: Expr f 'String -> Stmt f
    noteOn note = do
      existing <- toSyntax (voiceOf note)
      whenNoneS (var existing) $ do
        now <- Audio.currentTime ctx
        let
          t0 = now + number lookahead
        osc <- Audio.oscillator ctx
        st.wave >>= Audio.setType osc
        hz <- toSyntax (freqForNote note)
        Audio.setValueAt (Audio.param osc "frequency") (var hz) t0
        vca <- Audio.gain ctx
        atk <- get @"attack" st
        dec <- get @"decay" st
        sus <- get @"sustainLevel" st
        Audio.scheduleAdsr (Audio.param vca "gain") t0 atk dec sus peak minAmp
        Audio.connect osc vca
        Audio.connect vca filt
        Audio.startAt osc t0
        -- The oscillator is collected once stopped; its amplifier is not,
        -- so take it out of the graph when the note is really over.
        Audio.onEnded osc (Audio.disconnect vca >> done)
        voice <-
          toSyntax
            ( obj
                [ field @"osc" osc
                , field @"vca" vca
                , field @"t0" t0
                , field @"atk" atk
                , field @"dec" dec
                , field @"sus" sus
                ] ::
                Effect f ('MutableObject Audio.Voice)
            )
        Audio.dictSet voices note (var voice)
        el <- Dom.lookupId note
        Dom.classAdd el (string classHeld)

    noteOff :: Expr f 'String -> Stmt f
    noteOff note = do
      found <- toSyntax (voiceOf note)
      whenSomeS (var found) $ \voice -> do
        now <- Audio.currentTime ctx
        rel <- st.release
        osc <- Object.get @"osc" (expr voice)
        vca <- Object.get @"vca" (expr voice)
        t0 <- get @"t0" (expr voice)
        atk <- get @"atk" (expr voice)
        dec <- get @"dec" (expr voice)
        sus <- get @"sus" (expr voice)
        let
          amp = Audio.param vca "gain"
        Audio.releaseVoice amp osc now rel minAmp t0 atk dec sus peak
        toSyntax_ (Object.delete voices note)
        el <- Dom.lookupId note
        Dom.classRemove el (string classHeld)

    -- A pointer can also be cancelled (gesture taken over, touch lost),
    -- which must release the note as surely as a clean release does.
    releasePointer :: Expr f ('MutableObject Event) -> Effect f 'Unit
    releasePointer ev = stmts $ do
      pid <- eventPointerId ev
      found <-
        toSyntax (Audio.dictGet pointers (toString pid) :: Effect f ('Option 'String))
      whenSomeS (var found) $ \note -> do
        toSyntax_ (Object.delete pointers (toString pid))
        noteOff note

  Audio.listenOnce "pointerdown" keyboard startAudio
  Audio.listenOnce "keydown" window startAudio

  -- Pointer: press a key, release wherever the pointer ends up. Keyed by
  -- pointerId so a second finger does not evict the first note.
  addEventListenerS "pointerdown" keyboard $ \ev -> do
    target <- Dom.eventTarget ev
    hit <-
      toSyntax (callMethod target "closest" (arg (string ".key") <: RecNil))
    whenSomeS (unsafeNullable (var hit)) $ \el -> do
      note <-
        toSyntax
          (callMethod (expr el) "getAttribute" (arg (string dataNote) <: RecNil))
      pid <- eventPointerId ev
      Audio.dictSet pointers (toString pid) (var note)
      noteOn (var note)
  addEventListener "pointerup" window releasePointer
  addEventListener "pointercancel" window releasePointer

  -- Losing focus means no keyup is coming, which would leave notes on.
  addEventListener_ "blur" window $ Audio.forEachKey voices noteOff *> done

  -- Auto-repeat would retrigger a note that is already sounding.
  addEventListenerS "keydown" window $ \ev -> do
    repeated <- eventRepeat ev
    key <- eventKey ev
    note <- toSyntax (noteForKey key)
    whenS (repeated .!= true_ .&& var note .!= noNote) $ do
      toSyntax_ (callMethod (expr ev) "preventDefault" RecNil)
      noteOn (var note)
  addEventListenerS "keyup" window $ \ev -> do
    key <- eventKey ev
    note <- toSyntax (noteForKey key)
    whenS (var note .!= noNote) (noteOff (var note))

  -- Live edits land on the shared filter, so held notes follow them.
  forM_ sliders $ \s@(el, _) ->
    addEventListener_ "input" el (syncRangeFill el *> readSlider s)
  mapM_ (syncRangeFill . fst) sliders

  forM_ waveEls $ \(w, el) -> addEventListener_ "click" el $ do
    set @"wave" st (string (waveName w))
    mapM_ (markWave (waveName w)) waveEls
    done

  -- Register a dispose hook so a hot reload tears the audio graph down
  -- instead of leaking the old AudioContext (and its oscillators).
  toSyntax_ $
    ffi
      "ctx => { window.__JSHARK_DISPOSE__ = function () { try { ctx.close(); } catch (_) {} }; }"
      (arg ctx <: RecNil)

  -- The only per-frame work: read the analyser, resize one bar.
  Timers.foreverFrame $ \_ -> do
    Audio.byteFrequencyData ana spectrum
    level <- Audio.meanByte spectrum
    setProp meterBar "style.width" (toString (level * number 100) <> string "%")
 where
  sliderIds = [idCutoff, idResonance, idAttack, idDecay, idSustain, idRelease]

-- | @el.classList.toggle("on", isChosen)@ — one call per button, no
-- branch, so the emitted JS stays flat.
markWave :: Text -> (Wave, Element f) -> Stmt f
markWave chosen (w, el) =
  toSyntax . callMethod el "classList.toggle" $
    arg (string "on") <: arg (bool (waveName w == chosen)) <: RecNil
