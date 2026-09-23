{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Web Audio bound with 'ffi'.
--
-- The Good Parts subset has no @new@, no dynamic property access, and no
-- knowledge of browser objects. All three live behind 'ffi' /
-- 'JShark.Api.callMethod' / 'unsafeObjectGet', and this module is the only
-- place in the example that touches them: everything below hands back an
-- ordinary typed JShark term, so 'Client' never writes a string of
-- JavaScript.
--
-- The phantom types keep the graph honest — 'connect' takes nodes, the
-- automation functions take params — even though JavaScript would let you
-- aim anything at anything.
--
-- Node handles are 'Expr' because a voice stores them in an object;
-- params are 'Effect' because they are read straight back out of a node
-- (@osc.frequency@) and used on the spot. Arguments are accepted through
-- 'ToEffect', so either form works at a call site.
module JShark.Example.Synth.Audio (module JShark.Example.Synth.Audio) where

import Data.Text (Text)
import JShark.Api
import JShark.Api.Types
import JShark.Object (unsafeObjectAssign, unsafeObjectGet)

data AudioCtx

-- | Any @AudioNode@: oscillator, gain, filter, analyser, destination.
data Node

-- | An @AudioParam@ — a value the audio thread automates.
data Param

-- | One held note: the oscillator, its own amplifier, and its envelope.
data Voice

type instance Field Voice "osc" = 'MutableObject Node

type instance Field Voice "vca" = 'MutableObject Node

type instance Field Voice "t0" = 'Number

type instance Field Voice "atk" = 'Number

type instance Field Voice "dec" = 'Number

type instance Field Voice "sus" = 'Number

-- | Anything that lifts to an @AudioContext@ handle.
type IsCtx f a = ToEffect f ('MutableObject AudioCtx) a

-- | Anything that lifts to an @AudioNode@ handle.
type IsNode f a = ToEffect f ('MutableObject Node) a

type NodeE f = Expr f ('MutableObject Node)

type ParamE f = Effect f ('MutableObject Param)

type NumE f = Expr f 'Number

-- Both lifts name their target universe: 'toEffect' alone would leave it
-- ambiguous at a call site like 'callMethod', whose receiver is any object.
node :: IsNode f a => a -> Effect f ('MutableObject Node)
node = toEffect

ctxOf :: IsCtx f a => a -> Effect f ('MutableObject AudioCtx)
ctxOf = toEffect

-- | @new AudioContext()@. 'ffi' is free text, so a constructor is reachable
-- even though the object language has no @new@.
newAudioContext :: EffectSyntax f (Expr f ('MutableObject AudioCtx))
newAudioContext = fmap var (toSyntax (ffi "new AudioContext" RecNil))

-- | @ctx.resume()@. A context starts suspended until a user gesture, so
-- this belongs in an input handler.
resume :: IsCtx f a => a -> EffectSyntax f ()
resume ctx = toSyntax_ (callMethod (ctxOf ctx) "resume" RecNil)

-- | @ctx.state@: @"suspended"@ until a gesture resumes it, then @"running"@.
contextState :: IsCtx f a => a -> EffectSyntax f (Expr f 'String)
contextState ctx = getProp (ctxOf ctx) "state"

-- | @ctx.currentTime@ — the audio clock, in seconds. Read it fresh at
-- every use: it advances continuously, so a bound value goes stale.
currentTime :: IsCtx f a => a -> EffectSyntax f (NumE f)
currentTime ctx = getProp (ctxOf ctx) "currentTime"

-- | @ctx.destination@ — the speakers.
destination ::
  Expr f ('MutableObject AudioCtx) -> Effect f ('MutableObject Node)
destination ctx = unsafeObjectGet (ctxOf ctx) "destination"

-- | @ctx.createOscillator()@ and the other node constructors. Voices sum,
-- so the compressor ahead of the speakers keeps a full chord from clipping.
oscillator
  , gain
  , biquadFilter
  , analyser
  , compressor ::
    IsCtx f a => a -> EffectSyntax f (NodeE f)
oscillator = create "createOscillator"
gain = create "createGain"
biquadFilter = create "createBiquadFilter"
analyser = create "createAnalyser"
compressor = create "createDynamicsCompressor"

create :: IsCtx f a => Text -> a -> EffectSyntax f (NodeE f)
create method ctx = fmap var (toSyntax (callMethod (ctxOf ctx) method RecNil))

-- | @from.connect(to)@ — one edge of the audio graph.
connect :: (IsNode f a, IsNode f b) => a -> b -> EffectSyntax f ()
connect from to =
  toSyntax_ (callMethod (node from) "connect" (ArgEffect (node to) <: RecNil))

-- | @node.disconnect()@. A stopped oscillator is collected, but the gain
-- node it fed stays wired to the graph; without this each note leaks one.
disconnect :: IsNode f a => a -> EffectSyntax f ()
disconnect n = toSyntax_ (callMethod (node n) "disconnect" RecNil)

-- | @src.onended = () => …@ — runs once the source has finished, which
-- is when its part of the graph can be taken down.
onEnded :: IsNode f a => a -> EffectSyntax f (f 'Unit) -> EffectSyntax f ()
onEnded n body =
  toSyntax_ . unsafeObjectAssign (unsafeObjectGet (node n) "onended") $
    LambdaE (\_ -> stmts body)

-- | @node.type = t@ (@"sawtooth"@, @"lowpass"@, …).
setType :: IsNode f a => a -> Expr f 'String -> EffectSyntax f (f 'Unit)
setType n = setProp (node n) "type"

-- | @analyser.fftSize = n@. The bin count is half of this, and the bins
-- span the whole spectrum, so reading a 32-byte buffer from the default
-- 2048 would only cover the bottom 750Hz or so; see 'fftSizeFor'.
setFftSize :: IsNode f a => a -> NumE f -> EffectSyntax f (f 'Unit)
setFftSize n = setProp (node n) "fftSize"

-- | A named param of a node: @osc.frequency@, @vca.gain@, @filter.Q@.
param :: IsNode f a => a -> String -> ParamE f
param n = unsafeObjectGet (node n)

-- | @p.value = v@. Immediate and unscheduled — right for a control the
-- user is dragging, wrong for anything an envelope owns.
setValue :: ParamE f -> NumE f -> EffectSyntax f (f 'Unit)
setValue p = setProp p "value"

-- | @p.setValueAtTime(v, t)@
setValueAt :: ParamE f -> NumE f -> NumE f -> EffectSyntax f ()
setValueAt p v t =
  toSyntax_ (callMethod p "setValueAtTime" (arg v <: arg t <: RecNil))

-- | One-shot ADSR on a gain param. Attack is a linear rise to peak; decay
-- and the hold use @setTargetAtTime@ so Chromium cannot drop a second
-- exponential ramp. @amp.value@ starts at the floor so a new GainNode
-- does not sit at unity until @t0@.
scheduleAdsr ::
  ParamE f
  -> NumE f
  -> NumE f
  -> NumE f
  -> NumE f
  -> NumE f
  -> NumE f
  -> EffectSyntax f ()
scheduleAdsr amp t0 atk dec sus peak minAmp =
  toSyntax_ . ffi scheduleAdsrJs $
    ArgEffect amp
      <: arg t0
      <: arg atk
      <: arg dec
      <: arg sus
      <: arg peak
      <: arg minAmp
      <: RecNil

scheduleAdsrJs :: Text
scheduleAdsrJs =
  "(amp,t0,atk,dec,sus,peak,floor)=>{"
    <> "const a=Math.max(0.001,atk),d=Math.max(0.001,dec);"
    <> "const s=Math.max(floor,Math.min(peak,sus));"
    <> "try{amp.cancelScheduledValues(0)}catch(e){}"
    <> "amp.value=floor;"
    <> "amp.setValueAtTime(floor,t0);"
    <> "amp.linearRampToValueAtTime(peak,t0+a);"
    <> "amp.setTargetAtTime(s,t0+a,d/3)"
    <> "}"

-- | Freeze the computed level, exponential-ramp to a near-zero floor over
-- the full release, then linear-ramp the last bit to 0 (Web Audio cannot
-- exponential-ramp to zero). Stop only after silence so the tail is not
-- a hard cut. Isolated so a missing @cancelAndHoldAtTime@ cannot skip
-- @stop@ and leave a voice hanging at unity.
releaseVoice ::
  IsNode f osc =>
  ParamE f
  -> osc
  -> NumE f
  -> NumE f
  -> NumE f
  -> NumE f
  -> NumE f
  -> NumE f
  -> NumE f
  -> NumE f
  -> EffectSyntax f ()
releaseVoice amp osc now rel minAmp t0 atk dec sus peak =
  toSyntax_ . ffi releaseVoiceJs $
    ArgEffect amp
      <: ArgEffect (node osc)
      <: arg now
      <: arg rel
      <: arg minAmp
      <: arg t0
      <: arg atk
      <: arg dec
      <: arg sus
      <: arg peak
      <: RecNil

releaseVoiceJs :: Text
releaseVoiceJs =
  "(amp,osc,now,rel,floor,t0,atk,dec,sus,peak)=>{"
    <> "const r=Math.max(0.001,rel),a=Math.max(0.001,atk),d=Math.max(0.001,dec);"
    <> "const s=Math.max(floor,Math.min(peak,sus)),quiet=1e-5,tail=0.03;"
    <> "let v;"
    <> "if(now<=t0)v=floor;"
    <> "else if(now<t0+a){const u=(now-t0)/a;v=floor+(peak-floor)*u;}"
    <> "else{v=s+(peak-s)*Math.exp(-(now-(t0+a))/(d/3));}"
    <> "if(!(v>quiet))v=quiet;"
    <> "try{amp.cancelAndHoldAtTime?amp.cancelAndHoldAtTime(now)"
    <> ":amp.cancelScheduledValues(now)}catch(e){}"
    <> "try{"
    <> "amp.setValueAtTime(v,now);"
    <> "amp.exponentialRampToValueAtTime(quiet,now+r);"
    <> "amp.linearRampToValueAtTime(0,now+r+tail)"
    <> "}catch(e){amp.value=0}"
    <> "try{osc.stop(now+r+tail+0.02)}catch(e){try{osc.stop()}catch(e2){}}"
    <> "}"

-- | @src.start(t)@
startAt :: IsNode f a => a -> NumE f -> EffectSyntax f ()
startAt n t = toSyntax_ (callMethod (node n) "start" (arg t <: RecNil))

-- | A zeroed analysis buffer of @n@ bytes, bound so the analyser and the
-- meter share one array. 'newByteArray' asks for the size and nothing
-- else, which is what an output buffer wants: the analyser fills it.
analysisBuffer :: Int -> EffectSyntax f (Expr f 'Uint8Array)
analysisBuffer n = fmap var (toSyntax (newByteArray (number (fromIntegral n))))

-- | The @fftSize@ that yields @bins@ frequency bins: the analyser reports
-- half its transform size.
fftSizeFor :: Int -> NumE f
fftSizeFor bins = number (fromIntegral (bins * 2))

-- | @analyser.getByteFrequencyData(buf)@. Fills @buf@ in place.
byteFrequencyData :: IsNode f a => a -> Expr f 'Uint8Array -> EffectSyntax f ()
byteFrequencyData a buf =
  toSyntax_ (callMethod (node a) "getByteFrequencyData" (arg buf <: RecNil))

-- | Mean of the buffer, scaled to @0..1@. Byte arrays carry no fold in the
-- object language, so the reduction is one 'ffi' crossing per frame.
meanByte :: Expr f 'Uint8Array -> EffectSyntax f (NumE f)
meanByte buf =
  fmap var . toSyntax $
    ffi
      "((b) => b.reduce((a, x) => a + x, 0) / (b.length * 255))"
      (arg buf <: RecNil)

-- | @o[k]@ with a computed key, as an 'Option'.
--
-- 'JShark.Api.getProp' only takes a literal name, and the voice table is
-- keyed by whichever note is held, so this one needs 'ffi'. The result type
-- is the caller's assertion.
--
-- The @?? null@ matters: 'Option' is @null@ to JShark, and a missing
-- property is @undefined@, which would fail the @=== null@ test that
-- 'unsafeNullable' compiles to.
dictGet :: Effect f ('MutableObject r) -> Expr f 'String -> Effect f ('Option u)
dictGet o k =
  Bind
    Nothing
    (ffi "((o, k) => o[k] ?? null)" (ArgEffect o <: arg k <: RecNil))
    (\x -> Lift (unsafeNullable (Var x)))

-- | @o[k] = v@ with a computed key. See 'dictGet'.
dictSet ::
  Effect f ('MutableObject r) -> Expr f 'String -> Expr f u -> EffectSyntax f ()
dictSet o k v =
  toSyntax_
    (ffi "((o, k, v) => { o[k] = v; })" (ArgEffect o <: arg k <: arg v <: RecNil))

-- | Run the body for every key of @o@. @Object.keys@ snapshots, so the body
-- may delete as it goes. Used to drop every held note at once.
forEachKey ::
  Effect f ('MutableObject r)
  -> (Expr f 'String -> EffectSyntax f (f 'Unit))
  -> EffectSyntax f ()
forEachKey o body =
  toSyntax_ . ffi "((o, f) => { for (const k of Object.keys(o)) f(k); })" $
    ArgEffect o <: ArgEffect (LambdaE (\k -> stmts (body (var k)))) <: RecNil

-- | @el.addEventListener(ev, fn, { once: true })@. The browser drops the
-- listener after the first call, which is cheaper and simpler than a
-- "have I started yet" flag in the program.
listenOnce ::
  Text
  -> Effect f ('MutableObject o)
  -> EffectSyntax f (f 'Unit)
  -> EffectSyntax f ()
listenOnce ev el body =
  toSyntax_ (ffi js (ArgEffect el <: arg (string ev) <: handler <: RecNil))
 where
  js = "((el, ev, fn) => el.addEventListener(ev, fn, { once: true }))"
  handler = ArgEffect (LambdaE (\_ -> stmts body))
