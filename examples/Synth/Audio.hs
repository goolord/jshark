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
module Audio
  ( -- * Handles
    AudioCtx
  , Node
  , Param
  , Voice
  , IsCtx
  , IsNode

    -- * Context
  , newAudioContext
  , resume
  , contextState
  , currentTime
  , destination

    -- * Nodes
  , oscillator
  , gain
  , biquadFilter
  , compressor
  , analyser
  , connect
  , disconnect
  , onEnded
  , setType
  , setFftSize

    -- * Params
  , param
  , paramValue
  , setValue
  , setValueAt
  , rampTo
  , cancelFrom

    -- * Sources
  , startAt
  , stopAt

    -- * Analysis
  , analysisBuffer
  , fftSizeFor
  , byteFrequencyData
  , meanByte

    -- * Escape hatches
  , dictGet
  , dictSet
  , forEachKey
  , listenOnce
  )
where

import Data.Text (Text)
import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types
import JShark.Object (unsafeObjectAssign, unsafeObjectGet)

-- | An @AudioContext@.
data AudioCtx

-- | Any @AudioNode@: oscillator, gain, filter, analyser, destination.
data Node

-- | An @AudioParam@ — a value the audio thread automates.
data Param

-- | One held note: the oscillator and its own amplifier.
data Voice

type instance Field Voice "osc" = 'MutableObject Node

type instance Field Voice "vca" = 'MutableObject Node

-- | Anything that lifts to an @AudioContext@ handle.
type IsCtx f a = ToEffect f ('MutableObject AudioCtx) a

-- | Anything that lifts to an @AudioNode@ handle.
type IsNode f a = ToEffect f ('MutableObject Node) a

-- Both lifts name their target universe: 'toEffect' alone would leave it
-- ambiguous at a call site like 'callMethod', whose receiver is any object.
node :: IsNode f a => a -> Effect f ('MutableObject Node)
node = toEffect

ctxOf :: IsCtx f a => a -> Effect f ('MutableObject AudioCtx)
ctxOf = toEffect

-- | @new AudioContext()@.
--
-- 'ffi' is free text, so a constructor is reachable even though the object
-- language has no @new@.
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
currentTime :: IsCtx f a => a -> EffectSyntax f (Expr f 'Number)
currentTime ctx = getProp (ctxOf ctx) "currentTime"

-- | @ctx.destination@ — the speakers.
destination :: IsCtx f a => a -> Effect f ('MutableObject Node)
destination ctx = unsafeObjectGet (ctxOf ctx) "destination"

-- | @ctx.createOscillator()@
oscillator :: IsCtx f a => a -> EffectSyntax f (Expr f ('MutableObject Node))
oscillator = create "createOscillator"

-- | @ctx.createGain()@
gain :: IsCtx f a => a -> EffectSyntax f (Expr f ('MutableObject Node))
gain = create "createGain"

-- | @ctx.createBiquadFilter()@
biquadFilter :: IsCtx f a => a -> EffectSyntax f (Expr f ('MutableObject Node))
biquadFilter = create "createBiquadFilter"

-- | @ctx.createAnalyser()@
analyser :: IsCtx f a => a -> EffectSyntax f (Expr f ('MutableObject Node))
analyser = create "createAnalyser"

-- | @ctx.createDynamicsCompressor()@.
--
-- Voices sum, so a handful of held notes would otherwise exceed unity and
-- clip. One of these ahead of the speakers keeps a full chord in range.
compressor :: IsCtx f a => a -> EffectSyntax f (Expr f ('MutableObject Node))
compressor = create "createDynamicsCompressor"

create ::
  IsCtx f a => String -> a -> EffectSyntax f (Expr f ('MutableObject Node))
create method ctx =
  fmap var (toSyntax (callMethod (ctxOf ctx) method RecNil))

-- | @from.connect(to)@ — one edge of the audio graph.
connect :: (IsNode f a, IsNode f b) => a -> b -> EffectSyntax f ()
connect from to =
  toSyntax_ (callMethod (node from) "connect" (ArgEffect (node to) <: RecNil))

-- | @node.disconnect()@.
--
-- A stopped oscillator is collected, but the gain node it fed stays wired
-- to the graph. Without this each note played leaks one node.
disconnect :: IsNode f a => a -> EffectSyntax f ()
disconnect n = toSyntax_ (callMethod (node n) "disconnect" RecNil)

-- | @src.onended = () => …@ — runs once the source has finished, which
-- is when its part of the graph can be taken down.
onEnded :: IsNode f a => a -> EffectSyntax f (f 'Unit) -> EffectSyntax f ()
onEnded n body =
  toSyntax_
    ( unsafeObjectAssign
        (unsafeObjectGet (node n) "onended")
        (LambdaE (\_ -> stmts body))
    )

-- | @node.type = t@ (@"sawtooth"@, @"lowpass"@, …).
setType :: IsNode f a => a -> Expr f 'String -> EffectSyntax f (f 'Unit)
setType n t = setProp (node n) "type" t

-- | @analyser.fftSize = n@.
--
-- The bin count is half of this, and the bins span the whole spectrum, so
-- reading a 32-byte buffer from the default 2048 would only cover the
-- bottom 750Hz or so. Size the transform to the buffer instead.
setFftSize :: IsNode f a => a -> Expr f 'Number -> EffectSyntax f (f 'Unit)
setFftSize n v = setProp (node n) "fftSize" v

-- | A named param of a node: @osc.frequency@, @vca.gain@, @filter.Q@.
param :: IsNode f a => a -> String -> Effect f ('MutableObject Param)
param n = unsafeObjectGet (node n)

-- | @p.value@, the param's value right now.
paramValue :: Effect f ('MutableObject Param) -> EffectSyntax f (Expr f 'Number)
paramValue p = getProp p "value"

-- | @p.value = v@. Immediate and unscheduled — right for a control the
-- user is dragging, wrong for anything an envelope owns.
setValue ::
  Effect f ('MutableObject Param) -> Expr f 'Number -> EffectSyntax f (f 'Unit)
setValue p v = setProp p "value" v

-- | @p.setValueAtTime(v, t)@
setValueAt ::
  Effect f ('MutableObject Param)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f ()
setValueAt p v t =
  toSyntax_ (callMethod p "setValueAtTime" (arg v <: arg t <: RecNil))

-- | @p.linearRampToValueAtTime(v, t)@.
--
-- Where the timing guarantee comes from: the ramp runs on the audio thread,
-- so a busy main thread cannot make it stutter.
rampTo ::
  Effect f ('MutableObject Param)
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f ()
rampTo p v t =
  toSyntax_ (callMethod p "linearRampToValueAtTime" (arg v <: arg t <: RecNil))

-- | @p.cancelScheduledValues(t)@ — drop automation queued after @t@.
cancelFrom ::
  Effect f ('MutableObject Param) -> Expr f 'Number -> EffectSyntax f ()
cancelFrom p t =
  toSyntax_ (callMethod p "cancelScheduledValues" (arg t <: RecNil))

-- | @src.start(t)@
startAt :: IsNode f a => a -> Expr f 'Number -> EffectSyntax f ()
startAt n t = toSyntax_ (callMethod (node n) "start" (arg t <: RecNil))

-- | @src.stop(t)@
stopAt :: IsNode f a => a -> Expr f 'Number -> EffectSyntax f ()
stopAt n t = toSyntax_ (callMethod (node n) "stop" (arg t <: RecNil))

-- | A zeroed analysis buffer of @n@ bytes.
--
-- 'JShark.Api.newByteArray' asks for the size and nothing else, which is
-- what an output buffer wants: the analyser supplies the contents.
-- 'JShark.Api.uint8Array' is the host 'ByteArray' literal.
--
-- Bound, so the analyser and the meter share one array. JS can write the
-- object.
analysisBuffer ::
  Int -> EffectSyntax f (Expr f 'Uint8Array)
analysisBuffer n =
  fmap var (toSyntax (newByteArray (number (fromIntegral n))))

-- | The @fftSize@ that yields @bins@ frequency bins.
--
-- The analyser reports half its transform size, and a buffer shorter than
-- that only ever sees the low end of the spectrum.
fftSizeFor :: Int -> Expr f 'Number
fftSizeFor bins = number (fromIntegral (bins * 2))

-- | @analyser.getByteFrequencyData(buf)@. Fills @buf@ in place.
byteFrequencyData ::
  IsNode f a =>
  a
  -> Expr f 'Uint8Array
  -> EffectSyntax f ()
byteFrequencyData a buf =
  toSyntax_ (callMethod (node a) "getByteFrequencyData" (arg buf <: RecNil))

-- | Mean of the buffer, scaled to @0..1@.
--
-- Byte arrays carry no fold in the object language, so the reduction is an
-- 'ffi'. Doing it in one call also keeps the per-frame work to a single
-- crossing. Reads the handle directly.
meanByte ::
  Expr f 'Uint8Array -> EffectSyntax f (Expr f 'Number)
meanByte buf =
  fmap
    var
    ( toSyntax
        ( ffi
            "((b) => b.reduce((a, x) => a + x, 0) / (b.length * 255))"
            (arg buf <: RecNil)
        )
    )

-- | @o[k]@ with a computed key, as an 'Option'.
--
-- 'JShark.Api.getProp' only takes a literal name, and the voice table is
-- keyed by whichever note is held, so this one needs 'ffi'. The result type
-- is the caller's assertion.
--
-- The @?? null@ matters: 'Option' is @null@ to JShark, and a missing
-- property is @undefined@, which would fail the @=== null@ test that
-- 'unsafeNullable' compiles to.
dictGet ::
  Effect f ('MutableObject r) -> Expr f 'String -> Effect f ('Option u)
dictGet o k =
  Bind
    (ffi "((o, k) => o[k] ?? null)" (ArgEffect o <: arg k <: RecNil))
    (\x -> Lift (unsafeNullable (Var x)))

-- | @o[k] = v@ with a computed key. See 'dictGet'.
dictSet ::
  Effect f ('MutableObject r)
  -> Expr f 'String
  -> Expr f u
  -> EffectSyntax f ()
dictSet o k v =
  toSyntax_
    (ffi "((o, k, v) => { o[k] = v; })" (ArgEffect o <: arg k <: arg v <: RecNil))

-- | Run the body for every key of @o@.
--
-- @Object.keys@ snapshots, so the body may delete as it goes. Used to drop
-- every held note at once.
forEachKey ::
  Effect f ('MutableObject r)
  -> (Expr f 'String -> EffectSyntax f (f 'Unit))
  -> EffectSyntax f ()
forEachKey o body =
  toSyntax_
    ( ffi
        "((o, f) => { for (const k of Object.keys(o)) f(k); })"
        (ArgEffect o <: ArgEffect (LambdaE (\k -> stmts (body (var k)))) <: RecNil)
    )

-- | @el.addEventListener(ev, fn, { once: true })@.
--
-- The browser drops the listener after the first call, which is cheaper and
-- simpler than a "have I started yet" flag in the program.
listenOnce ::
  Text
  -> Effect f ('MutableObject o)
  -> EffectSyntax f (f 'Unit)
  -> EffectSyntax f ()
listenOnce ev el body =
  toSyntax_
    ( ffi
        "((el, ev, fn) => el.addEventListener(ev, fn, { once: true }))"
        ( ArgEffect el
            <: arg (string ev)
            <: ArgEffect (LambdaE (\_ -> stmts body))
            <: RecNil
        )
    )
