{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Browser-side Life frame profiler. Samples land on
--   @window.__jsharkLifeProfile@ for @scripts/profile-life.sh@.
module Profile (install, sampleFrame) where

import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Dom (DomElement)

-- | Copy the GL renderer string and open the ring buffer.
install ::
  Effect f ('MutableObject DomElement)
  -> Effect f ('MutableObject ())
  -> EffectSyntax f (f 'Unit)
install canvas viewport = do
  toSyntax_
    $ discard
    $ ffi
      ( "(view, viewport) => {"
          <> " viewport._lifeView = view;"
          <> " viewport.glRenderer = view.__lifeGlRenderer || '';"
          <> " const p = {"
          <> "   glRenderer: viewport.glRenderer,"
          <> "   glLost: 0,"
          <> "   frames: [],"
          <> "   at: 0,"
          <> "   last: null,"
          <> "   started: performance.now()"
          <> " };"
          <> " viewport._lifeProfile = p;"
          <> " window.__jsharkLifeProfile = p;"
          <> " }"
      )
      (ArgEffect canvas <: ArgEffect viewport <: RecNil)
  done

-- | One rAF sample. Caps at 1800 frames (~30s at 60 Hz).
sampleFrame ::
  Effect f ('MutableObject ())
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> EffectSyntax f (f 'Unit)
sampleFrame viewport now fps gen pop stepMs renderMs otherMs glLost = do
  toSyntax_
    $ discard
    $ ffi
      ( "(viewport, now, fps, gen, pop, stepMs, renderMs, otherMs, glLost) => {"
          <> " const p = viewport._lifeProfile;"
          <> " if (!p) return;"
          <> " const view = viewport._lifeView;"
          <> " const renderer = (view && view.__lifeGlRenderer)"
          <> "   || viewport.glRenderer || p.glRenderer || '';"
          <> " viewport.glRenderer = renderer;"
          <> " p.glRenderer = renderer;"
          <> " p.glLost = glLost|0;"
          <> " const sample = {"
          <> "   t: now, fps: fps, gen: gen, pop: pop,"
          <> "   stepMs: stepMs, renderMs: renderMs, otherMs: otherMs,"
          <> "   glLost: glLost|0"
          <> " };"
          <> " p.last = sample;"
          <> " const cap = 1800;"
          <> " if (p.frames.length < cap) p.frames.push(sample);"
          <> " else {"
          <> "   p.frames[p.at|0] = sample;"
          <> "   p.at = ((p.at|0) + 1) % cap;"
          <> " }"
          <> " }"
      )
      ( ArgEffect viewport
          <: arg now
          <: arg fps
          <: arg gen
          <: arg pop
          <: arg stepMs
          <: arg renderMs
          <: arg otherMs
          <: arg glLost
          <: RecNil
      )
  done
