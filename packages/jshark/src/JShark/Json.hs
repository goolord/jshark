{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Minimal @JSON@ wrapper.
module JShark.Json
  ( stringify
  , stringifyPure
  , unsafeParse
  , tryParse
  )
where

import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types

-- | @JSON.stringify(x)@. Effectful: it throws on @BigInt@ and cyclic
-- values and yields 'none' when the value has no JSON form (functions,
-- @undefined@). The native @undefined@ is normalized to @null@.
stringify :: Expr f u -> Effect f ('Option 'String)
stringify x =
  ffi
    "((x) => { const s = JSON.stringify(x); return s === undefined ? {some: false} : {some: true, value: s}; })"
    (arg x <: RecNil)

-- | @JSON.stringify(x)@ for values the caller knows have a JSON form and
-- cannot throw. Pure 'Std' 'Un', so it participates in the pure kernel;
-- the optimizer still treats it as effectful (it may throw) and never
-- discards it.
stringifyPure :: Expr f u -> Expr f 'String
stringifyPure = expr1 FixStringify

-- | @JSON.parse(x)@. Throws on bad JSON, so this is an 'Effect'. The
-- result type is asserted by the caller and not checked.
unsafeParse :: Expr f 'String -> Effect f u
unsafeParse x = ffi "JSON.parse" (arg x <: RecNil)

-- | @JSON.parse@ that yields 'none' on throw.
tryParse :: Expr f 'String -> Effect f ('Option u)
tryParse s =
  catch_
    (Bind Nothing (unsafeParse s) (\x -> Lift (some (Var x))))
    (\_ -> expr none)
