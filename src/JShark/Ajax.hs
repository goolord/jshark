{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | @XMLHttpRequest@ and @fetch@ wrappers for browser I/O.
module JShark.Ajax
  ( XHR
  , new
  , open
  , send
  , sendPost
  , readyStateDone
  , statusOK
  , FetchResponse
  , fetch
  )
where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import Network.HTTP.Types

new :: EffectSyntax f (Effect f ('MutableObject (XHR)))
new = hold $ ffi "new XMLHttpRequest" RecNil

open ::
  StdMethod
  -> BS.ByteString
  -> Bool
  -> Effect f ('MutableObject (XHR))
  -> EffectSyntax f ()
open method url async x =
  toSyntax_
    ( callMethod
        x
        "open"
        ( arg (string (T.decodeUtf8 (renderStdMethod method)))
            <: arg (string (T.decodeUtf8 url))
            <: arg (bool async)
            <: RecNil
        )
    )

send :: Effect f ('MutableObject XHR) -> EffectSyntax f ()
send x = toSyntax_ $ callMethod x "send" RecNil

sendPost :: Effect f ('MutableObject XHR) -> Expr f 'String -> EffectSyntax f ()
sendPost x y = toSyntax_ $ callMethod x "send" (arg y <: RecNil)

data XHR

type instance Field XHR "responseText" = 'String

readyStateDone :: Effect f 'Number
readyStateDone = expr 4

statusOK :: Effect f 'Number
statusOK = expr 200

-- Fetch -------------------------------------------------------------------

-- | Opaque tag for the object returned by @fetch@. Simplified: hands back
-- the resolved response's handle directly instead of modeling a Promise
-- of Response, and has no body-streaming/JSON-decoding methods.
data FetchResponse

type instance Field FetchResponse "ok" = 'Bool

type instance Field FetchResponse "status" = 'Number

-- | @fetch(url)@
fetch ::
  Expr f 'String -> EffectSyntax f (Effect f ('MutableObject FetchResponse))
fetch url = hold $ ffi "fetch" (arg url <: RecNil)
