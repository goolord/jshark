{-# LANGUAGE
    DataKinds
  , OverloadedStrings
  , ExistentialQuantification
  , GADTs
  , BangPatterns
  , TypeApplications
  , TypeFamilies
  , ScopedTypeVariables
#-}
module JShark.Ajax where

import JShark
import JShark.Api
import JShark.Object
import JShark.Types
import Network.HTTP.Types
import JShark.Rec (Rec(..), (<:))
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

new :: EffectSyntax f (Effect f ('Object (XHR)))
new = hold $ ffi "new XMLHttpRequest" RecNil

open :: StdMethod -> BS.ByteString -> Bool -> Effect f ('Object (XHR)) -> EffectSyntax f ()
open method url async x = toSyntax_ (callMethod x "open" (arg (string (T.decodeUtf8 (renderStdMethod method))) <: arg (string (T.decodeUtf8 url)) <: arg (bool async) <: RecNil))

send :: Effect f ('Object XHR) -> EffectSyntax f ()
send x = toSyntax_ $ callMethod x "send" RecNil

sendPost :: Effect f ('Object XHR) -> Expr f 'String -> EffectSyntax f ()
sendPost x y = toSyntax_ $ callMethod x "send" (arg y <: RecNil)

data XHR

type instance Field XHR "responseText" = 'String

ex :: EffectSyntax f (f 'Unit)
ex = do 
  xhrObj <- new
  open GET "https://postman-echo.com/get?foo1=bar1&foo2=bar2" True xhrObj
  send xhrObj
  foo <- get @"responseText" xhrObj
  consoleLog foo
  toSyntax noOp

ex2 :: IO ()
ex2 = printComputation $ effectfulAST (fromSyntax ex)

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
fetch :: Expr f 'String -> EffectSyntax f (Effect f ('Object FetchResponse))
fetch url = hold $ ffi "fetch" (arg url <: RecNil)

