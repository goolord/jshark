{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language ExistentialQuantification #-}
{-# language GADTs #-}
{-# language BangPatterns #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language ScopedTypeVariables #-}

module JShark.Ajax where

import JShark
import JShark.Api
import JShark.Object
import JShark.Types
import Network.HTTP.Types
import Topaz.Rec ((<:))
import Topaz.Types
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

new :: EffectSyntax f (Effect f ('Object (XHR)))
new = fmap (expr . Var) $ toSyntax $ ffi "new XMLHttpRequest" RecNil

open :: StdMethod -> BS.ByteString -> Bool -> Effect f ('Object (XHR)) -> EffectSyntax f ()
open method url async x = toSyntax_ (objectFfi x $ ffi "open" (string (T.decodeUtf8 (renderStdMethod method)) <: string (T.decodeUtf8 url) <: bool async <: RecNil))

send :: Effect f ('Object XHR) -> EffectSyntax f ()
send x = getCall @"send" x *> pure ()

sendPost :: Effect f ('Object XHR) -> Expr f 'String -> EffectSyntax f ()
sendPost x y = toSyntax (objectFfi x (Lift y)) *> pure ()

data XHR

type instance Field XHR "send" = 'Effectful 'Unit
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

