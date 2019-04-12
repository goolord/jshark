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

new :: EffectSyntax f (Expr f ('Object (XHR)))
new = fmap Var $ toSyntax $ ffi "new XMLHttpRequest" RecNil

open :: StdMethod -> BS.ByteString -> Bool -> (Expr f ('Object (XHR))) -> EffectSyntax f ()
open method url async x = toSyntax_ (objectFfi x $ ffi "open" (string (T.decodeUtf8 (renderStdMethod method)) <: string (T.decodeUtf8 url) <: bool async <: RecNil))

send :: Expr f ('Object XHR) -> EffectSyntax f ()
send x = get @"send" x >>= call_

data XHR

type instance Field XHR "send" = 'Effectful 'Unit
type instance Field XHR "responseText" = 'String

-- ex :: EffectSyntax f (f 'Unit)
-- ex = do 
  -- xhrObj <- new
  -- open GET "foo.com" True xhrObj
  -- xhr <- sendGet xhrObj
  -- foo <- toSyntax (responseText xhr)
  -- toSyntax $ consoleLog (Var foo)

ex :: EffectSyntax f (f 'Unit)
ex = do 
  xhrObj <- new
  open GET "foo.com" True xhrObj
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
