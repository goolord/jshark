{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language ExistentialQuantification #-}
{-# language GADTs #-}
{-# language BangPatterns #-}

{-# language TypeFamilies #-}
{-# language AllowAmbiguousTypes #-}
{-# language ScopedTypeVariables #-}
{-# language KindSignatures #-}
{-# language TypeApplications #-}

module JShark.Ajax where

import JShark.Types
import JShark
import JShark.Api
import Network.HTTP.Types
import Topaz.Rec ((<:))
import Topaz.Types
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

import GHC.TypeLits
import Data.Proxy
import Data.Kind

new :: EffectSyntax f (Expr f ('Object (XHR)))
new = fmap Var $ toSyntax $ ffi "new XMLHttpRequest" RecNil

open :: StdMethod -> BS.ByteString -> Bool -> (Expr f ('Object (XHR))) -> EffectSyntax f ()
open method url async x = toSyntax_ (objectFfi x $ ffi "open" (string (T.decodeUtf8 (renderStdMethod method)) <: string (T.decodeUtf8 url) <: bool async <: RecNil))

sendGet :: Expr f ('Object (XHR)) -> EffectSyntax f ()
sendGet x = get @"send" x >>= call_

type family Field (r :: Type) (k :: Symbol) :: Universe

data XHR

type instance Field XHR "send" = 'Effectful 'Unit
type instance Field XHR "responseText" = 'String

get :: forall k r f. KnownSymbol k => Expr f ('Object r) -> EffectSyntax f (Expr f (Field r k))
get x = fmap Var $ toSyntax $ unsafeObject x (symbolVal (Proxy :: Proxy k))

call :: Expr f ('Effectful u) -> EffectSyntax f (Expr f u)
call e = do
  x <- toSyntax (expr e)
  y <- toSyntax (unEffectful (Var x))
  pure $ Var y

call_ :: Expr f ('Effectful 'Unit) -> EffectSyntax f ()
call_ e = do
  x <- toSyntax (expr e)
  _ <- toSyntax $ unEffectful (Var x)
  pure ()

sendPost :: Expr f 'String -> Effect f 'Unit
sendPost x = ffi "xhr.send" (x <: RecNil)

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
  sendGet xhrObj
  foo <- get @"responseText" xhrObj
  consoleLog foo
  toSyntax noOp

ex2 :: IO ()
ex2 = printComputation $ effectfulAST (fromSyntax ex)

readyStateDone :: Effect f 'Number
readyStateDone = expr 4

statusOK :: Effect f 'Number
statusOK = expr 200
