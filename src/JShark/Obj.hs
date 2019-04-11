{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language ExistentialQuantification #-}
{-# language GADTs #-}
{-# language BangPatterns #-}

{-# language TypeFamilies #-}
{-# language ScopedTypeVariables #-}
{-# language KindSignatures #-}

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

new :: EffectSyntax f (f ('Object (XHR f)))
new = toSyntax $ ffi "new XMLHttpRequest" RecNil

open :: StdMethod -> BS.ByteString -> Bool -> (f ('Object (XHR f))) -> EffectSyntax f ()
open method url async x = toSyntax_ (objectFfi (Var x) $ ffi "open" (string (T.decodeUtf8 (renderStdMethod method)) <: string (T.decodeUtf8 url) <: bool async <: RecNil))

sendGet' :: Effect f 'Unit
sendGet' = ffi "xhr.send" RecNil

sendGet :: f ('Object (XHR f)) -> EffectSyntax f (XHR f)
sendGet x = toSyntax sendGet' *> pure XHR 
  { responseText = unsafeObject (Var x) "responseText" }

type family Field (r :: Type) (k :: Symbol) :: Universe

get :: forall k r f. KnownSymbol k => f ('Object r) -> EffectSyntax f (f (Field r k))
get x = unsafeObject (Var x) (symbolVal (Proxy :: Proxy k))

data XHR f = XHR { responseText :: Effect f 'String }

sendPost :: Expr f 'String -> Effect f 'Unit
sendPost x = ffi "xhr.send" (x <: RecNil)

ex :: EffectSyntax f (f 'Unit)
ex = do 
  xhrObj <- new
  open GET "foo.com" True xhrObj
  xhr <- sendGet xhrObj
  foo <- toSyntax (responseText xhr)
  toSyntax $ consoleLog (Var foo)

ex2 :: IO ()
ex2 = printComputation $ effectfulAST (fromSyntax ex)

readyStateDone :: Effect f 'Number
readyStateDone = expr 4

statusOK :: Effect f 'Number
statusOK = expr 200
