{-# language DataKinds #-}
{-# language GADTs #-}

module Ajax where

import Types
import Reboot
import Network.HTTP.Types
import Topaz.Rec ((<:))
import Topaz.Types
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

open :: StdMethod -> BS.ByteString -> Bool -> Effect f 'Unit
open method url async = ffi "xhttp.open" (string (T.decodeUtf8 (renderStdMethod method)) <: string (T.decodeUtf8 url) <: bool async <: RecNil)

sendGet :: Effect f 'Unit
sendGet = ffi "xhttp.send" RecNil

sendPost :: Expr f 'String -> Effect f 'Unit
sendPost x = ffi "xhttp.send" (x <: RecNil)
