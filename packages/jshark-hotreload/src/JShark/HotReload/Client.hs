{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Browser hot-reload runtime bytes, embedded from
-- @assets/jshark-reload.js@ and served at @/__jshark/client.js@. The
-- runtime implements the @__JSHARK_DISPOSE__@ \/ @__JSHARK_HOT_STATE__@
-- lifecycle protocol directly in JS.
module JShark.HotReload.Client
  ( clientRuntimeScript
  , clientRuntimeText
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Language.Haskell.TH (stringE)
import Language.Haskell.TH.Syntax
  ( makeRelativeToProject
  , qAddDependentFile
  , runIO
  )

-- | Embedded @assets/jshark-reload.js@ served at
-- @/__jshark/client.js@.
clientRuntimeScript :: ByteString
clientRuntimeScript = TE.encodeUtf8 clientRuntimeText

-- | The embedded runtime as UTF-8 'Text'.
clientRuntimeText :: Text
clientRuntimeText = T.pack clientRuntimeSource

-- Rebuild this module when the browser runtime changes. The asset is read
-- as bytes and decoded as UTF-8 explicitly: 'readFile' would decode it with
-- whatever locale the build machine happens to have, which fails outright
-- under a non-UTF-8 locale.
clientRuntimeSource :: String
clientRuntimeSource =
  $( do
       rel <- makeRelativeToProject "assets/jshark-reload.js"
       qAddDependentFile rel
       runIO (T.unpack . TE.decodeUtf8 <$> BS.readFile rel) >>= stringE
   )
