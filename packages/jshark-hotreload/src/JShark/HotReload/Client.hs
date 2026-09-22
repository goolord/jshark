{-# LANGUAGE TemplateHaskell #-}

-- | Browser hot-reload runtime, embedded from @assets/jshark-reload.js@ and
-- served at @/__jshark/client.js@. The runtime implements the
-- @__JSHARK_DISPOSE__@ \/ @__JSHARK_HOT_STATE__@ lifecycle protocol
-- directly in JS.
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

-- | The runtime as UTF-8 bytes.
clientRuntimeScript :: ByteString
clientRuntimeScript = TE.encodeUtf8 clientRuntimeText

-- | The runtime as 'Text'. The asset is decoded as UTF-8 explicitly:
-- 'readFile' would use the build machine's locale, which fails outright
-- under a non-UTF-8 locale. Editing the asset rebuilds this module.
clientRuntimeText :: Text
clientRuntimeText =
  T.pack
    $( do
         rel <- makeRelativeToProject "assets/jshark-reload.js"
         qAddDependentFile rel
         runIO (T.unpack . TE.decodeUtf8 <$> BS.readFile rel) >>= stringE
     )
