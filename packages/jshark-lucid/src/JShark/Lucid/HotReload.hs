{-# LANGUAGE OverloadedStrings #-}

-- | Lucid helpers that emit the hot-reload client script tag.
module JShark.Lucid.HotReload
  ( HotReloadClientConfig (..)
  , defaultHotReloadClientConfig
  , hotReloadClient
  , hotReloadClientWith
  )
where

import Data.Text (Text)
import Lucid

data HotReloadClientConfig = HotReloadClientConfig
  { hrClientSrc :: Text
  , hrEnabled :: Bool
  }

defaultHotReloadClientConfig :: HotReloadClientConfig
defaultHotReloadClientConfig =
  HotReloadClientConfig
    { hrClientSrc = "/__jshark/client.js"
    , hrEnabled = True
    }

-- | Emits the @<script>@ tag for the hot-reload client runtime.
-- In production, pass a config with 'hrEnabled' = False (renders nothing).
hotReloadClient :: Html ()
hotReloadClient = hotReloadClientWith defaultHotReloadClientConfig

hotReloadClientWith :: HotReloadClientConfig -> Html ()
hotReloadClientWith cfg
  | not (hrEnabled cfg) = mempty
  | otherwise =
      -- No defer: must run before body @app.js@ so rAF patches apply.
      script_
        [src_ (hrClientSrc cfg)]
        ("" :: Html ())
