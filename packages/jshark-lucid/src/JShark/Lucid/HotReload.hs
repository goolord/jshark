{-# LANGUAGE OverloadedStrings #-}

-- | Lucid helper that emits the hot-reload client script tag.
module JShark.Lucid.HotReload
  ( hotReloadClient
  , hotReloadClientDisabled
  )
where

import Lucid

-- | Emits the @<script>@ tag for the hot-reload client runtime.
-- No defer: it must run before body @app.js@ so rAF patches apply.
hotReloadClient :: Html ()
hotReloadClient =
  script_ [src_ "/__jshark/client.js"] ("" :: Html ())

-- | @mempty@ — use in production shells that must not reference the
-- dev-server client.
hotReloadClientDisabled :: Html ()
hotReloadClientDisabled = mempty
