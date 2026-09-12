{-# LANGUAGE OverloadedStrings #-}

-- | Lucid helper that emits the hot-reload client script tag.
module JShark.Lucid.HotReload
  ( hotReloadClient
  )
where

import Lucid

-- | Emits the @<script>@ tag for the hot-reload client runtime.
-- No defer: it must run before body @app.js@ so rAF patches apply.
hotReloadClient :: Html ()
hotReloadClient =
  script_ [src_ "/__jshark/client.js"] ("" :: Html ())
