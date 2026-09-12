{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Framework-agnostic hot-reload hub: typed events, broadcast channel,
-- and in-memory JS artifact cache.
module JShark.HotReload.Core
  ( HotReloadEvent (..)
  , HotReloadConfig (..)
  , HotReloadHub
  , defaultHotReloadConfig
  , newHotReloadHub
  , hotReloadConfig
  , broadcastEvent
  , subscribe
  , encodeEvent
  , registerJs
  , lookupJs
  , registerHtml
  , lookupHtml
  , currentJsHashes
  , setBuildError
  , lastBuildError
  , setBuildStart
  , lastCompiling
  )
where

import Control.Concurrent.STM
  ( TChan
  , atomically
  , dupTChan
  , newBroadcastTChanIO
  , readTChan
  , writeTChan
  )
import Data.Bits (xor)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | Server → browser hot-reload notifications (SSE payloads).
-- Positional fields avoid -Wpartial-fields across sum constructors.
data HotReloadEvent
  = -- | appName, url, hash
    JsUpdate Text Text Text
  | -- | url, timestamp
    CssUpdate Text Int64
  | -- | reason
    PageReload Text
  | -- | message
    BuildError Text
  | -- | appName currently compiling (Haskell rebuild started)
    BuildStart Text
  | -- | Sent on SSE connect so Mode C (server restart) can detect bumps.
    Hello [(Text, Text)]
  deriving (Show, Eq)

-- | Runtime settings for event/client paths, auto-inject, and debounce.
data HotReloadConfig = HotReloadConfig
  { hrEnabled :: Bool
  , hrAutoInject :: Bool
  , hrEventsPath :: Text
  , hrClientPath :: Text
  , hrDebounceMs :: Int
  }
  deriving (Show, Eq)

-- | Default config: enabled, auto-inject, standard paths, 75 ms debounce.
defaultHotReloadConfig :: HotReloadConfig
defaultHotReloadConfig =
  HotReloadConfig
    { hrEnabled = True
    , hrAutoInject = True
    , hrEventsPath = "/__jshark/events"
    , hrClientPath = "/__jshark/client.js"
    , hrDebounceMs = 75
    }

-- | Shared broadcast channel, artifact caches, and build-status refs.
data HotReloadHub = HotReloadHub
  { hubConfig :: HotReloadConfig
  , hubChan :: TChan HotReloadEvent
  , hubJs :: IORef (Map.Map Text (Text, Text))
  , hubHtml :: IORef (Map.Map Text (Text, Text))
  , hubError :: IORef (Maybe Text)
  , hubCompiling :: IORef (Maybe Text)
  }

-- | The 'HotReloadConfig' the hub was created with.
hotReloadConfig :: HotReloadHub -> HotReloadConfig
hotReloadConfig = hubConfig

-- | Create a hub with an empty cache and no build status.
newHotReloadHub :: HotReloadConfig -> IO HotReloadHub
newHotReloadHub cfg = do
  chan <- newBroadcastTChanIO
  js <- newIORef Map.empty
  html <- newIORef Map.empty
  err <- newIORef Nothing
  compiling <- newIORef Nothing
  pure
    HotReloadHub
      { hubConfig = cfg
      , hubChan = chan
      , hubJs = js
      , hubHtml = html
      , hubError = err
      , hubCompiling = compiling
      }

-- | Broadcast an event to all subscribers and update build status.
broadcastEvent :: HotReloadHub -> HotReloadEvent -> IO ()
broadcastEvent hub ev = do
  case ev of
    BuildError msg -> do
      atomicModifyIORef' (hubError hub) (\_ -> (Just msg, ()))
      clearCompiling hub
    BuildStart app -> do
      clearBuildError hub
      atomicModifyIORef' (hubCompiling hub) (\_ -> (Just app, ()))
    JsUpdate {} -> do
      clearBuildError hub
      clearCompiling hub
    PageReload {} -> clearCompiling hub
    _ -> pure ()
  atomically $ writeTChan (hubChan hub) ev

-- | Duplicate the broadcast channel for one SSE client.
subscribe :: HotReloadHub -> IO (IO HotReloadEvent)
subscribe hub = do
  ch <- atomically $ dupTChan (hubChan hub)
  pure (atomically (readTChan ch))

-- | SSE @data:@ JSON line (no trailing blank line).
encodeEvent :: HotReloadEvent -> Text
encodeEvent = \case
  JsUpdate name u h ->
    object
      [ ("type", str "js-update")
      , ("appName", str name)
      , ("url", str u)
      , ("hash", str h)
      ]
  CssUpdate u ts ->
    object
      [ ("type", str "css-update")
      , ("url", str u)
      , ("timestamp", T.pack (show ts))
      ]
  PageReload why ->
    object
      [ ("type", str "page-reload")
      , ("reason", str why)
      ]
  BuildError msg ->
    object
      [ ("type", str "build-error")
      , ("message", str msg)
      ]
  BuildStart app ->
    object
      [ ("type", str "build-start")
      , ("appName", str app)
      ]
  Hello hashes ->
    object
      [ ("type", str "hello")
      , ("jsHashes", hashObject hashes)
      ]
 where
  str t = "\"" <> escapeJson t <> "\""
  object pairs =
    "{"
      <> T.intercalate "," [str k <> ":" <> v | (k, v) <- pairs]
      <> "}"
  hashObject hs =
    "{"
      <> T.intercalate
        ","
        [str k <> ":" <> str v | (k, v) <- hs]
      <> "}"

escapeJson :: Text -> Text
escapeJson =
  T.concatMap $ \c -> case c of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ -> T.singleton c

-- | Cache compiled JS and return its content hash.
registerJs :: HotReloadHub -> Text -> Text -> IO Text
registerJs hub name source = do
  let
    h = jsHash source
  atomicModifyIORef' (hubJs hub) $ \m ->
    (Map.insert name (source, h) m, ())
  pure h

-- | Look up cached JS source and hash by app name.
lookupJs :: HotReloadHub -> Text -> IO (Maybe (Text, Text))
lookupJs hub name = Map.lookup name <$> readIORef (hubJs hub)

-- | Cache rendered Lucid HTML and return its content hash.
registerHtml :: HotReloadHub -> Text -> Text -> IO Text
registerHtml hub name source = do
  let
    h = jsHash source
  atomicModifyIORef' (hubHtml hub) $ \m ->
    (Map.insert name (source, h) m, ())
  pure h

-- | Look up cached HTML source and hash by app name.
lookupHtml :: HotReloadHub -> Text -> IO (Maybe (Text, Text))
lookupHtml hub name = Map.lookup name <$> readIORef (hubHtml hub)

jsHash :: Text -> Text
jsHash t =
  -- Short fingerprint: length + FNV-1a 32-bit hex (no extra deps).
  T.pack (show (T.length t)) <> "-" <> T.pack (pad8 (showHex fnv))
 where
  fnv = T.foldl' step (2166136261 :: Int) t
  step h c =
    let
      h' = h `xor` fromEnum c
     in
      h' * 16777619
  showHex n =
    let
      digits = "0123456789abcdef"
      go 0 acc = acc
      go x acc = go (x `div` 16) (digits !! (x `mod` 16) : acc)
     in
      if n == 0 then "0" else go (abs n) ""
  pad8 s = replicate (max 0 (8 - length s)) '0' <> take 8 s

-- | Snapshot the app-name to JS hash map for a new SSE client.
currentJsHashes :: HotReloadHub -> IO [(Text, Text)]
currentJsHashes hub = do
  m <- readIORef (hubJs hub)
  pure [(k, h) | (k, (_, h)) <- Map.toList m]

-- | Record and broadcast a build error.
setBuildError :: HotReloadHub -> Text -> IO ()
setBuildError hub msg = broadcastEvent hub (BuildError msg)

clearBuildError :: HotReloadHub -> IO ()
clearBuildError hub = atomicModifyIORef' (hubError hub) (\_ -> (Nothing, ()))

-- | The most recent build error, if any.
lastBuildError :: HotReloadHub -> IO (Maybe Text)
lastBuildError = readIORef . hubError

-- | Record and broadcast that an app started compiling.
setBuildStart :: HotReloadHub -> Text -> IO ()
setBuildStart hub app = broadcastEvent hub (BuildStart app)

clearCompiling :: HotReloadHub -> IO ()
clearCompiling hub = atomicModifyIORef' (hubCompiling hub) (\_ -> (Nothing, ()))

-- | The app currently compiling, if any.
lastCompiling :: HotReloadHub -> IO (Maybe Text)
lastCompiling = readIORef . hubCompiling
