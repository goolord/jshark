{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Framework-agnostic hot-reload hub: typed events, broadcast channel,
-- and in-memory JS artifact cache.
module JShark.HotReload.Core
  ( HotReloadEvent (..)
  , HotReloadConfig (..)
  , HotReloadHub
  , HotReloadSnapshot (..)
  , defaultHotReloadConfig
  , newHotReloadHub
  , hotReloadConfig
  , broadcastEvent
  , subscribe
  , subscribeWithSnapshot
  , encodeEvent
  , registerJs
  , lookupJs
  , registerHtml
  , lookupHtml
  , currentJsHashes
  , currentRevision
  , setBuildError
  , lastBuildError
  , setBuildStart
  , lastCompiling
  )
where

import Control.Concurrent.STM
  ( TChan
  , TVar
  , atomically
  , dupTChan
  , modifyTVar'
  , newBroadcastTChanIO
  , newTVarIO
  , readTChan
  , readTVar
  , readTVarIO
  , writeTChan
  , writeTVar
  )
import Data.Aeson (ToJSON (..), encode, object, (.=))
import qualified Data.Aeson.Key as Key
import Data.Bits (xor)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

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
--
-- The SSE-facing state ('hubJs', 'hubError', 'hubCompiling') lives in 'TVar's
-- so a client's snapshot and its subscription can be taken in one atomic
-- transaction (see 'subscribeWithSnapshot').
data HotReloadHub = HotReloadHub
  { hubConfig :: HotReloadConfig
  , hubChan :: TChan HotReloadEvent
  , hubJs :: TVar (Map.Map Text (Text, Text))
  , hubHtml :: IORef (Map.Map Text (Text, Text))
  , hubError :: TVar (Maybe Text)
  , hubCompiling :: TVar (Maybe Text)
  , -- | Monotonic publication counter: increments on every broadcast, so
    -- clients and tests can order snapshots against events.
    hubRevision :: TVar Int64
  }

-- | A coherent point-in-time view for a new SSE client: cached JS hashes,
-- the current build status, and the publication revision at that instant.
data HotReloadSnapshot = HotReloadSnapshot
  { snapshotJsHashes :: [(Text, Text)]
  , snapshotBuildError :: Maybe Text
  , snapshotCompiling :: Maybe Text
  , snapshotRevision :: Int64
  }
  deriving (Show, Eq)

-- | The 'HotReloadConfig' the hub was created with.
hotReloadConfig :: HotReloadHub -> HotReloadConfig
hotReloadConfig = hubConfig

-- | Create a hub with an empty cache and no build status.
newHotReloadHub :: HotReloadConfig -> IO HotReloadHub
newHotReloadHub cfg = do
  chan <- newBroadcastTChanIO
  js <- newTVarIO Map.empty
  html <- newIORef Map.empty
  err <- newTVarIO Nothing
  compiling <- newTVarIO Nothing
  rev <- newTVarIO 0
  pure
    HotReloadHub
      { hubConfig = cfg
      , hubChan = chan
      , hubJs = js
      , hubHtml = html
      , hubError = err
      , hubCompiling = compiling
      , hubRevision = rev
      }

-- | Broadcast an event to all subscribers and update build status in one
-- transaction, so a concurrent 'subscribeWithSnapshot' observes a coherent
-- publication (never a status change without its event or vice versa). The
-- revision increments with each publication.
broadcastEvent :: HotReloadHub -> HotReloadEvent -> IO ()
broadcastEvent hub ev =
  atomically $ do
    case ev of
      BuildError msg -> do
        writeTVar (hubError hub) (Just msg)
        writeTVar (hubCompiling hub) Nothing
      BuildStart app -> do
        writeTVar (hubError hub) Nothing
        writeTVar (hubCompiling hub) (Just app)
      JsUpdate {} -> do
        writeTVar (hubError hub) Nothing
        writeTVar (hubCompiling hub) Nothing
      PageReload {} -> writeTVar (hubCompiling hub) Nothing
      _ -> pure ()
    modifyTVar' (hubRevision hub) (+ 1)
    writeTChan (hubChan hub) ev

-- | Duplicate the broadcast channel for one SSE client.
subscribe :: HotReloadHub -> IO (IO HotReloadEvent)
subscribe hub = do
  ch <- atomically $ dupTChan (hubChan hub)
  pure (atomically (readTChan ch))

-- | Take the cached state and register a subscription in a single STM
-- transaction. Any event published after this point is delivered on the
-- returned stream; every event published before is reflected in the
-- snapshot. This closes the snapshot/subscribe race: the old split reads
-- could take a stale snapshot and then subscribe after the update event
-- had already been broadcast.
subscribeWithSnapshot ::
  HotReloadHub -> IO (HotReloadSnapshot, IO HotReloadEvent)
subscribeWithSnapshot hub =
  atomically $ do
    js <- readTVar (hubJs hub)
    err <- readTVar (hubError hub)
    comp <- readTVar (hubCompiling hub)
    rev <- readTVar (hubRevision hub)
    ch <- dupTChan (hubChan hub)
    let
      snap =
        HotReloadSnapshot
          { snapshotJsHashes = [(k, h) | (k, (_, h)) <- Map.toList js]
          , snapshotBuildError = err
          , snapshotCompiling = comp
          , snapshotRevision = rev
          }
    pure (snap, atomically (readTChan ch))

-- | SSE @data:@ JSON line (no trailing blank line).
encodeEvent :: HotReloadEvent -> Text
encodeEvent = decodeUtf8 . LBS.toStrict . encode

instance ToJSON HotReloadEvent where
  toJSON = \case
    JsUpdate name u h ->
      object
        [ "type" .= ("js-update" :: Text)
        , "appName" .= name
        , "url" .= u
        , "hash" .= h
        ]
    CssUpdate u ts ->
      object
        [ "type" .= ("css-update" :: Text)
        , "url" .= u
        , "timestamp" .= ts
        ]
    PageReload why ->
      object
        [ "type" .= ("page-reload" :: Text)
        , "reason" .= why
        ]
    BuildError msg ->
      object
        [ "type" .= ("build-error" :: Text)
        , "message" .= msg
        ]
    BuildStart app ->
      object
        [ "type" .= ("build-start" :: Text)
        , "appName" .= app
        ]
    Hello hashes ->
      object
        [ "type" .= ("hello" :: Text)
        , "jsHashes" .= object [Key.fromText k .= v | (k, v) <- hashes]
        ]

-- | Cache compiled JS and return its content hash.
registerJs :: HotReloadHub -> Text -> Text -> IO Text
registerJs hub name source = do
  let
    h = jsHash source
  atomically $ modifyTVar' (hubJs hub) (Map.insert name (source, h))
  pure h

-- | Look up cached JS source and hash by app name.
lookupJs :: HotReloadHub -> Text -> IO (Maybe (Text, Text))
lookupJs hub name = Map.lookup name <$> readTVarIO (hubJs hub)

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

-- | Snapshot the app-name to JS hash map for a new SSE client. Prefer
-- 'subscribeWithSnapshot', which reads this together with the channel in
-- one transaction.
currentJsHashes :: HotReloadHub -> IO [(Text, Text)]
currentJsHashes hub = do
  m <- readTVarIO (hubJs hub)
  pure [(k, h) | (k, (_, h)) <- Map.toList m]

-- | The current monotonic publication revision.
currentRevision :: HotReloadHub -> IO Int64
currentRevision = readTVarIO . hubRevision

-- | Record and broadcast a build error.
setBuildError :: HotReloadHub -> Text -> IO ()
setBuildError hub msg = broadcastEvent hub (BuildError msg)

-- | The most recent build error, if any.
lastBuildError :: HotReloadHub -> IO (Maybe Text)
lastBuildError = readTVarIO . hubError

-- | Record and broadcast that an app started compiling.
setBuildStart :: HotReloadHub -> Text -> IO ()
setBuildStart hub app = broadcastEvent hub (BuildStart app)

-- | The app currently compiling, if any.
lastCompiling :: HotReloadHub -> IO (Maybe Text)
lastCompiling = readTVarIO . hubCompiling
