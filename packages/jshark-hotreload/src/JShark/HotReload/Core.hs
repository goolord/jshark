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
  , currentSnapshot
  , encodeEvent
  , registerJs
  , lookupJs
  , registerHtml
  , lookupHtml
  )
where

import Control.Concurrent.STM
import Data.Aeson (ToJSON (..), Value, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (Pair)
import Data.Bits (xor)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word32)
import Text.Printf (printf)

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
  HotReloadConfig True True "/__jshark/events" "/__jshark/client.js" 75

-- | Shared broadcast channel, artifact caches, and build-status refs.
--
-- Every mutable field is a 'TVar' so a client's snapshot and its
-- subscription can be taken in one atomic transaction (see
-- 'subscribeWithSnapshot').
data HotReloadHub = HotReloadHub
  { hotReloadConfig :: HotReloadConfig
  -- ^ The 'HotReloadConfig' the hub was created with.
  , hubChan :: TChan HotReloadEvent
  , hubJs :: TVar ArtifactCache
  , hubHtml :: TVar ArtifactCache
  , hubError :: TVar (Maybe Text)
  , hubCompiling :: TVar (Maybe Text)
  , hubRevision :: TVar Int64
  -- ^ Monotonic publication counter: increments on every broadcast.
  }

-- | A coherent point-in-time view: cached JS hashes, the current build
-- status, and the publication revision at that instant.
data HotReloadSnapshot = HotReloadSnapshot
  { snapshotJsHashes :: [(Text, Text)]
  , snapshotBuildError :: Maybe Text
  , snapshotCompiling :: Maybe Text
  , snapshotRevision :: Int64
  }
  deriving (Show, Eq)

-- | Cached artifact source keyed by app name, paired with its 'jsHash'.
type ArtifactCache = Map.Map Text (Text, Text)

-- | Create a hub with an empty cache and no build status.
newHotReloadHub :: HotReloadConfig -> IO HotReloadHub
newHotReloadHub cfg =
  HotReloadHub cfg
    <$> newBroadcastTChanIO
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO Nothing
    <*> newTVarIO Nothing
    <*> newTVarIO 0

-- | Broadcast an event to all subscribers and update build status in one
-- transaction, so a concurrent 'subscribeWithSnapshot' observes a coherent
-- publication (never a status change without its event or vice versa). The
-- revision increments with each publication.
broadcastEvent :: HotReloadHub -> HotReloadEvent -> IO ()
broadcastEvent hub ev = atomically $ do
  let
    status err comp =
      writeTVar (hubError hub) err >> writeTVar (hubCompiling hub) comp
  case ev of
    BuildError msg -> status (Just msg) Nothing
    BuildStart app -> status Nothing (Just app)
    JsUpdate {} -> status Nothing Nothing
    PageReload {} -> writeTVar (hubCompiling hub) Nothing
    _ -> pure ()
  modifyTVar' (hubRevision hub) (+ 1)
  writeTChan (hubChan hub) ev

-- | Duplicate the broadcast channel for one SSE client.
subscribe :: HotReloadHub -> IO (IO HotReloadEvent)
subscribe hub = snd <$> subscribeWithSnapshot hub

-- | Take the cached state and register a subscription in a single STM
-- transaction. Any event published after this point is delivered on the
-- returned stream; every event published before is reflected in the
-- snapshot, so no update can fall between the two.
subscribeWithSnapshot ::
  HotReloadHub -> IO (HotReloadSnapshot, IO HotReloadEvent)
subscribeWithSnapshot hub = atomically $ do
  snap <- snapshotSTM hub
  ch <- dupTChan (hubChan hub)
  pure (snap, atomically (readTChan ch))

-- | The hub's current cached hashes, build status, and revision.
currentSnapshot :: HotReloadHub -> IO HotReloadSnapshot
currentSnapshot = atomically . snapshotSTM

snapshotSTM :: HotReloadHub -> STM HotReloadSnapshot
snapshotSTM hub =
  HotReloadSnapshot
    <$> (hashes <$> readTVar (hubJs hub))
    <*> readTVar (hubError hub)
    <*> readTVar (hubCompiling hub)
    <*> readTVar (hubRevision hub)
 where
  hashes m = [(k, h) | (k, (_, h)) <- Map.toList m]

-- | SSE @data:@ JSON line (no trailing blank line).
encodeEvent :: HotReloadEvent -> Text
encodeEvent = decodeUtf8 . LBS.toStrict . encode

instance ToJSON HotReloadEvent where
  toJSON = \case
    JsUpdate name u h ->
      typed "js-update" ["appName" .= name, "url" .= u, "hash" .= h]
    CssUpdate u ts -> typed "css-update" ["url" .= u, "timestamp" .= ts]
    PageReload why -> typed "page-reload" ["reason" .= why]
    BuildError msg -> typed "build-error" ["message" .= msg]
    BuildStart app -> typed "build-start" ["appName" .= app]
    Hello hashes ->
      typed
        "hello"
        ["jsHashes" .= object [Key.fromText k .= v | (k, v) <- hashes]]

typed :: Text -> [Pair] -> Value
typed ty fields = object (("type" .= ty) : fields)

-- | Cache compiled JS and return its content hash.
registerJs :: HotReloadHub -> Text -> Text -> IO Text
registerJs = register . hubJs

-- | Look up cached JS source and hash by app name.
lookupJs :: HotReloadHub -> Text -> IO (Maybe (Text, Text))
lookupJs hub name = Map.lookup name <$> readTVarIO (hubJs hub)

-- | Cache rendered Lucid HTML and return its content hash.
registerHtml :: HotReloadHub -> Text -> Text -> IO Text
registerHtml = register . hubHtml

-- | Look up cached HTML source and hash by app name.
lookupHtml :: HotReloadHub -> Text -> IO (Maybe (Text, Text))
lookupHtml hub name = Map.lookup name <$> readTVarIO (hubHtml hub)

register :: TVar ArtifactCache -> Text -> Text -> IO Text
register cache name source = do
  atomically $ modifyTVar' cache (Map.insert name (source, h))
  pure h
 where
  h = jsHash source

-- | Short content fingerprint: character length and FNV-1a, as
-- @\<len\>-\<hex\>@. 'Word32' makes the multiply wrap at 32 bits, which is
-- what FNV-1a specifies.
jsHash :: Text -> Text
jsHash t = T.pack (printf "%d-%08x" (T.length t) (T.foldl' step seed t))
 where
  seed = 2166136261 :: Word32
  step h c = (h `xor` fromIntegral (fromEnum c)) * 16777619
