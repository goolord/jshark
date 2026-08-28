{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- | Zig-compatible programmatic progress reporting.
--
-- When @JSHARK_PROGRESS@ or @ZIG_PROGRESS@ is set to a writable,
-- non-blocking file descriptor, compile progress is streamed as binary
-- tree updates instead of drawing to stderr.
--
-- Wire format matches the
-- [Zig Progress Protocol](https://andrewkelley.me/post/zig-new-cli-progress-bar-explained.html).
module JShark.Compiler.CompileProgressProtocol
  ( ProgressNode (..)
  , ProgressParent (..)
  , progressFdFromEnv
  , encodeProgressMessage
  , decodeProgressMessage
  , writeProgressMessage
  , drainProgressFd
  , maxProgressNodes
  )
where

import Control.Exception (catch, throwIO)
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word32, Word8)
import GHC.IO.Exception (IOErrorType (..), ioe_type)
import GHC.IO.Unsafe (unsafePerformIO)
import System.Environment (lookupEnv)
import System.IO.Error (isResourceVanishedError)
import qualified System.Posix.IO.ByteString as PB
import System.Posix.Types (Fd (..))

data ProgressParent
  = ProgressRoot
  | ProgressChild Word8
  deriving (Eq, Show)

data ProgressNode = ProgressNode
  { pnCompleted :: !Word32
  , pnEstimatedTotal :: !Word32
  , pnName :: !BS.ByteString
  , pnParent :: !ProgressParent
  }
  deriving (Eq, Show)

progressNameLimit :: Int
progressNameLimit = 40

maxProgressNodes :: Int
maxProgressNodes = 253

progressNoneParent :: Word8
progressNoneParent = 0xff

type ProgressFd = Fd

{-# NOINLINE progressFdRef #-}
progressFdRef :: IORef (Maybe ProgressFd)
progressFdRef = unsafePerformIO (newIORef Nothing)

progressFdFromEnv :: IO (Maybe ProgressFd)
progressFdFromEnv = do
  cached <- readIORef progressFdRef
  case cached of
    Just fd -> pure (Just fd)
    Nothing -> do
      mFd <- lookupProgressEnv
      case mFd of
        Nothing -> pure Nothing
        Just fd -> do
          writeIORef progressFdRef (Just fd)
          pure (Just fd)

lookupProgressEnv :: IO (Maybe ProgressFd)
lookupProgressEnv = do
  mShark <- lookupEnv "JSHARK_PROGRESS"
  case mShark of
    Just s -> parseFd s
    Nothing -> do
      mZig <- lookupEnv "ZIG_PROGRESS"
      case mZig of
        Just s -> parseFd s
        Nothing -> pure Nothing

parseFd :: String -> IO (Maybe ProgressFd)
parseFd s =
  case reads s of
    [(n, "")] | n >= 0 && n <= 65535 -> pure (Just (Fd n))
    _ -> pure Nothing

encodeProgressMessage :: [ProgressNode] -> BS.ByteString
encodeProgressMessage nodes =
  let
    trimmed = take maxProgressNodes nodes
    len = length trimmed
    lenB = fromIntegral len :: Word8
    nodeBs =
      mconcat
        [ nodeStorage n
        | n <- trimmed
        ]
    parentBs =
      BS.pack
        [ parentByte (pnParent n)
        | n <- trimmed
        ]
   in
    BS.singleton lenB <> nodeBs <> parentBs

nodeStorage :: ProgressNode -> BS.ByteString
nodeStorage n =
  let
    ProgressNode
      { pnCompleted = completed
      , pnEstimatedTotal = estimated
      , pnName = name
      } =
        n
   in
    word32le completed
      <> word32le estimated
      <> padName name

padName :: BS.ByteString -> BS.ByteString
padName name =
  let
    raw = BS.take progressNameLimit name
    pad = progressNameLimit - BS.length raw
   in
    raw <> BS.replicate pad 0

parentByte :: ProgressParent -> Word8
parentByte p = case p of
  ProgressRoot -> progressNoneParent
  ProgressChild i -> i

word32le :: Word32 -> BS.ByteString
word32le w =
  BS.pack
    [ fromIntegral w
    , fromIntegral (w `Bits.shiftR` 8)
    , fromIntegral (w `Bits.shiftR` 16)
    , fromIntegral (w `Bits.shiftR` 24)
    ]

word32leRead :: BS.ByteString -> (Word32, BS.ByteString)
word32leRead bs =
  case BS.uncons bs of
    Nothing -> (0, BS.empty)
    Just (b0, rest1) ->
      case BS.uncons rest1 of
        Nothing -> (fromIntegral b0, BS.empty)
        Just (b1, rest2) ->
          case BS.uncons rest2 of
            Nothing -> (fromIntegral b0 + fromIntegral b1 * 256, BS.empty)
            Just (b2, rest3) ->
              case BS.uncons rest3 of
                Nothing ->
                  ( fromIntegral b0
                      + fromIntegral b1 * 256
                      + fromIntegral b2 * 65536
                  , BS.empty
                  )
                Just (b3, rest4) ->
                  ( fromIntegral b0
                      + fromIntegral b1 * 256
                      + fromIntegral b2 * 65536
                      + fromIntegral b3 * 16777216
                  , rest4
                  )

decodeProgressMessage :: BS.ByteString -> Maybe [ProgressNode]
decodeProgressMessage bs =
  case BS.uncons bs of
    Nothing -> Nothing
    Just (lenB, rest)
      | fromIntegral lenB > maxProgressNodes -> Nothing
      | otherwise ->
          let
            len = fromIntegral lenB
            storageLen = len * 48
            parentLen = len
           in
            if BS.length rest < storageLen + parentLen
              then Nothing
              else
                let
                  (storage, rest') = BS.splitAt storageLen rest
                  (parents, _) = BS.splitAt parentLen rest'
                 in
                  parseNodes len storage parents

parseNodes :: Int -> BS.ByteString -> BS.ByteString -> Maybe [ProgressNode]
parseNodes 0 _ _ = Just []
parseNodes n storage parents =
  case parseNode storage of
    Nothing -> Nothing
    Just (node, storage') ->
      case BS.uncons parents of
        Nothing -> Nothing
        Just (p, parents') ->
          let
            parent =
              if p == progressNoneParent
                then ProgressRoot
                else ProgressChild p
           in
            case parseNodes (n - 1) storage' parents' of
              Nothing -> Nothing
              Just rest -> Just (node {pnParent = parent} : rest)

parseNode :: BS.ByteString -> Maybe (ProgressNode, BS.ByteString)
parseNode bs
  | BS.length bs < 48 = Nothing
  | otherwise =
      let
        (completed, rest1) = word32leRead bs
        (estimated, rest2) = word32leRead rest1
        (name, rest3) = BS.splitAt progressNameLimit rest2
       in
        Just
          ( ProgressNode
              { pnCompleted = completed
              , pnEstimatedTotal = estimated
              , pnName = BS.takeWhile (/= 0) name
              , pnParent = ProgressRoot
              }
          , rest3
          )

writeProgressMessage :: [ProgressNode] -> IO ()
writeProgressMessage nodes = do
  mFd <- progressFdFromEnv
  case mFd of
    Nothing -> pure ()
    Just fd ->
      let
        msg = encodeProgressMessage nodes
       in
        catch
          (voidWrite fd msg)
          ( \e ->
              if isResourceVanishedError e
                then pure ()
                else throwIO e
          )

voidWrite :: ProgressFd -> BS.ByteString -> IO ()
voidWrite fd msg =
  catch
    (PB.fdWrite fd msg >> pure ())
    ( \e ->
        if isWriteDropError e
          then pure ()
          else throwIO e
    )

isWriteDropError :: IOError -> Bool
isWriteDropError e =
  isResourceVanishedError e || ioe_type e == ResourceExhausted

drainProgressFd :: ProgressFd -> IO (Maybe [ProgressNode])
drainProgressFd fd = go BS.empty
 where
  go acc = do
    chunk <- readChunk fd
    if BS.null chunk
      then pure (lastMessage acc)
      else go (acc <> chunk)

lastMessage :: BS.ByteString -> Maybe [ProgressNode]
lastMessage bs = go 0 Nothing
 where
  go off best =
    let
      rest = BS.drop off bs
     in
      if BS.null rest
        then best
        else case BS.uncons rest of
          Nothing -> best
          Just (lenB, tailBs) ->
            let
              len = fromIntegral lenB
              msgLen = 1 + len * 49
              end = off + msgLen
             in
              if len > maxProgressNodes || BS.length tailBs < len * 49
                then best
                else case decodeProgressMessage (BS.take msgLen (BS.drop off bs)) of
                  Nothing -> best
                  Just nodes -> go end (Just nodes)

readChunk :: ProgressFd -> IO BS.ByteString
readChunk fd = PB.fdRead fd 4096
