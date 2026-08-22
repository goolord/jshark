{-# LANGUAGE ScopedTypeVariables #-}

{- | Run raw JavaScript with bun. Plumbing for 'JShark.Bun'.

A caller holding a 'String' of JavaScript has already left the typed
subset, so this stays behind @.Internal@ rather than in the public API.
-}
module JShark.Bun.Internal
  ( JSProgram (..)
  , plainProgram
  , runProgram
  , runJS
  , runJSWith
  , bunTimeoutMicroseconds
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, catch, throwIO)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import JShark (escapeJsString)
import System.Directory
  ( createDirectory
  , doesFileExist
  , findExecutable
  , getTemporaryDirectory
  , removeFile
  , removePathForcibly
  )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hClose, hPutStrLn, openTempFile, stderr, withFile)
import System.Process
  ( CreateProcess (..)
  , ProcessHandle
  , StdStream (NoStream, UseHandle)
  , getProcessExitCode
  , proc
  , terminateProcess
  , withCreateProcess
  )

{- | Wall-clock ceiling on one bun run. The object language has
'JShark.Types.While' and 'JShark.Timers', so a program that never
terminates is expressible; without a ceiling the caller waits forever.
-}
bunTimeoutMicroseconds :: Int
bunTimeoutMicroseconds = 10 * 1000 * 1000

{- | An ES module to run under bun.

'jsExpression' is the term under evaluation; its value becomes the
result. 'jsPrelude' runs first (this is where a DOM environment installs
its globals) and 'jsEpilogue' last, in a @finally@, so a prelude that
holds the event loop open still shuts itself down when the program throws.
-}
data JSProgram = JSProgram
  { jsFlags :: [String]
  -- ^ Flags for bun itself, ahead of the script path.
  , jsPrelude :: String
  , jsExpression :: String
  , jsEpilogue :: String
  }

-- | Just an expression: no environment, no flags.
plainProgram :: String -> JSProgram
plainProgram js = JSProgram [] "" js ""

{- | Evaluate a JavaScript expression with bun and return
@JSON.stringify@ of its value (@\"undefined\"@ when that value is
@undefined@). A thenable result is awaited first.

The JSON travels through a file, not stdout, so writes from the program
itself (@console.log@) do not corrupt the result. Requires bun on @PATH@.
-}
runJS :: String -> IO Text
runJS = runJSWith bunTimeoutMicroseconds

-- | 'runJS' with an explicit timeout in microseconds.
runJSWith :: Int -> String -> IO Text
runJSWith limit = runProgram limit . plainProgram

-- | 'runJS' for a program that needs a prelude or bun flags.
runProgram :: Int -> JSProgram -> IO Text
runProgram limit p = do
  bun <- requireBun
  tmp <- getTemporaryDirectory
  withRunDir tmp $ \dir -> do
    let
      scriptPath = dir </> "program.mjs"
      resultPath = dir </> "result.json"
      outPath = dir </> "stdout.txt"
      errPath = dir </> "stderr.txt"
      source = script resultPath
      -- Report the whole module: when the prelude is what failed (a DOM
      -- environment that could not be resolved), the expression alone
      -- points at the wrong place.
      die msg = do
        out <- readIfPresent outPath
        err <- readIfPresent errPath
        throwIO . userError . concat $
          ["JShark.Bun: ", msg]
            ++ ["\nstderr:\n" ++ T.unpack err | not (T.null (T.strip err))]
            ++ ["\nstdout:\n" ++ T.unpack out | not (T.null (T.strip out))]
            ++ ["\njs:\n", source]
    BS.writeFile scriptPath (encodeUtf8 (T.pack source))
    code <- spawnBun bun (jsFlags p) scriptPath outPath errPath limit
    case code of
      Nothing ->
        die
          ( "bun timed out after "
              ++ show limit
              ++ "us and was killed (non-terminating program?)"
          )
      Just (ExitFailure n) -> die ("bun exited " ++ show n)
      Just ExitSuccess -> do
        raw <- BS.readFile resultPath
        if BS.null raw
          then die "bun wrote no result (did the program exit early?)"
          else pure (decodeUtf8With lenientDecode raw)
  where
    -- The result leaves through 'resultPath'. Using stdout would mix it
    -- with whatever the program itself logs. ES imports hoist, so the
    -- prelude may carry its own.
    script resultPath =
      unlines
        [ "import { writeFileSync } from \"node:fs\";"
        , jsPrelude p
        , "try {"
        , "  const $raw = (" ++ jsExpression p ++ ");"
        , "  const $thenable ="
        , "    $raw !== null &&"
        , "    (typeof $raw === \"object\" || typeof $raw === \"function\") &&"
        , "    typeof $raw.then === \"function\";"
        , -- Only a thenable is awaited: an unconditional await would add a
          -- microtask tick, letting a pending timer fire before the result
          -- is read.
          "  const $jshark = $thenable ? await $raw : $raw;"
        , "  const $json = JSON.stringify($jshark);"
        , "  writeFileSync("
            ++ jsString resultPath
            ++ ", $json === undefined ? \"undefined\" : $json);"
        , "} finally {"
        , jsEpilogue p
        , "}"
        ]
    jsString s = '"' : escapeJsString s ++ "\""

readIfPresent :: FilePath -> IO Text
readIfPresent path = do
  there <- doesFileExist path
  if there
    then decodeUtf8With lenientDecode <$> BS.readFile path
    else pure T.empty

{- | Run bun with stdio on files, and kill it if it outlives @limit@.

stdout and stderr go to files rather than pipes so there is nothing to
drain: a pipe left unread by a killed reader thread would deadlock. The
wait polls 'getProcessExitCode' instead of blocking in @waitForProcess@,
because an async exception cannot interrupt a blocking foreign call —
'System.Timeout.timeout' around a wait leaves the child spinning.
-}
spawnBun ::
  FilePath
  -> [String]
  -> FilePath
  -> FilePath
  -> FilePath
  -> Int
  -> IO (Maybe ExitCode)
spawnBun bun flags scriptPath outPath errPath limit =
  withFile outPath WriteMode $ \hOut ->
    withFile errPath WriteMode $ \hErr -> do
      let
        cp =
          (proc bun (flags ++ [scriptPath]))
            { std_in = NoStream
            , std_out = UseHandle hOut
            , std_err = UseHandle hErr
            }
      withCreateProcess cp $ \_ _ _ ph -> do
        res <- waitBounded limit ph
        case res of
          Just c -> pure (Just c)
          Nothing -> do
            terminateProcess ph
            -- Reap the kill so the handles are released before cleanup.
            _ <- waitBounded (1 * 1000 * 1000) ph
            pure Nothing

{- | Poll for exit until @limit@ microseconds of wall clock have passed.
Elapsed time comes from the monotonic clock, not a count of sleeps, which
would ignore the time each poll itself takes.
-}
waitBounded :: Int -> ProcessHandle -> IO (Maybe ExitCode)
waitBounded limit ph = do
  start <- getMonotonicTimeNSec
  go start
  where
    stepMicroseconds = 10 * 1000
    limitNanoseconds = fromIntegral limit * 1000 :: Word64
    go start = do
      m <- getProcessExitCode ph
      case m of
        Just c -> pure (Just c)
        Nothing -> do
          now <- getMonotonicTimeNSec
          if now - start >= limitNanoseconds
            then pure Nothing
            else do
              threadDelay stepMicroseconds
              go start

{- | A private directory for one run, holding the script, the result, and
bun's stdout and stderr.

'openTempFile' supplies the unique name and the directory hangs off it,
so there is no delete-then-create race. The handles are closed before bun
starts: it opens these paths itself, and on Windows an open handle would
lock them.
-}
withRunDir :: FilePath -> (FilePath -> IO a) -> IO a
withRunDir parent act = bracket acquire release (act . snd)
  where
    acquire = do
      (path, h) <- openTempFile parent "jshark-bun.tmp"
      hClose h
      let dir = path ++ ".d"
      createDirectory dir
      pure (path, dir)
    release (path, dir) = do
      removeQuietly dir (removePathForcibly dir)
      removeQuietly path (removeFile path)

removeQuietly :: FilePath -> IO () -> IO ()
removeQuietly path action =
  action
    `catch` \(e :: IOException) ->
      hPutStrLn stderr ("JShark.Bun: could not remove " ++ path ++ ": " ++ show e)

requireBun :: IO FilePath
requireBun = do
  m <- findExecutable "bun"
  case m of
    Just p -> pure p
    Nothing ->
      throwIO (userError "JShark.Bun: bun not found on PATH; install https://bun.sh")
