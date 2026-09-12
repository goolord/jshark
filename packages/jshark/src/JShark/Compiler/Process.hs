{-# LANGUAGE OverloadedStrings #-}

-- | Shared stdin-to-stdout process helpers for external JS tools.
module JShark.Compiler.Process
  ( executeProcessStdin
  )
where

import Control.Concurrent.Async (wait, withAsync)
import Control.Exception (SomeException, catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Streaming.ByteString as Q
import System.Exit (ExitCode (..))
import System.IO (Handle)
import System.Process.Typed
  ( byteStringInput
  , createPipe
  , getStderr
  , getStdout
  , proc
  , setStderr
  , setStdin
  , setStdout
  , waitExitCode
  , withProcessWait
  )

-- | Run @cmd args@ with @source@ on stdin; return stdout or an error message
-- (stderr when non-empty).
executeProcessStdin ::
  FilePath -> [String] -> ByteString -> IO (Either String ByteString)
executeProcessStdin cmd args source =
  ( do
      let
        pConfig =
          setStdin (byteStringInput (BL.fromStrict source))
            $ setStdout createPipe
            $ setStderr createPipe
            $ proc cmd args
      withProcessWait pConfig $ \p -> do
        (code, outBs, errBs) <-
          withAsync (drainHandle (getStdout p)) $ \outA ->
            withAsync (drainHandle (getStderr p)) $ \errA -> do
              exitCode <- waitExitCode p
              outBs' <- wait outA
              errBs' <- wait errA
              pure (exitCode, outBs', errBs')
        case code of
          ExitSuccess -> pure (Right outBs)
          ExitFailure c ->
            pure
              ( Left
                  ( if BS.null errBs
                      then "Process exited with code " ++ show c
                      else BC.unpack errBs
                  )
              )
  )
    `catch` (\e -> pure (Left (show (e :: SomeException))))
 where
  drainHandle :: Handle -> IO BS.ByteString
  drainHandle h = Q.toStrict_ (Q.hGetContents h)
