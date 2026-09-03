{-# LANGUAGE CPP #-}

-- | Capture stderr from an in-process 'IO' action (restores stderr afterward).
module CaptureStderr (captureStderr) where

import Control.Exception (evaluate, finally)
import Foreign (Ptr, allocaArray, peekElemOff)
import Foreign.C.Types (CInt (..))
import GHC.IO.Handle.FD (fdToHandle)
import System.IO
  ( BufferMode (..)
  , hClose
  , hFlush
  , hGetContents
  , hSetBuffering
  , stderr
  )

#if defined(mingw32_HOST_OS)
foreign import ccall "msvcrt _pipe" c_pipe
  :: Ptr CInt -> CInt -> CInt -> IO CInt

foreign import ccall "msvcrt _dup" c_dup :: CInt -> IO CInt

foreign import ccall "msvcrt _dup2" c_dup2 :: CInt -> CInt -> IO CInt

foreign import ccall "msvcrt _close" c_close :: CInt -> IO CInt
#else
foreign import ccall "pipe" c_pipe :: Ptr CInt -> IO CInt

foreign import ccall "dup" c_dup :: CInt -> IO CInt

foreign import ccall "dup2" c_dup2 :: CInt -> CInt -> IO CInt

foreign import ccall "close" c_close :: CInt -> IO CInt
#endif

stdErrorFd :: CInt
stdErrorFd = 2

captureStderr :: IO a -> IO (a, String)
captureStderr io =
  allocaArray 2 $ \pfds -> do
#if defined(mingw32_HOST_OS)
    rc <- c_pipe pfds 4096 0
#else
    rc <- c_pipe pfds
#endif
    if rc /= 0
      then ioError (userError "captureStderr: pipe failed")
      else pure ()
    readFd <- peekElemOff pfds 0
    writeFd <- peekElemOff pfds 1
    backup <- c_dup stdErrorFd
    _ <- c_dup2 writeFd stdErrorFd
    _ <- c_close writeFd
    result <-
      io `finally` do
        hFlush stderr
        _ <- c_dup2 backup stdErrorFd
        _ <- c_close backup
        pure ()
    readH <- fdToHandle readFd
    hSetBuffering readH NoBuffering
    msg <- hGetContents readH
    _ <- evaluate (length msg)
    hClose readH
    pure (result, msg)
