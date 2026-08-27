{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Version-portable compile probe for regression curves. Restricted to API
-- present at both 28b5085 and HEAD.
module Main (main) where

import Control.Monad (forM_)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import GHC.IO (evaluate)
import JShark (ClosedEffect, effectfulAST, renderJSCompact)
import JShark.Api
import JShark.Rec (Rec (..), (<:))
import qualified JShark.Types as T
import System.Environment (getArgs)

-- | Nested binds + while nests scaled by depth.
progN :: Int -> ClosedEffect T.Unit
progN depth = fromSyntax (build depth)

nestWhile :: Int -> Effect f 'Unit
nestWhile 0 = ffi "step" RecNil
nestWhile n = while_ (ffi "cond" RecNil) (nestWhile (n - 1))

build :: Int -> EffectSyntax f (f 'Unit)
build 0 = done
build d = do
  buf <- bindExpr (newByteArray (number (fromIntegral (d * 8))))
  toSyntax_ (ffi "touch" (arg buf <: RecNil))
  toSyntax_ (nestWhile d)
  build (d - 1)

runOne :: Int -> IO ()
runOne n = do
  start <- getMonotonicTime
  let
    bytes = T.length (renderJSCompact (effectfulAST (progN n)))
  bytes `seq` pure ()
  end <- getMonotonicTime
  evaluate bytes
  putStrLn $ show n ++ "," ++ show bytes ++ "," ++ show (end - start)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "curve" : _ -> forM_ [1, 2, 4, 8, 16] runOne
    nStr : _ -> runOne (read nStr)
    _ -> forM_ [1, 2, 4, 8, 16] runOne
