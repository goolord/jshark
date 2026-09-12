{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import qualified Data.ByteString as BS
import GHC.Clock (getMonotonicTime)
import GHC.IO (evaluate)
import JShark (renderJS)
import JShark.Api (stmts)
import JShark.Api.Types (ClosedEffect)
import qualified JShark.Api.Types as Ty
import JShark.Example.Life (mainJS)
import JShark.Internal (effectfulAST, optimizedEffectSize)

life :: ClosedEffect Ty.Unit
life = stmts mainJS

main :: IO ()
main = do
  raw <- evaluate (optimizedEffectSize life)
  putStrLn $ "rawNodes," ++ show raw
  -- 'effectfulAST' returns a 'Data.ByteString.Builder.Builder', so the only
  -- honest measurement is end-to-end: force the rendered bytes.
  t1 <- getMonotonicTime
  bytes <- evaluate (BS.length (renderJS (effectfulAST life)))
  t2 <- getMonotonicTime
  putStrLn $ show bytes ++ "," ++ show (t2 - t1)
