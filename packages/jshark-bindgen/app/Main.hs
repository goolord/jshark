{-# LANGUAGE OverloadedStrings #-}

-- | @jshark-bindgen@ — TypeScript / JavaScript → JShark FFI wrappers.
module Main (main) where

import JShark.Bindgen.Cli (runMain)

main :: IO ()
main =
  runMain
