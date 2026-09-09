{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Compile pure 'Expr' kernels to Bend for the HVM2 pipeline.
--
-- Pipeline: JShark 'ClosedExpr' → Bend (.bend) → HVM2 ('bend gen-c') → C →
-- WASM (Zig @wasm/hvm2/build.zig@, same role as Life's Zig wasm build).
-- HVM2 is the interaction-combinator runtime; Bend is its frontend.
--
-- The demo module assembly and WASM bridge live in
-- @JShark.Example.Hvm2Demo.WasmBuild@ (examples package).
module JShark.Hvm2
  ( Hvm2Error (..)
  , bendKernel
  , bendDefNames
  , bendDefExports
  , emitKernelExportsC
  , sanitizeKernelCForWasm
  )
where

import Data.Text (Text)
import JShark (irExprFromClosed)
import JShark.Api.Types (ClosedExpr)
import JShark.Compiler.EmitBend
  ( Hvm2Error (..)
  , bendDefExports
  , bendDefNames
  , emitBendKernel
  , emitKernelExportsC
  , sanitizeKernelCForWasm
  )

bendKernel :: Text -> ClosedExpr u -> Either Hvm2Error Text
bendKernel name closed = emitBendKernel name (irExprFromClosed closed)
