{-# LANGUAGE RankNTypes #-}

-- | Shared compiler-pipeline benches, one per stage, so a slow compile can
-- be attributed to optimizing, emitting, or formatting.
--
-- Manual profiling only (not CI-gated):
--
--   cabal bench jshark-compiler -- -p 'bindChain'
--   cabal bench jshark-examples-bench -- -p life
module Bench.Stages
  ( stageBenches
  , stageBenchesPure
  )
where

import qualified Data.ByteString as BS
import JShark
  ( ClosedEffect
  , ClosedExpr
  , effectfulProgram
  , pureProgram
  , renderJS
  )
import JShark.Compiler
  ( compileEffect
  , compilePure
  , defaultCompilerConfig
  , readableConfig
  )
import JShark.Internal
  ( effectfulAST
  , optimizedEffectSize
  , optimizedExprSize
  , pureAST
  )
import Test.Tasty.Bench

-- | Stages of an effectful program. The closed term is re-instantiated per
-- run so no stage is shared between benches.
stageBenches :: String -> ClosedEffect u -> Benchmark
stageBenches name prog =
  bgroup
    name
    [ bench "optimize" $ nf (\() -> optimizedEffectSize prog) ()
    , bench "emit" $ nf (\() -> BS.length (renderJS (effectfulAST prog))) ()
    , bench "program" $ nf (\() -> BS.length (renderJS (effectfulProgram prog))) ()
    , bench "compile/default" $
        nfAppIO (\() -> compileEffect defaultCompilerConfig prog) ()
    , bench "compile/readable" $ nfAppIO (\() -> compileEffect readableConfig prog) ()
    ]

-- | 'stageBenches' for a pure expression.
stageBenchesPure :: String -> ClosedExpr u -> Benchmark
stageBenchesPure name prog =
  bgroup
    name
    [ bench "optimize" $ nf (\() -> optimizedExprSize prog) ()
    , bench "emit" $ nf (\() -> BS.length (renderJS (pureAST prog))) ()
    , bench "program" $ nf (\() -> BS.length (renderJS (pureProgram prog))) ()
    , bench "compile/default" $
        nfAppIO (\() -> compilePure defaultCompilerConfig prog) ()
    , bench "compile/readable" $ nfAppIO (\() -> compilePure readableConfig prog) ()
    ]
