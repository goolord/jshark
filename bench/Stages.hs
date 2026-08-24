{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | Shared compiler-pipeline benches. Each name is one stage so a
-- slow Life compile can be attributed (optimize vs emit vs pretty).
--
-- Manual profiling only (not CI-gated). Typical workflow:
--
--   cabal bench jshark-compiler
--   cabal bench jshark-compiler -- jshark-compiler -p 'stages/lifeStep'
--   cabal bench jshark-compiler-examples -- jshark-compiler-examples -p life
module Stages
  ( emit
  , emitLen
  , nfClosed
  , nfPure
  , stageBenches
  , stageBenchesPure
  , codepathStages
  , codepathStagesPure
  )
where

import Control.DeepSeq (NFData)
import qualified Data.Text as T
import JShark
  ( ClosedEffect
  , ClosedExpr
  , effectfulAST
  , effectfulProgram
  , optimizedEffectSize
  , optimizedExprSize
  , pureAST
  , pureProgram
  , renderJS
  , renderJSCompact
  )
import JShark.Compiler
  ( compileEffect
  , compilePure
  , defaultCompilerConfig
  , passthroughConfig
  , prettyJS
  , readableConfig
  )
import Test.Tasty.Bench

-- | Optimize, lower, and compact-render (the path 'compileEffect' uses).
emit :: ClosedEffect u -> String
emit e = renderJSCompact (effectfulAST e)

emitLen :: ClosedEffect u -> Int
emitLen e = length (emit e)

-- | @nf@ of a rank-2 closed term. Composition with 'ClosedEffect'
-- instantiates @f@ too early.
nfClosed :: NFData b => (ClosedEffect u -> b) -> ClosedEffect u -> Benchmarkable
nfClosed f e = nf (\() -> f e) ()

nfAppClosed ::
  NFData b => (ClosedEffect u -> IO b) -> ClosedEffect u -> Benchmarkable
nfAppClosed f e = nfAppIO (\_ -> f e) ()

nfPure :: NFData b => (ClosedExpr u -> b) -> ClosedExpr u -> Benchmarkable
nfPure f e = nf (\() -> f e) ()

nfAppPure :: NFData b => (ClosedExpr u -> IO b) -> ClosedExpr u -> Benchmarkable
nfAppPure f e = nfAppIO (\_ -> f e) ()

-- | Per-stage split for a single microprogram (no precomputed 'env').
codepathStages :: String -> ClosedEffect u -> Benchmark
codepathStages name prog =
  bgroup
    name
    [ bench "optimizeEffect" $ nfClosed optimizedEffectSize prog
    , bench "optNodes+emit/bytes" $ nfClosed (\e -> (optimizedEffectSize e, emitLen e)) prog
    , bench "effectfulAST" $ nfClosed (\e -> length (renderJS (effectfulAST e))) prog
    , bench "emit" $ nfClosed emit prog
    , bench "emit/bytes" $ nfClosed emitLen prog
    , bench "prettyJS/e2e" $ nfClosed (\e -> prettyJS (T.pack (emit e))) prog
    , bench "compileEffect/readable/e2e" $ nfAppClosed (compileEffect readableConfig) prog
    ]

codepathStagesPure :: String -> ClosedExpr u -> Benchmark
codepathStagesPure name prog =
  bgroup
    name
    [ bench "optimize" $ nfPure optimizedExprSize prog
    , bench "optNodes+emit/bytes" $ nfPure (\e -> (optimizedExprSize e, length (renderJSCompact (pureAST e)))) prog
    , bench "pureAST" $ nfPure (\e -> length (renderJS (pureAST e))) prog
    , bench "emit" $ nfPure (\e -> renderJSCompact (pureAST e)) prog
    , bench "emit/bytes" $ nfPure (\e -> length (renderJSCompact (pureAST e))) prog
    , bench "prettyJS/e2e" $ nfPure (\e -> prettyJS (T.pack (renderJSCompact (pureAST e)))) prog
    , bench "compilePure/readable/e2e" $ nfAppPure (compilePure readableConfig) prog
    ]

stageBenches :: String -> ClosedEffect u -> Benchmark
stageBenches name prog =
  bgroup
    name
    [ bench "optimizeEffect" $ nfClosed optimizedEffectSize prog
    , bench "optNodes+emit/bytes" $ nfClosed (\e -> (optimizedEffectSize e, emitLen e)) prog
    , bench "effectfulAST" $ nfClosed (\e -> length (renderJS (effectfulAST e))) prog
    , bench "renderJSCompact" $ nfClosed emit prog
    , bench "emit/bytes" $ nfClosed emitLen prog
    , bench "effectfulProgram" $ nfClosed (\e -> renderJSCompact (effectfulProgram e)) prog
    , bench "prettyJS/e2e" $ nfClosed (\e -> prettyJS (T.pack (emit e))) prog
    , env (pure (T.pack (emit prog))) $ \js ->
        bench "prettyJS/precomputed" $ nf prettyJS js
    , bench "compileEffect/readable/e2e" $ nfAppClosed (compileEffect readableConfig) prog
    , bench "compileEffect/passthrough/e2e" $ nfAppClosed (compileEffect passthroughConfig) prog
    , bench "compileEffect/default/e2e" $ nfAppClosed (compileEffect defaultCompilerConfig) prog
    ]

stageBenchesPure :: String -> ClosedExpr u -> Benchmark
stageBenchesPure name prog =
  bgroup
    name
    [ bench "optimize" $ nfPure optimizedExprSize prog
    , bench "optNodes+emit/bytes" $ nfPure (\e -> (optimizedExprSize e, length (renderJSCompact (pureAST e)))) prog
    , bench "pureAST" $ nfPure (\e -> length (renderJS (pureAST e))) prog
    , bench "renderJSCompact" $ nfPure (\e -> renderJSCompact (pureAST e)) prog
    , bench "emit/bytes" $ nfPure (\e -> length (renderJSCompact (pureAST e))) prog
    , bench "pureProgram" $ nfPure (\e -> renderJSCompact (pureProgram e)) prog
    , bench "prettyJS/e2e" $ nfPure (\e -> prettyJS (T.pack (renderJSCompact (pureAST e)))) prog
    , env (pure (T.pack (renderJSCompact (pureAST prog)))) $ \js ->
        bench "prettyJS/precomputed" $ nf prettyJS js
    , bench "compilePure/readable/e2e" $ nfAppPure (compilePure readableConfig) prog
    , bench "compilePure/passthrough/e2e" $ nfAppPure (compilePure passthroughConfig) prog
    , bench "compilePure/default/e2e" $ nfAppPure (compilePure defaultCompilerConfig) prog
    ]
