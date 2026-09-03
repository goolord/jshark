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
  , nodeCountEff
  , nodeCountExpr
  , optimize
  , optimizeEffect
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
emit :: ClosedEffect u -> T.Text
emit e = renderJSCompact (effectfulAST e)
{-# NOINLINE emit #-}

emitLen :: ClosedEffect u -> Int
emitLen e = T.length (emit e)
{-# NOINLINE emitLen #-}

-- | PHOAS optimizer node count (the path 'effectfulAST' uses).
optEffectNodes :: ClosedEffect u -> Int
optEffectNodes e =
  let
    r = optimizeEffect e
   in
    nodeCountEff r
{-# NOINLINE optEffectNodes #-}

optExprNodes :: ClosedExpr u -> Int
optExprNodes e =
  let
    r = optimize e
   in
    nodeCountExpr r
{-# NOINLINE optExprNodes #-}

runClosedEffect :: (ClosedEffect u -> b) -> ClosedEffect u -> b
runClosedEffect f e = f e
{-# NOINLINE runClosedEffect #-}

runClosedEffectIO :: (ClosedEffect u -> IO b) -> ClosedEffect u -> IO b
runClosedEffectIO f e = f e
{-# NOINLINE runClosedEffectIO #-}

runClosedExpr :: (ClosedExpr u -> b) -> ClosedExpr u -> b
runClosedExpr f e = f e
{-# NOINLINE runClosedExpr #-}

runClosedExprIO :: (ClosedExpr u -> IO b) -> ClosedExpr u -> IO b
runClosedExprIO f e = f e
{-# NOINLINE runClosedExprIO #-}

-- | @nf@ of a rank-2 closed term. Composition with 'ClosedEffect'
-- instantiates @f@ too early.
nfClosed :: NFData b => (ClosedEffect u -> b) -> ClosedEffect u -> Benchmarkable
nfClosed f e = nf (\() -> runClosedEffect f e) ()

nfAppClosed ::
  NFData b => (ClosedEffect u -> IO b) -> ClosedEffect u -> Benchmarkable
nfAppClosed f e = nfAppIO (\() -> runClosedEffectIO f e) ()

nfPure :: NFData b => (ClosedExpr u -> b) -> ClosedExpr u -> Benchmarkable
nfPure f e = nf (\() -> runClosedExpr f e) ()

nfAppPure :: NFData b => (ClosedExpr u -> IO b) -> ClosedExpr u -> Benchmarkable
nfAppPure f e = nfAppIO (\() -> runClosedExprIO f e) ()

-- | Per-stage split for a single microprogram (no precomputed 'env').
codepathStages :: String -> ClosedEffect u -> Benchmark
codepathStages name prog =
  bgroup
    name
    [ bench "optimizeEffect" $ nfClosed optEffectNodes prog
    , bench "optNodes+emit/bytes" $
        nfClosed (\e -> (optEffectNodes e, emitLen e)) prog
    , bench "effectfulAST" $
        nfClosed (\e -> T.length (renderJS (effectfulAST e))) prog
    , bench "emit" $ nfClosed emit prog
    , bench "emit/bytes" $ nfClosed emitLen prog
    , bench "prettyJS/e2e" $ nfAppClosed (\e -> prettyJS (emit e)) prog
    , bench "compileEffect/readable/e2e" $
        nfAppClosed (compileEffect readableConfig) prog
    ]

codepathStagesPure :: String -> ClosedExpr u -> Benchmark
codepathStagesPure name prog =
  bgroup
    name
    [ bench "optimize" $ nfPure optExprNodes prog
    , bench "optNodes+emit/bytes" $
        nfPure (\e -> (optExprNodes e, T.length (renderJSCompact (pureAST e)))) prog
    , bench "pureAST" $ nfPure (\e -> T.length (renderJS (pureAST e))) prog
    , bench "emit" $ nfPure (\e -> renderJSCompact (pureAST e)) prog
    , bench "emit/bytes" $ nfPure (\e -> T.length (renderJSCompact (pureAST e))) prog
    , bench "prettyJS/e2e" $
        nfAppPure (\e -> prettyJS (renderJSCompact (pureAST e))) prog
    , bench "compilePure/readable/e2e" $ nfAppPure (compilePure readableConfig) prog
    ]

stageBenches :: String -> ClosedEffect u -> Benchmark
stageBenches name prog =
  bgroup
    name
    [ bench "optimizeEffect" $ nfClosed optEffectNodes prog
    , bench "optNodes+emit/bytes" $
        nfClosed (\e -> (optEffectNodes e, emitLen e)) prog
    , bench "effectfulAST" $
        nfClosed (\e -> T.length (renderJS (effectfulAST e))) prog
    , bench "renderJSCompact" $ nfClosed emit prog
    , bench "emit/bytes" $ nfClosed emitLen prog
    , bench "effectfulProgram" $
        nfClosed (\e -> renderJSCompact (effectfulProgram e)) prog
    , bench "prettyJS/e2e" $ nfAppClosed (\e -> prettyJS (emit e)) prog
    , env (pure (emit prog)) $ \js ->
        bench "prettyJS/precomputed" $ nfAppIO (\() -> prettyJS js) ()
    , bench "compileEffect/readable/e2e" $
        nfAppClosed (compileEffect readableConfig) prog
    , bench "compileEffect/passthrough/e2e" $
        nfAppClosed (compileEffect passthroughConfig) prog
    , bench "compileEffect/default/e2e" $
        nfAppClosed (compileEffect defaultCompilerConfig) prog
    ]

stageBenchesPure :: String -> ClosedExpr u -> Benchmark
stageBenchesPure name prog =
  bgroup
    name
    [ bench "optimize" $ nfPure optExprNodes prog
    , bench "optNodes+emit/bytes" $
        nfPure (\e -> (optExprNodes e, T.length (renderJSCompact (pureAST e)))) prog
    , bench "pureAST" $ nfPure (\e -> T.length (renderJS (pureAST e))) prog
    , bench "renderJSCompact" $ nfPure (\e -> renderJSCompact (pureAST e)) prog
    , bench "emit/bytes" $ nfPure (\e -> T.length (renderJSCompact (pureAST e))) prog
    , bench "pureProgram" $ nfPure (\e -> renderJSCompact (pureProgram e)) prog
    , bench "prettyJS/e2e" $
        nfAppPure (\e -> prettyJS (renderJSCompact (pureAST e))) prog
    , env (pure (renderJSCompact (pureAST prog))) $ \js ->
        bench "prettyJS/precomputed" $ nfAppIO (\() -> prettyJS js) ()
    , bench "compilePure/readable/e2e" $ nfAppPure (compilePure readableConfig) prog
    , bench "compilePure/passthrough/e2e" $
        nfAppPure (compilePure passthroughConfig) prog
    , bench "compilePure/default/e2e" $
        nfAppPure (compilePure defaultCompilerConfig) prog
    ]
