{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | Shared compiler-pipeline benches. Each name is one stage so a
-- slow Life compile can be attributed (optimize vs emit vs pretty).
module Stages
  ( emit
  , nfClosed
  , stageBenches
  , stageBenchesPure
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
  , renderJSCompact
  )
import JShark.Compiler
  ( compileEffect
  , compilePure
  , passthroughConfig
  , prettyJS
  , readableConfig
  )
import Test.Tasty.Bench

-- | The emit used by 'compileEffect' before 'prettyJS' / minify.
emit :: ClosedEffect u -> String
emit e = renderJSCompact (effectfulAST e)

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

stageBenches :: String -> ClosedEffect u -> Benchmark
stageBenches name prog =
  bgroup
    name
    [ bench "optimizeEffect" $ nfClosed optimizedEffectSize prog
    , bench "effectfulAST" $ whnf (\() -> effectfulAST prog) ()
    , bench "renderJSCompact" $ nfClosed emit prog
    , bench "effectfulProgram" $ nfClosed (\e -> renderJSCompact (effectfulProgram e)) prog
    , env (pure (T.pack (emit prog))) $ \js ->
        bench "prettyJS" $ nf prettyJS js
    , bench "compileEffect/readable" $ nfAppClosed (compileEffect readableConfig) prog
    , bench "compileEffect/passthrough" $ nfAppClosed (compileEffect passthroughConfig) prog
    ]

stageBenchesPure :: String -> ClosedExpr u -> Benchmark
stageBenchesPure name prog =
  bgroup
    name
    [ bench "optimize" $ nfPure optimizedExprSize prog
    , bench "pureAST" $ whnf (\() -> pureAST prog) ()
    , bench "renderJSCompact" $ nfPure (\e -> renderJSCompact (pureAST e)) prog
    , bench "pureProgram" $ nfPure (\e -> renderJSCompact (pureProgram e)) prog
    , env (pure (T.pack (renderJSCompact (pureAST prog)))) $ \js ->
        bench "prettyJS" $ nf prettyJS js
    , bench "compilePure/readable" $ nfAppPure (compilePure readableConfig) prog
    , bench "compilePure/passthrough" $ nfAppPure (compilePure passthroughConfig) prog
    ]
