{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | Every example must emit JavaScript that parses.
--
-- Compiling the Haskell says nothing about whether the emitted program is
-- syntactically valid — a codegen bug can produce a statement like
-- @.setAttribute(…)@ with no receiver, and only a browser would notice.
-- These cases hand each example's whole program to bun.
--
-- The program is wrapped in an arrow function that is never called, so bun
-- parses every line without running any of it: no DOM, no audio, no
-- listeners. A syntax error anywhere still fails the parse.
module ExampleTests (exampleTests) where

import Control.Concurrent.Async (async, wait)
import qualified Breakout
import qualified Data.Text as T
import JShark (effectfulProgram, renderJSCompact)
import JShark.Api (stmts)
import JShark.Bun.Internal (runJS)
import JShark.Types (ClosedEffect, Universe (Unit))
import qualified Life
import qualified Synth
import Test.Tasty
import Test.Tasty.HUnit
import qualified TodoMvc

data ExampleJs = ExampleJs
  { breakoutJs :: String
  , todoMvcJs :: String
  , synthJs :: String
  , lifeJs :: String
  }

exampleTests :: TestTree
exampleTests =
  after AllSucceed "bun is on PATH" $
    withResource acquireExampleJs (const (pure ())) $ \getExampleJs ->
      testGroup
        "examples emit parseable JS"
        [ parseCachedCase "breakout" (breakoutJs <$> getExampleJs)
        , parseCachedCase "todo-mvc" (todoMvcJs <$> getExampleJs)
        , parseCachedCase "synth" (synthJs <$> getExampleJs)
        , parseCachedCase "life" (lifeJs <$> getExampleJs)
        ]

acquireExampleJs :: IO ExampleJs
acquireExampleJs = do
  breakoutA <- async (renderExample (stmts Breakout.mainJS))
  todoA <- async (renderExample (stmts TodoMvc.mainJS))
  synthA <- async (renderExample (stmts Synth.mainJS))
  lifeA <- async (renderExample (stmts Life.mainJS))
  ExampleJs
    <$> wait breakoutA
    <*> wait todoA
    <*> wait synthA
    <*> wait lifeA

renderExample :: ClosedEffect 'Unit -> IO String
renderExample eff = pure (renderJSCompact (effectfulProgram eff))

parseCachedCase :: String -> IO String -> TestTree
parseCachedCase name getJs = testCase name $ do
  js <- getJs
  -- Bound but never applied: parsed in full, executed not at all.
  let probe = "(() => { const unused = () => (" ++ js ++ "); return 1; })()"
  got <- T.unpack <$> runJS probe
  assertEqual (name ++ " should parse") "1" got
