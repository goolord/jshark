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

exampleTests :: TestTree
exampleTests =
  after AllSucceed "bun is on PATH" $
    testGroup
      "examples emit parseable JS"
      [ parseCase "breakout" (stmts Breakout.mainJS)
      , parseCase "todo-mvc" (stmts TodoMvc.mainJS)
      , parseCase "synth" (stmts Synth.mainJS)
      , parseCase "life" (stmts Life.mainJS)
      ]

parseCase :: String -> ClosedEffect 'Unit -> TestTree
parseCase name eff = testCase name $ do
  let
    js = renderJSCompact (effectfulProgram eff)
    -- Bound but never applied: parsed in full, executed not at all.
    probe = "(() => { const unused = () => (" ++ js ++ "); return 1; })()"
  got <- T.unpack <$> runJS probe
  assertEqual (name ++ " should parse") "1" got
