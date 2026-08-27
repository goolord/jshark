{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | Every example must emit JavaScript that parses.
--
-- Compiling the Haskell says nothing about whether the emitted program is
-- syntactically valid — a codegen bug can produce a statement like
-- @.setAttribute(…)@ with no receiver, and only a browser would notice.
-- These cases hand each example's whole program to bun.
--
-- The program is parked inside a function that is never called, so bun
-- parses every line without running any of it: no DOM, no audio, no
-- listeners. A syntax error anywhere still fails the parse.
--
-- Each case compiles only its own example via 'compileEffect' +
-- 'readableConfig' (optimized 'effectfulAST', no minifier). Cases do not
-- share a setup hook, so a slow example like Life cannot block Breakout.
module ExampleTests (exampleTests) where

import qualified Breakout
import qualified Data.Text as T
import JShark.Api (stmts)
import JShark.Api.Types (ClosedEffect, Universe (Unit))
import JShark.Bun.Internal (runJS)
import JShark.Compiler (compileEffect, readableConfig)
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
      [ parseExampleCase "breakout" (stmts Breakout.mainJS)
      , parseExampleCase "todo-mvc" (stmts TodoMvc.mainJS)
      , parseExampleCase "synth" (stmts Synth.mainJS)
      , parseExampleCase "life" (stmts Life.mainJS)
      ]

parseExampleCase :: String -> ClosedEffect 'Unit -> TestTree
parseExampleCase name eff = testCase name $ do
  js <- renderExample eff
  let
    probe = "(() => { function unused() {\n" ++ js ++ "\n} return 1; })()"
  got <- T.unpack <$> runJS probe
  assertEqual (name ++ " should parse") "1" got

renderExample :: ClosedEffect 'Unit -> IO String
renderExample eff =
  T.unpack <$> compileEffect readableConfig eff
