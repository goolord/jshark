{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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
module ExampleTests (exampleTests, watchMappingTests) where

import BunGate (bunPathTestName)
import qualified Data.Text as T
import JShark.Api (stmts)
import JShark.Api.Types (ClosedEffect, Universe (Unit))
import JShark.Bun.Internal (runJS)
import JShark.Compiler (compileEffect, readableConfig)
import qualified JShark.Example.Breakout as Breakout
import qualified JShark.Example.Life as Life
import qualified JShark.Example.Synth as Synth
import qualified JShark.Example.TodoMvc as TodoMvc
import JShark.Example.Watch
  ( exampleAppForHs
  , exampleAppsForHs
  , isLucidShellPath
  )
import Test.Tasty
import Test.Tasty.HUnit

exampleTests :: TestTree
exampleTests =
  after AllSucceed bunPathTestName $
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

-- | The example-specific watch-path mapping moved out of
-- @jshark-hotreload@; it lives here now.
watchMappingTests :: TestTree
watchMappingTests =
  testGroup
    "example watch mapping"
    [ testCase "exampleAppForHs maps Client.hs paths" $ do
        assertEqual
          "todo"
          (Just "todo-mvc")
          (exampleAppForHs "examples/src/JShark/Example/TodoMvc/Client.hs")
        assertEqual
          "breakout"
          (Just "breakout")
          (exampleAppForHs "examples\\src\\JShark\\Example\\Breakout\\Types.hs")
        assertEqual
          "page maps"
          (Just "todo-mvc")
          (exampleAppForHs "examples/src/JShark/Example/TodoMvc/Page.hs")
        assertEqual
          "server skip"
          Nothing
          (exampleAppForHs "examples/app/server/DevServer.hs")
        assertEqual
          "theme all"
          ["breakout", "todo-mvc", "synth", "life"]
          (exampleAppsForHs "examples/src/JShark/Example/Theme.hs")
        assertBool
          "lucid shell"
          (isLucidShellPath "examples/src/JShark/Example/Breakout/Page.hs")
        assertBool
          "not lucid"
          (not (isLucidShellPath "examples/src/JShark/Example/Breakout/Client.hs"))
    ]
