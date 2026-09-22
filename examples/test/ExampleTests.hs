{-# LANGUAGE OverloadedStrings #-}

-- | Every example must emit JavaScript that parses: a codegen bug can emit
-- e.g. @.setAttribute(…)@ with no receiver, which only a JS engine notices.
--
-- Each case compiles its own example ('compileEffect' + 'readableConfig')
-- and parks it in a never-called function, so bun parses every line
-- without running any of it (no DOM, audio, or listeners). Cases share no
-- setup, so a slow example like Life cannot block Breakout.
module ExampleTests (exampleTests, watchMappingTests) where

import BunGate (bunPathTestName)
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import JShark.Bun.Internal (runJS)
import JShark.Compiler (compileEffect, readableConfig)
import JShark.Example.Registry (exampleLabels, exampleMainJS)
import JShark.Example.Watch
  ( exampleAppForHs
  , exampleAppsForHs
  , isLucidShellPath
  )
import Test.Tasty
import Test.Tasty.HUnit

exampleTests :: TestTree
exampleTests =
  after AllSucceed bunPathTestName . testGroup "examples emit parseable JS" $
    map parseCase exampleLabels
      ++ [ disposeCase "synth" "an audio"
         , disposeCase "life" "a renderer"
         ]
 where
  render :: Text -> IO String
  render label = BC.unpack <$> compileEffect readableConfig (exampleMainJS label)
  parseCase label = testCase (T.unpack label) $ do
    js <- render label
    got <- runJS ("(() => { function unused() {\n" ++ js ++ "\n} return 1; })()")
    assertEqual (T.unpack label ++ " should parse") "1" got
  disposeCase label what =
    testCase (T.unpack label ++ " registers " ++ what ++ " dispose hook") $ do
      js <- render label
      assertBool "dispose" ("__JSHARK_DISPOSE__" `T.isInfixOf` T.pack js)

-- | The example-specific watch-path mapping (moved out of
-- @jshark-hotreload@).
watchMappingTests :: TestTree
watchMappingTests =
  testGroup
    "example watch mapping"
    [ testCase "exampleAppForHs maps Client.hs paths" $ do
        mapM_
          (\(path, want) -> assertEqual path want (exampleAppForHs path))
          [ ("examples/src/JShark/Example/TodoMvc/Client.hs", Just "todo-mvc")
          , ("examples\\src\\JShark\\Example\\Breakout\\Types.hs", Just "breakout")
          , ("examples/src/JShark/Example/TodoMvc/Page.hs", Just "todo-mvc")
          , ("examples/app/server/DevServer.hs", Nothing)
          ]
        exampleAppsForHs "examples/src/JShark/Example/Theme.hs"
          @?= ["breakout", "todo-mvc", "synth", "life"]
        isLucidShellPath "examples/src/JShark/Example/Breakout/Page.hs" @?= True
        isLucidShellPath "examples/src/JShark/Example/Breakout/Client.hs"
          @?= False
    ]
