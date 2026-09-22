-- | Shared bun-gating scaffold for test groups that shell out to bun.
module BunGate
  ( bunGated
  , bunGroup
  , bunPathTestName
  )
where

import System.Directory (findExecutable)
import Test.Tasty
import Test.Tasty.HUnit (assertFailure, testCase)

-- | Name of the probe case asserting bun is on PATH. Cases that need bun
-- defer with @'after' 'AllSucceed' bunPathTestName@ so they skip cleanly
-- when the probe fails.
bunPathTestName :: TestName
bunPathTestName = "bun is on PATH"

-- | Wrap a test group in the shared bun-on-PATH resource. The group
-- receives a lookup yielding bun's path, or 'Nothing' when bun is absent.
bunGated :: (IO (Maybe FilePath) -> TestTree) -> TestTree
bunGated = withResource (findExecutable "bun") (const (pure ()))

-- | The bun probe case, then @ts@ deferred until it passes.
bunGroup :: TestName -> [TestTree] -> TestTree
bunGroup name ts = bunGated $ \getBun ->
  testGroup name $
    testCase bunPathTestName (getBun >>= maybe missing (const (pure ())))
      : map (after AllSucceed bunPathTestName) ts
 where
  missing = assertFailure "bun not found on PATH; install https://bun.sh"
