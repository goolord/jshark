-- | Shared bun-gating scaffold for test groups that shell out to bun.
module BunGate
  ( bunGated
  , bunPathTestName
  )
where

import System.Directory (findExecutable)
import Test.Tasty (TestName, TestTree, withResource)

-- | Name of the probe case asserting bun is on PATH. Test groups that
-- need bun defer their bun-dependent cases with
-- @'after' 'AllSucceed' bunPathTestName@ so they skip cleanly when the
-- probe fails.
bunPathTestName :: TestName
bunPathTestName = "bun is on PATH"

-- | Wrap a test group in the shared bun-on-PATH resource. The group
-- receives a lookup yielding bun's path, or 'Nothing' when bun is
-- absent.
bunGated :: (IO (Maybe FilePath) -> TestTree) -> TestTree
bunGated = withResource (findExecutable "bun") (const (pure ()))
