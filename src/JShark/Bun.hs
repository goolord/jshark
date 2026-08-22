{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Impure evaluator: compile a closed 'JShark.Types.Effect' and run it
-- with bun.
--
-- 'JShark.evaluate' walks a pure 'JShark.Types.Expr' in Haskell. Effects
-- have FFI, mutation, and I/O, so there is no host tree-walk for them; bun
-- is the runtime.
--
-- Bare bun has no @document@ or @window@, so 'JShark.Dom',
-- 'JShark.Storage', and the rest of the browser stdlib need a DOM. Pass
-- 'domBunConfig' to install one ('HappyDom').
module JShark.Bun
  ( evaluateEffectJSON
  , evaluateEffectJSONWith

    -- * Environment
  , BunConfig (..)
  , BunEnv (..)
  , HappyDomOptions (..)
  , defaultBunConfig
  , domBunConfig
  , defaultHappyDomOptions
  , domTimeoutMicroseconds
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import JShark (effectfulProgram, escapeJsString, renderJSCompact)
import JShark.Bun.Internal
  ( JSProgram (..)
  , bunTimeoutMicroseconds
  , plainProgram
  , runProgram
  )
import JShark.Types (ClosedEffect)

-- | How to run the program: the JS globals it may reach for, and how
-- long it may take.
data BunConfig = BunConfig
  { bunTimeout :: Int
  -- ^ Microseconds before the run is killed.
  , bunEnv :: BunEnv
  }
  deriving (Show, Eq, Ord)

-- | The global environment the program runs in.
data BunEnv
  = -- | Bare bun. No @document@ or @window@.
    Sandbox
  | -- | Browser globals from @happy-dom@, registered in-process.
    HappyDom HappyDomOptions
  deriving (Show, Eq, Ord)

-- | Options for the @happy-dom@ environment.
--
-- 'happyDomModule' is resolved by bun at run time (@--install=fallback@): a
-- local @node_modules@ wins, otherwise bun installs it into its global
-- cache, which needs network access on the first run.
data HappyDomOptions = HappyDomOptions
  { happyDomUrl :: Text
  -- ^ @window.location@ for the run.
  , happyDomBody :: Text
  -- ^ Markup to seed @document.body.innerHTML@ before the program runs.
  }
  deriving (Show, Eq, Ord)

defaultHappyDomOptions :: HappyDomOptions
defaultHappyDomOptions =
  HappyDomOptions {happyDomUrl = "http://localhost/", happyDomBody = ""}

-- | The npm package that supplies the browser globals. Fixed, not
-- configurable: it lands in an @import@ specifier, and with
-- @--install=fallback@ a caller-supplied string would let a config field
-- install and run arbitrary code.
happyDomModule :: Text
happyDomModule = "@happy-dom/global-registrator"

-- | Ceiling for a 'HappyDom' run. Higher than
-- 'JShark.Bun.Internal.bunTimeoutMicroseconds' because the first run may
-- install the package from npm.
domTimeoutMicroseconds :: Int
domTimeoutMicroseconds = 60 * 1000 * 1000

-- | 'Sandbox', with the default timeout.
defaultBunConfig :: BunConfig
defaultBunConfig =
  BunConfig {bunTimeout = bunTimeoutMicroseconds, bunEnv = Sandbox}

-- | 'HappyDom' with 'defaultHappyDomOptions' and 'domTimeoutMicroseconds'.
domBunConfig :: BunConfig
domBunConfig =
  BunConfig
    { bunTimeout = domTimeoutMicroseconds
    , bunEnv = HappyDom defaultHappyDomOptions
    }

-- | Compile a closed 'JShark.Types.Effect' to a self-contained IIFE and
-- run it with bun under 'defaultBunConfig'. The result is
-- @JSON.stringify@ of the program's value (@\"undefined\"@ when that value
-- is JS @undefined@).
--
-- JSON text, not a 'JShark.Types.Value': a @'JShark.Types.MutableObject'@
-- or a function has no 'JShark.Types.Value' constructor, so the universe
-- index @u@ cannot survive the round trip. That is the asymmetry with
-- 'JShark.evaluate', which returns @'JShark.Types.Value' u@.
--
-- Writes to stdout from the program itself ('JShark.Console.log') do not
-- corrupt the result, and a promise-valued program (see 'JShark.Promise',
-- 'JShark.Ajax') is awaited before it is serialized. A program that never
-- terminates is killed after 'bunTimeout'. Requires bun on @PATH@.
evaluateEffectJSON :: ClosedEffect u -> IO Text
evaluateEffectJSON = evaluateEffectJSONWith defaultBunConfig

-- | 'evaluateEffectJSON' in a chosen environment. See 'domBunConfig'.
evaluateEffectJSONWith :: BunConfig -> ClosedEffect u -> IO Text
evaluateEffectJSONWith cfg e =
  runProgram (bunTimeout cfg) (envProgram (bunEnv cfg) js)
 where
  js = renderJSCompact (effectfulProgram e)

envProgram :: BunEnv -> String -> JSProgram
envProgram Sandbox js = plainProgram js
envProgram (HappyDom opts) js =
  JSProgram
    { -- Resolve the registrator from node_modules if present, else from
      -- bun's install cache.
      jsFlags = ["--install=fallback"]
    , jsPrelude =
        unlines
          ( [ "import { GlobalRegistrator } from " ++ jsText happyDomModule ++ ";"
            , "GlobalRegistrator.register({ url: " ++ jsText (happyDomUrl opts) ++ " });"
            ]
              ++ [ "document.body.innerHTML = " ++ jsText (happyDomBody opts) ++ ";"
                 | not (T.null (happyDomBody opts))
                 ]
          )
    , jsExpression = js
    , -- happy-dom keeps the event loop alive (timers, the async window);
      -- without this the run would sit until the timeout kills it.
      jsEpilogue = "await GlobalRegistrator.unregister();"
    }

jsText :: Text -> String
jsText t = '"' : escapeJsString (T.unpack t) ++ "\""
