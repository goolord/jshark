{-# LANGUAGE OverloadedStrings #-}

-- | Haskell call-site names for readable JS binder hints ('esSourceNames').
module JShark.Api.Caller
  ( callerBinderHint
  )
where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack)

-- | Enclosing user function for the current call site, when known.
--
-- Skips JShark API frames and a few test/wrapper helpers so hints name the
-- program (e.g. @mainJS@) rather than plumbing (@with1@, @fromSyntax@).
callerBinderHint :: HasCallStack => Maybe Text
callerBinderHint =
  listToMaybe
    [ T.pack name
    | (name, loc) <- getCallStack callStack
    , isUserFunction name loc
    ]
{-# NOINLINE callerBinderHint #-}

isUserFunction :: String -> SrcLoc -> Bool
isUserFunction name loc =
  not (null name)
    && not ("$" `isPrefixOf` name)
    && name `notElem` skippedFunctions
    && not (any (`isPrefixOf` srcLocModule loc) skippedModules)

skippedFunctions :: [String]
skippedFunctions =
  [ "callerBinderHint"
  , "bindEffectSyntax"
  , ">>="
  , ">>"
  , "*>"
  , "let_"
  , "lambda"
  , "lambdaE"
  , "loop0"
  , "fromSyntax"
  , "toSyntax"
  , "toSyntax_"
  , "bindExpr"
  , "with1"
  , "with2"
  ]

skippedModules :: [String]
skippedModules =
  [ "JShark.Api"
  , "JShark.Compiler"
  ]

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys
