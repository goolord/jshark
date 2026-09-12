{-# LANGUAGE OverloadedStrings #-}

-- | Example-specific file-watch mapping: @.hs@ paths to app names and the
-- @/static@ CSS URL layout. Lives here rather than in @jshark-hotreload@ so
-- the hot-reload library carries no example knowledge.
module JShark.Example.Watch
  ( exampleWatchTargets
  , exampleCssUrl
  , exampleAppForHs
  , exampleAppsForHs
  , isLucidShellPath
  )
where

import Data.List (isInfixOf)
import qualified Data.Text as T
import JShark.HotReload.Watcher (WatchTargets (..))
import System.FilePath (takeExtension, takeFileName)

-- | 'WatchTargets' over the given roots using the examples' CSS layout.
exampleWatchTargets :: [FilePath] -> WatchTargets
exampleWatchTargets dirs =
  WatchTargets
    { watchDirs = dirs
    , cssUrlFor = exampleCssUrl
    , onHaskellSource = \_ -> pure ()
    }

-- | Map a changed @.css@ path to its @/static/<file>@ browser URL.
exampleCssUrl :: FilePath -> Maybe T.Text
exampleCssUrl path
  | takeExtension path /= ".css" = Nothing
  | otherwise =
      Just (T.pack ("/static/" <> map slash (takeFileName path)))
 where
  slash c = if c == '\\' then '/' else c

-- | Map a changed @.hs@ path under @examples/@ to an example app name.
exampleAppForHs :: FilePath -> Maybe T.Text
exampleAppForHs path =
  case exampleAppsForHs path of
    [one] -> Just one
    _ -> Nothing

-- | Like 'exampleAppForHs', but @Theme@ maps to every example.
exampleAppsForHs :: FilePath -> [T.Text]
exampleAppsForHs path
  | takeExtension path /= ".hs" = []
  | serverOrCompile path = []
  | isThemeHead path =
      ["breakout", "todo-mvc", "synth", "life"]
  | otherwise =
      case matchDir path of
        Just app -> [app]
        Nothing -> []
 where
  serverOrCompile p =
    any
      (`isInfixOf` p)
      [ "examples\\app\\server"
      , "examples/app/server"
      , "examples\\app\\compile"
      , "examples/app/compile"
      , "examples\\app\\wasm"
      , "examples/app/wasm"
      ]
  isThemeHead p = "Theme" `isInfixOf` p
  matchDir p
    | "TodoMvc" `isInfixOf` p = Just "todo-mvc"
    | "Breakout" `isInfixOf` p = Just "breakout"
    | "Synth" `isInfixOf` p = Just "synth"
    | "Life" `isInfixOf` p = Just "life"
    | otherwise = Nothing

-- | Lucid shell / shared head — prefer full page reload after rebuild.
isLucidShellPath :: FilePath -> Bool
isLucidShellPath path =
  takeFileName path == "Page.hs"
    || "Theme" `isInfixOf` path
    || takeExtension path == ".html"
