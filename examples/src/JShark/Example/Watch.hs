{-# LANGUAGE OverloadedStrings #-}

-- | Example-specific file-watch mapping: @.hs@ paths to app names and the
-- @/static@ CSS URL layout. Lives here rather than in @jshark-hotreload@ so
-- the hot-reload library carries no example knowledge.
module JShark.Example.Watch
  ( exampleWatchTargets
  , exampleAppForHs
  , exampleAppsForHs
  , isLucidShellPath
  )
where

import Data.List (isInfixOf)
import qualified Data.Text as T
import JShark.HotReload.Watcher (WatchTargets (..))
import System.FilePath (takeExtension, takeFileName)

-- | 'WatchTargets' over the given roots, mapping a changed @.css@ file to
-- its @/static/<file>@ browser URL.
exampleWatchTargets :: [FilePath] -> WatchTargets
exampleWatchTargets dirs = WatchTargets dirs cssUrl (\_ -> pure ())
 where
  cssUrl path
    | takeExtension path /= ".css" = Nothing
    | otherwise = Just (T.pack ("/static/" <> unixSlashes (takeFileName path)))

unixSlashes :: FilePath -> FilePath
unixSlashes = map (\c -> if c == '\\' then '/' else c)

-- | Map a changed @.hs@ path under @examples/@ to an example app name.
exampleAppForHs :: FilePath -> Maybe T.Text
exampleAppForHs path = case exampleAppsForHs path of
  [one] -> Just one
  _ -> Nothing

-- | Like 'exampleAppForHs', but @Theme@ maps to every example.
exampleAppsForHs :: FilePath -> [T.Text]
exampleAppsForHs path
  | takeExtension path /= ".hs" || any (`isInfixOf` unixSlashes path) tools = []
  | "Theme" `isInfixOf` path = ["breakout", "todo-mvc", "synth", "life"]
  | otherwise = take 1 [app | (dir, app) <- dirs, dir `isInfixOf` path]
 where
  tools = ["examples/app/server", "examples/app/compile", "examples/app/wasm"]
  dirs =
    [ ("TodoMvc", "todo-mvc")
    , ("Breakout", "breakout")
    , ("Synth", "synth")
    , ("Life", "life")
    ]

-- | Lucid shell / shared head — prefer full page reload after rebuild.
isLucidShellPath :: FilePath -> Bool
isLucidShellPath path =
  takeFileName path == "Page.hs"
    || "Theme" `isInfixOf` path
    || takeExtension path == ".html"
