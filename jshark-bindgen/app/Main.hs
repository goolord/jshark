{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @jshark-bindgen@ — TypeScript / JavaScript → JShark FFI wrappers.
module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import JShark.Bindgen
import JShark.Bindgen.Json (encodeModule)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Mode
  = Haskell
  | JsonOut

data Cli = Cli
  { cliOpts :: BindgenOpts
  , cliOut :: Maybe FilePath
  , cliMode :: Mode
  , cliFile :: FilePath
  }

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left Help -> TIO.putStrLn usage
    Left (Bad msg) -> do
      hPutStrLn stderr msg
      TIO.hPutStrLn stderr usage
      exitFailure
    Right cli -> run cli

data Parse = Help | Bad String

run :: Cli -> IO ()
run cli = do
  let
    opts = cliOpts cli
  ir <- parseIrFromFile opts (cliFile cli)
  case ir of
    Left e -> die e
    Right x ->
      case cliMode cli of
        JsonOut -> writeOut (cliOut cli) (encodeModule x <> "\n")
        Haskell -> writeOut (cliOut cli) (generateFromIr x)

writeOut :: Maybe FilePath -> Text -> IO ()
writeOut Nothing t = TIO.putStr t
writeOut (Just p) t = TIO.writeFile p t

die :: String -> IO a
die e = hPutStrLn stderr e >> exitFailure

usage :: Text
usage =
  T.unlines
    [ "jshark-bindgen — generate JShark FFI bindings from TypeScript / JS"
    , ""
    , "Usage:"
    , "  jshark-bindgen [options] FILE"
    , ""
    , "FILE is a .d.ts / .ts declaration file, or .js with JSDoc."
    , "Generated wrappers call global names (use --prefix for UMD /"
    , "namespace libraries such as PIXI)."
    , ""
    , "Options:"
    , "  -m, --module NAME   Haskell module name (default JShark.FILE)"
    , "  -p, --prefix NAME   JS global prefix (PIXI, toy, …)"
    , "  -o, --out FILE      write to FILE instead of stdout"
    , "      --json          emit jshark-bindgen IR JSON instead of Haskell"
    , "      --no-ts         skip the TypeScript extractor; parse in Haskell"
    , "  -h, --help          show this help"
    ]

parseArgs :: [String] -> Either Parse Cli
parseArgs = go defaultBindgenOpts Nothing Haskell Nothing
 where
  go opts out mode file = \case
    [] -> case file of
      Nothing -> Left (Bad "missing FILE")
      Just f ->
        Right
          Cli
            { cliOpts = opts
            , cliOut = out
            , cliMode = mode
            , cliFile = f
            }
    ("-h" : _) -> Left Help
    ("--help" : _) -> Left Help
    ("--json" : xs) -> go opts out JsonOut file xs
    ("--no-ts" : xs) -> go opts {optNoTs = True} out mode file xs
    ("-m" : n : xs) ->
      go opts {optModuleName = Just (T.pack n)} out mode file xs
    ("--module" : n : xs) ->
      go opts {optModuleName = Just (T.pack n)} out mode file xs
    ("-p" : n : xs) ->
      go opts {optPrefix = Just (T.pack n)} out mode file xs
    ("--prefix" : n : xs) ->
      go opts {optPrefix = Just (T.pack n)} out mode file xs
    ("-o" : n : xs) -> go opts (Just n) mode file xs
    ("--out" : n : xs) -> go opts (Just n) mode file xs
    (x : _)
      | take 1 x == "-" -> Left (Bad ("unknown flag: " <> x))
    (x : xs) -> case file of
      Nothing -> go opts out mode (Just x) xs
      Just _ -> Left (Bad ("extra argument: " <> x))
