{-# LANGUAGE LambdaCase #-}

-- | Command-line interface for @jshark-bindgen@.
module JShark.Bindgen.Cli
  ( Cli (..)
  , parserInfo
  , parserPrefs
  , parseCliArgs
  , runCli
  , runMain
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import JShark.Bindgen
import JShark.Bindgen.Ir (Diagnostic (..), irDiagnostics)
import Options.Applicative
import System.Exit (die)
import System.IO (hPutStrLn, stderr)

-- | Parsed arguments: generation options, optional output path, input file.
data Cli = Cli
  { cliOpts :: BindgenOpts
  , cliOut :: Maybe FilePath
  , cliFile :: FilePath
  }

-- | optparse-applicative preferences: show help on error and on empty input.
parserPrefs :: ParserPrefs
parserPrefs = prefs (showHelpOnError <> showHelpOnEmpty)

-- | Full parser description for the @jshark-bindgen@ command.
parserInfo :: ParserInfo Cli
parserInfo =
  info (helper <*> cli) $
    fullDesc
      <> progDesc "Generate JShark FFI bindings from TypeScript / JS declarations."
      <> header "jshark-bindgen — generate JShark FFI bindings from TypeScript / JS"
 where
  cli =
    Cli
      <$> ( BindgenOpts
              <$> opt "module" 'm' "NAME" "Haskell module name (default JShark.FILE)"
              <*> opt "prefix" 'p' "NAME" "JS global prefix (PIXI, toy, …)"
          )
      <*> opt "out" 'o' "FILE" "Write to FILE instead of stdout"
      <*> argument
        str
        ( metavar "FILE"
            <> help "Declaration file (.d.ts / .ts) or .js with JSDoc exports"
        )
  opt l s v h = optional (strOption (long l <> short s <> metavar v <> help h))

-- | Parse an argument vector into 'Cli', reporting failures as 'Left'.
parseCliArgs :: [String] -> Either String Cli
parseCliArgs args = case execParserPure parserPrefs parserInfo args of
  Success cli -> Right cli
  Failure err -> Left (show err)
  CompletionInvoked _ -> Left "shell completion invoked"

-- | Parse @argv@ and run the CLI; the executable entry point.
runMain :: IO ()
runMain = customExecParser parserPrefs parserInfo >>= runCli

-- | Run the generator for a parsed 'Cli', writing to file or stdout.
runCli :: Cli -> IO ()
runCli cli =
  parseIrFromFile (cliOpts cli) (cliFile cli) >>= \case
    Left e -> die e
    Right ir -> do
      mapM_ (hPutStrLn stderr . diagnostic) (irDiagnostics ir)
      maybe TIO.putStr TIO.writeFile (cliOut cli) (generateFromIr ir)
 where
  diagnostic d =
    "jshark-bindgen: [" <> T.unpack (dgCategory d) <> "] " <> T.unpack (dgMessage d)
