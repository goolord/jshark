{-# LANGUAGE OverloadedStrings #-}

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

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import JShark.Bindgen
import Options.Applicative
  ( Parser
  , ParserInfo
  , ParserPrefs
  , ParserResult (..)
  , argument
  , customExecParser
  , execParserPure
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , optional
  , prefs
  , progDesc
  , short
  , showHelpOnEmpty
  , showHelpOnError
  , str
  , strOption
  )
import System.Exit (die)

data Cli = Cli
  { cliOpts :: BindgenOpts
  , cliOut :: Maybe FilePath
  , cliFile :: FilePath
  }

parserPrefs :: ParserPrefs
parserPrefs =
  prefs (showHelpOnError <> showHelpOnEmpty)

parserInfo :: ParserInfo Cli
parserInfo =
  info (helper <*> cliParser) $
    fullDesc
      <> progDesc
        "Generate JShark FFI bindings from TypeScript / JS declarations."
      <> header
        "jshark-bindgen — generate JShark FFI bindings from TypeScript / JS"

cliParser :: Parser Cli
cliParser =
  Cli
    <$> bindgenOptsParser
    <*> optional
      ( strOption
          ( long "out"
              <> short 'o'
              <> metavar "FILE"
              <> help "Write to FILE instead of stdout"
          )
      )
    <*> argument
      str
      ( metavar "FILE"
          <> help
            "Declaration file (.d.ts / .ts) or .js with JSDoc exports"
      )

bindgenOptsParser :: Parser BindgenOpts
bindgenOptsParser =
  BindgenOpts
    <$> optional
      ( T.pack
          <$> strOption
            ( long "module"
                <> short 'm'
                <> metavar "NAME"
                <> help "Haskell module name (default JShark.FILE)"
            )
      )
    <*> optional
      ( T.pack
          <$> strOption
            ( long "prefix"
                <> short 'p'
                <> metavar "NAME"
                <> help "JS global prefix (PIXI, toy, …)"
            )
      )

parseCliArgs :: [String] -> Either String Cli
parseCliArgs args =
  case execParserPure parserPrefs parserInfo args of
    Success cli -> Right cli
    Failure err -> Left (show err)
    CompletionInvoked _ -> Left "shell completion invoked"

runMain :: IO ()
runMain =
  customExecParser parserPrefs parserInfo >>= runCli

runCli :: Cli -> IO ()
runCli cli = do
  ir <- parseIrFromFile (cliOpts cli) (cliFile cli)
  case ir of
    Left e -> die e
    Right x -> writeOut (cliOut cli) (generateFromIr x)

writeOut :: Maybe FilePath -> Text -> IO ()
writeOut Nothing t = TIO.putStr t
writeOut (Just p) t = TIO.writeFile p t
