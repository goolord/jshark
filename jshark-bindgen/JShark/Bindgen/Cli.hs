{-# LANGUAGE OverloadedStrings #-}

-- | Command-line interface for @jshark-bindgen@.
module JShark.Bindgen.Cli
  ( Mode (..)
  , Cli (..)
  , parserInfo
  , parserPrefs
  , parseCliArgs
  , runCli
  , runMain
  )
where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import JShark.Bindgen
import JShark.Bindgen.Json (encodeModule)
import Options.Applicative
  ( Parser
  , ParserInfo
  , ParserPrefs
  , ParserResult (..)
  , argument
  , customExecParser
  , execParserPure
  , flag'
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
  , switch
  )
import System.Exit (die)

data Mode
  = Haskell
  | JsonOut
  deriving (Eq, Show)

data Cli = Cli
  { cliOpts :: BindgenOpts
  , cliOut :: Maybe FilePath
  , cliMode :: Mode
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
    <*> modeParser
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
    <*> switch
      ( long "no-ts"
          <> help "Skip the TypeScript extractor; parse in Haskell"
      )

modeParser :: Parser Mode
modeParser =
  flag'
    JsonOut
    ( long "json"
        <> help "Emit jshark-bindgen IR JSON instead of Haskell"
    )
    <|> pure Haskell

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
