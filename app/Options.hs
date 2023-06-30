module Options
  ( Opts(..)
  , execParser
  , prefs
  , showHelpOnEmpty
  , optsParser
  , optsParserInfo ) where

import Options.Applicative
    ( (<**>),
      Alternative((<|>)),
      action,
      fullDesc,
      header,
      help,
      hidden,
      info,
      infoOption,
      long,
      metavar,
      prefs,
      progDesc,
      short,
      showHelpOnEmpty,
      strArgument,
      strOption,
      execParser,
      Parser,
      ParserInfo )

import Options.Applicative.Extra ( helperWith )

import Version ( versionStr, progName )
import Data.Kind (Type)

data Opts = Opts
  { optInputFile  :: FilePath
  , optOutputFile :: FilePath
  , optVersion    :: Type -> Type
  }

optsParser :: Parser Opts
optsParser
  = Opts
  <$> (inputFileOptParser <|> fileOptParser)
  <*> (outputFileOptParser <|> fileOptParser)
  <*> versionOptParse

fileOptParser :: Parser FilePath
fileOptParser
  = strArgument
  $ metavar "FILE"
  <> help "Input/output file"
  <> action "file"

inputFileOptParser :: Parser FilePath
inputFileOptParser
  = strOption
  $ long "file"
  <> short 'f'
  <> metavar "FILE"
  -- <> value ""
  <> help "Input video file"

outputFileOptParser :: Parser FilePath
outputFileOptParser
  = strOption
  $ long "output"
  <> short 'o'
  <> metavar "OUTPUTFILE"
  -- <> value ""
  <> help "Output file to write the framemd5 information."

versionOptParse :: Parser (a -> a)
versionOptParse =
  infoOption versionStr
  $ long "version"
  <> short 'V'
  <> help "Display the version number"

optsParserInfo :: ParserInfo Opts
optsParserInfo = info (optsParser <**> helper')
  $ fullDesc
  <> progDesc "Merge videos using matching md5 checksums from frames."
  <> header (progName ++ " - " ++ "Merge videos using framemd5")

helper' :: Parser (a -> a)
helper' = helperWith
          $ long "help"
          -- <> help "Show this help text"
          <> hidden  -- don't show in help messages
