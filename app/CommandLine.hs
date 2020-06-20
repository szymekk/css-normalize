module CommandLine
  ( Command (..),
    Input (..),
    cssnParseCommand,
    cssnPrintHelpText,
  )
where

import Options.Applicative

data Input
  = FileInput FilePath
  | StdInput

parseInputSource :: Parser Input
parseInputSource = FileInput <$> argument str (metavar "FILE") <|> pure StdInput

data Command a
  = VersionCommand
  | RunCommand a
  deriving (Eq, Show)

-- | Turn a Parser for a command of type a into a command with a version flag.
withVersionCommand :: Parser a -> Parser (Command a)
withVersionCommand commandParser =
  VersionCommand <$ versionFlag
    <|> RunCommand <$> commandParser
  where
    versionFlag :: Parser ()
    versionFlag = flag' () (long "version" <> help "Show version")

cssnInfo :: ParserInfo (Command Input)
cssnInfo =
  info
    (helper <*> parser)
    ( fullDesc
        <> header "CSS normalize - a tool for normalizing CSS files"
        <> progDesc
          "Normalize and pretty print a CSS stylesheet read from FILE. \
          \If no FILE, read standard input."
    )
  where
    parser = withVersionCommand parseInputSource

cssnPrintHelpText :: IO ()
cssnPrintHelpText = printHelpText cssnParserPrefs cssnInfo

printHelpText :: ParserPrefs -> ParserInfo a1 -> IO a2
printHelpText pPrefs pInfo =
  handleParseResult . Failure $ parserFailure pPrefs pInfo ShowHelpText mempty

cssnParserPrefs :: ParserPrefs
cssnParserPrefs = prefs showHelpOnError

cssnParseCommand :: IO (Command Input)
cssnParseCommand = customExecParser cssnParserPrefs cssnInfo
