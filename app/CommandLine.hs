module CommandLine
  ( Input (..),
    Command (..),
    pCommand,
  )
where

import Options.Applicative

data Input
  = FileInput FilePath
  | StdInput

pInput :: Parser Input
pInput = FileInput <$> argument str (metavar "FILE") <|> pure StdInput

data Command a
  = VersionCommand
  | RunCommand a
  deriving (Eq, Show)

-- | Turn a Parser for a command of type a into a command with a version flag.
mkCommand :: Parser a -> Parser (Command a)
mkCommand commandParser =
  VersionCommand <$ versionFlag
    <|> RunCommand <$> commandParser

versionFlag :: Parser ()
versionFlag = flag' () (long "version" <> help "Show version")

pCommand :: Parser (Command Input)
pCommand = mkCommand pInput
