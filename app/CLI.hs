module CLI
where

import Options.Applicative as A

data Input
  = FileInput FilePath
  | StdInput

data Command = Command Input ()

pFileInput :: A.Parser Input
pFileInput =
  FileInput
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILENAME"
          <> help "Input file"
      )

pStdInput :: A.Parser Input
pStdInput =
  flag'
    StdInput
    ( long "stdin"
        <> help "Read from stdin"
    )

pInput :: A.Parser Input
pInput = pFileInput <|> pStdInput
