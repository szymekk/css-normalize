module CLI where

import Options.Applicative as A
import Types

data Input
  = FileInput FilePath
  | StdInput

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

pSortSelectors :: A.Parser Bool
pSortSelectors =
  switch
    ( long "sort-selectors"
        <> help "Sort selectors in a selector group"
    )

pSortProperties :: A.Parser Bool
pSortProperties =
  switch
    ( long "sort-props"
        <> help "Sort properties"
    )

pAddZeros :: A.Parser Bool
pAddZeros =
  switch
    ( long "add-zeros"
        <> help "Add leading zeros"
    )

pInput :: A.Parser Input
pInput = pFileInput <|> pStdInput

pOptions :: A.Parser StylesheetOpts
pOptions = Opts <$> pSortSelectors <*> pSortProperties <*> pAddZeros
-- defaultOpts = Opts {sortSelectors = True,
-- sortProperties = True,
-- addLeadingZeros = True}
