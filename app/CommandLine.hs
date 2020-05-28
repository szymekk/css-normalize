module CommandLine
  ( Input (..),
    pInput,
  )
where

import Options.Applicative

data Input
  = FileInput FilePath
  | StdInput

pInput :: Parser Input
pInput = FileInput <$> argument str (metavar "FILE") <|> pure StdInput
