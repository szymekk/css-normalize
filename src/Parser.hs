module Parser
  ( Parser,
  )
where

import Data.CSS.Syntax.Tokens as CSS
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void [CSS.Token]
