-- | Internal module created to avoid circular dependencies.
module Parser
  ( Parser,
  )
where

import Data.CSS.Syntax.Tokens as CSS
import Data.Void
import Text.Megaparsec

-- | Parser type for lists of CSS token.
type Parser = Parsec Void [CSS.Token]
