-- | Internal module for encapsulating token sequences with balanced occurences
-- of bracket-like tokens.
--
-- This module is not intended for direct usage.
-- Its' public parts are re-exported by "Balanced".
module Balanced.Internal
  ( unBalanced,
    Balanced (UnsafeBalanced),
  )
where

import Data.CSS.Syntax.Tokens as CSS

-- | Balanced represents a list of tokens with the additional constraint
-- that bracket-like tokens are balanced. A sequence is balanced if for
-- any opening bracket there is exactly one matching closing bracket
-- and the pairs are properly nested.
-- Each 'LeftCurlyBracket' must be closed by a 'RightCurlyBracket'.
-- Each 'LeftSquareBracket' must be closed by a 'RightSquareBracket'.
-- Each 'LeftParen' must be closed by a 'RightParen'.
-- Each 'Function' must be closed by a 'RightParen'.
newtype Balanced = UnsafeBalanced [CSS.Token]
  deriving (Eq, Show)

-- | Extract the list of tokens from 'Balanced'.
unBalanced :: Balanced -> [CSS.Token]
unBalanced (UnsafeBalanced ts) = ts
