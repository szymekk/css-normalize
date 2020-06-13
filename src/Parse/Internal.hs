{-# LANGUAGE OverloadedStrings #-}

-- | Parsing CSS files.
module Parse.Internal
  ( Parser,
    betweenCurly,
    betweenFunction,
    betweenSquare,
    betweenWs,
    nonBracket,
    skipWs,
    pIdent,
  )
where

import Data.CSS.Syntax.Tokens as CSS
import Data.Set as Set
import Data.Text
import Data.Void
import Text.Megaparsec
import TokenStream ()

-- | Parser type for lists of CSS token.
type Parser = Parsec Void [CSS.Token]

-- | Skip and discard zero or more whitespace tokens.
skipWs :: Parser ()
skipWs = skipMany (single Whitespace)

-- | Parse an ident-token. Returns the ident's name.
pIdent :: Parser Text
pIdent = token test Set.empty <?> "ident"
  where
    test (Ident str) = Just str
    test _ = Nothing

-- | Parse a non-bracket-like token.
nonBracket :: Parser CSS.Token
nonBracket = token test Set.empty <?> "non-bracket"
  where
    test LeftCurlyBracket = Nothing
    test RightCurlyBracket = Nothing
    test LeftSquareBracket = Nothing
    test RightSquareBracket = Nothing
    test LeftParen = Nothing
    test RightParen = Nothing
    test (Function _) = Nothing
    test t = Just t

-- | @betweenWs p@ parses any number of whitespace tokens, followed by @p@ and
-- any number of whitespace tokens. Returns the value returned by @p@.
-- > betweenWs = between skipWs skipWs
betweenWs :: Parser a -> Parser a
betweenWs = between skipWs skipWs

-- | @betweenSquare p@ parses a 'LeftSquareBracket', followed by @p@ and
-- 'RightSquareBracket'. Returns the value returned by @p@.
betweenSquare :: Parser a -> Parser a
betweenSquare = between (single LeftSquareBracket) (single RightSquareBracket)

-- | @betweenCurly p@ parses a 'LeftCurlyBracket', followed by @p@ and
-- 'RightCurlyBracket'. Returns the value returned by @p@.
betweenCurly :: Parser a -> Parser a
betweenCurly = between (single LeftCurlyBracket) (single RightCurlyBracket)

-- | @betweenFunction name p@ parses a function-token named @name@, followed
-- by @p@ and 'RightParen'. Returns the value returned by @p@.
betweenFunction :: Text -> Parser a -> Parser a
betweenFunction name = between (single (Function name)) (single RightParen)
