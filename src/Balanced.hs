module Balanced
  ( mkEmptyBalanced,
    unBalanced,
    Balanced,
    someBalanced,
  )
where

import Control.Monad
import Data.CSS.Syntax.Tokens as CSS
import Data.Set as Set
import Text.Megaparsec
import TokenStream ()
import Types (BracketType (..), Parser)

-- | Balanced represents a list of tokens with the additional constraint
-- that bracket-like tokens are balanced. A sequence is balanced if for
-- any opening bracket there is exactly one matching closing bracket
-- and the pairs are properly nested.
-- Each 'LeftCurlyBracket' must be closed by a 'RightCurlyBracket'.
-- Each 'LeftSquareBracket' must be closed by a 'RightSquareBracket'.
-- Each 'LeftParen' must be closed by a 'RightParen'.
-- Each 'Function' must be closed by a 'RightParen'.
newtype Balanced = UnsafeBalanced [CSS.Token]
  deriving (Show)

unBalanced :: Balanced -> [CSS.Token]
unBalanced (UnsafeBalanced ts) = ts

mkEmptyBalanced :: Balanced
mkEmptyBalanced = UnsafeBalanced []

-- | Parse a matching pair of opening and closing bracket-like tokens.
parseMatchingPair :: Parser Balanced
parseMatchingPair =
  UnsafeBalanced
    <$> choice
      [ try $ parsePair (single LeftCurlyBracket) (single RightCurlyBracket),
        try $ parsePair (single LeftSquareBracket) (single RightSquareBracket),
        try $ parsePair (single LeftParen) (single RightParen),
        parsePair pFunction (single RightParen)
      ]
  where
    parsePair :: Parser CSS.Token -> Parser CSS.Token -> Parser [CSS.Token]
    parsePair pOpen pClose = do
      open <- pOpen
      close <- pClose
      return [open, close]

-- | Parse a nonempty sequence of balanced tokens.
someBalanced :: Parser Balanced
someBalanced =
  concatBalanced
    <$> some
      ( try parseMatchingPair
          <|> try someNonBracket
          <|> parseEnclosed
      )

-- | Parse a sequence of non-bracket tokens.
-- Such a list is trivially balanced.
someNonBracket :: Parser Balanced
someNonBracket = UnsafeBalanced <$> some nonBracket

-- | Parse a matching pair of bracket-like tokens
-- enclosing a nonempty sequence of balanced tokens.
parseEnclosed :: Parser Balanced
parseEnclosed = do
  (bracketType, ts) <- parseEnclosedWithBracketType
  let (opening, closing) = getBracketPair bracketType
  return $ UnsafeBalanced $ opening : unBalanced ts ++ [closing]

getBracketPair :: BracketType -> (CSS.Token, CSS.Token)
getBracketPair (FunctionToken name) = (Function name, RightParen)
getBracketPair Round = (LeftParen, RightParen)
getBracketPair Curly = (LeftCurlyBracket, RightCurlyBracket)
getBracketPair Square = (LeftSquareBracket, RightSquareBracket)

parseEnclosedWithBracketType :: Parser (BracketType, Balanced)
parseEnclosedWithBracketType = do
  bracketType <- parseBracketOpen
  innerTokens <- someBalanced
  let closingBracket = snd (getBracketPair bracketType)
  void $ single closingBracket
  return (bracketType, innerTokens)

concatBalanced :: [Balanced] -> Balanced
concatBalanced bs = UnsafeBalanced $ concatMap unBalanced bs

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

parseBracketOpen :: Parser BracketType
parseBracketOpen = token test Set.empty <?> "bracket opener"
  where
    test :: CSS.Token -> Maybe BracketType
    test LeftCurlyBracket = Just Curly
    test LeftSquareBracket = Just Square
    test LeftParen = Just Round
    test (Function name) = Just (FunctionToken name)
    test _ = Nothing

pFunction :: Parser CSS.Token
pFunction = token test Set.empty <?> "function"
  where
    test (Function name) = Just (Function name)
    test _ = Nothing
