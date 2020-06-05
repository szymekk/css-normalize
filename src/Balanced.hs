{-# LANGUAGE InstanceSigs #-}

module Balanced
  ( unBalanced,
    Balanced,
    someBalanced,
    manyBalancedTill,
  )
where

import Balanced.Internal
import Control.Monad
import Data.CSS.Syntax.Tokens as CSS
import Data.Set as Set
import Data.Text
import Parser
import Text.Megaparsec
import TokenStream ()

instance Semigroup Balanced where
  b1 <> b2 = UnsafeBalanced $ unBalanced b1 <> unBalanced b2

instance Monoid Balanced where
  mempty :: Balanced
  mempty = UnsafeBalanced []

data BracketType = FunctionToken Text | Round | Curly | Square
  deriving (Eq, Show)

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
  mconcat
    <$> some
      ( try parseMatchingPair
          <|> try someNonBracket
          <|> parseEnclosed
      )

-- | @manyBalancedTill end@ repeatedly parses /zero/ or more balanced atoms
-- until parser @end@ succeeds.
-- A balanced atom is either:
--
-- - a non-bracket token
-- - or an opening token (e.g. 'LeftCurlyBracket'), followed by a sequence
-- of tokens comprising /zero/ or more balanced atoms, followed by a matching
-- closing token (e.g. 'RightCurlyBracket').
--
-- Returns a nonempty list of balanced tokens.
manyBalancedTill :: Parser end -> Parser Balanced
manyBalancedTill end = (mempty <$ end) <|> someBalancedTill end

-- | @someBalancedTill end@ works similarly to @manyBalancedTill end@ but
-- at least one balanced atom should be parsed before @end@ succeeds.
someBalancedTill :: Parser end -> Parser Balanced
someBalancedTill end =
  mconcat
    <$> someTill balancedAtom end
  where
    balancedAtom :: Parser Balanced
    balancedAtom =
      try parseMatchingPair
        <|> try (fmap (UnsafeBalanced . pure) nonBracket)
        <|> parseEnclosed

-- | Parse a sequence of non-bracket tokens.
-- Such a list is trivially balanced.
someNonBracket :: Parser Balanced
someNonBracket = UnsafeBalanced <$> some nonBracket

-- | Parse a matching pair of bracket-like tokens
-- enclosing a nonempty sequence of balanced tokens.
-- Returns all consumed tokens including the surrounding bracket-like tokens.
parseEnclosed :: Parser Balanced
parseEnclosed = do
  (bracketType, ts) <- parseEnclosedWithBracketType
  let (opening, closing) = getBracketPair bracketType
  return $ UnsafeBalanced $ opening : unBalanced ts ++ [closing]

-- | Get a matching pair of an opening and closing tokens corresponding
-- to the specified type of bracket-like tokens.
getBracketPair :: BracketType -> (CSS.Token, CSS.Token)
getBracketPair (FunctionToken name) = (Function name, RightParen)
getBracketPair Round = (LeftParen, RightParen)
getBracketPair Curly = (LeftCurlyBracket, RightCurlyBracket)
getBracketPair Square = (LeftSquareBracket, RightSquareBracket)

-- | Parse a matching pair of bracket-like tokens
-- enclosing a nonempty sequence of balanced tokens.
-- Returns the type of surrounding bracket-like tokens and the enclosed tokens.
parseEnclosedWithBracketType :: Parser (BracketType, Balanced)
parseEnclosedWithBracketType = do
  bracketType <- parseBracketOpen
  innerTokens <- someBalanced
  let closingBracket = snd (getBracketPair bracketType)
  void $ single closingBracket
  return (bracketType, innerTokens)

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

-- | Parse an opening bracket-like token.
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
