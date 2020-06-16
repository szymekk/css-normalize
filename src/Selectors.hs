{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parsing CSS selectors.
module Selectors
  ( parseSelectorsGroup,
    parseSelector,
    parseSimpleSelectorSeq,
  )
where

import Balanced
import Control.Applicative hiding (many, some)
import qualified Control.Monad.Combinators.NonEmpty as NE
import Data.CSS.Syntax.Tokens as CSS
import Data.Functor
import Data.Set as Set
import Data.Text
import Parse.Internal
import Selectors.Types
import Text.Megaparsec

-- | Parse a group of comma separated selectors.
parseSelectorsGroup :: Parser SelectorsGroup
parseSelectorsGroup =
  fmap SelectorsGroup $
    (parseSelector <* skipWs) `NE.sepBy1` (single Comma <* skipWs)

-- | Parse a CSS selector.
parseSelector :: Parser Selector
parseSelector = do
  first <- parseSimpleSelectorSeq
  rest <- many . try $ (,) <$> parseCombinator <*> parseSimpleSelectorSeq
  return $ Selector first rest

-- | Parse a selector combinator.
parseCombinator :: Parser Combinator
parseCombinator =
  choice
    [ try (betweenWs (single (Delim '+'))) $> Plus,
      try (betweenWs (single (Delim '>'))) $> Greater,
      try (betweenWs (single (Delim '~'))) $> Tilde,
      some (single Whitespace) $> Space
    ]
    <* skipWs

-- | Parse a sequence of simple selectors.
parseSimpleSelectorSeq :: Parser SimpleSelectorSeq
parseSimpleSelectorSeq =
  optional parseTypeLikeSelector >>= \case
    Just ts -> SimpleSelectorSeq ts <$> many parseSimpleSelector
    Nothing -> SimpleSelectorSeq Universal <$> some parseSimpleSelector

-- | Parse a type selector.
parseTypeLikeSelector :: Parser TypeLikeSelector
parseTypeLikeSelector = Universal <$ single (Delim '*') <|> TypeSelector <$> pIdent

-- | Parse a simple selector.
parseSimpleSelector :: Parser SimpleSelector
parseSimpleSelector =
  try (Not <$> parseNegation)
    <|> CommonSimpleSelector <$> parseCommon

-- | Parse a negation selector.
parseNegation :: Parser Negation
parseNegation = single Colon *> functionParens "not" (betweenWs parseNegationArg)
  where
    parseNegationArg =
      NegationTypeLike <$> parseTypeLikeSelector
        <|> NegationCommon <$> parseCommon

-- | Parse several types of simple selectors.
parseCommon :: Parser Common
parseCommon =
  choice
    [ CommonClass <$> parseClassSelector,
      CommonAttribute <$> parseAttributeSelector,
      CommonId <$> parseIdSelector,
      CommonPseudoElement <$> parsePseudoElement <?> "pseudo-element selector",
      CommonPseudoClass <$> parsePseudoClass <?> "pseudo-class selector"
    ]

-- | Parse an ID selector.
parseIdSelector :: Parser Id
parseIdSelector = Id <$> pHash

-- | Parse an hash-token. Return the token's text.
pHash :: Parser Text
pHash = token test Set.empty <?> "hash"
  where
    test (Hash _ text) = Just text
    test _ = Nothing

-- | Parse a class selector.
parseClassSelector :: Parser Class
parseClassSelector = single (Delim '.') *> (Class <$> pIdent)

-- | Parse an attribute selector.
parseAttributeSelector :: Parser Attribute
parseAttributeSelector =
  squareBrackets
    ( do
        attributeName <- betweenWs (AttrName <$> pIdent) <?> "attribute name"
        optValuePredicate <- optional parseValuePredicate
        return (Attribute attributeName optValuePredicate)
    )
  where
    parseValuePredicate = AttrValueMatch <$> parseValueMatchType <*> betweenWs parseValue
    parseValueMatchType =
      choice
        [ single PrefixMatch $> Prefix,
          single SuffixMatch $> Suffix,
          single SubstringMatch $> Substring,
          single (Delim '=') $> Equals,
          single IncludeMatch $> Include,
          single DashMatch $> Dash
        ]
    parseValue = AttrVal <$> (pString <|> pIdent)

-- | Parse a pseudo-element.
parsePseudoElement :: Parser PseudoElement
parsePseudoElement = do
    try (void $ single Colon *> single Colon)
    PseudoElement <$> parsePseudoBody

-- | Parse a pseudo-class.
parsePseudoClass :: Parser PseudoClass
parsePseudoClass = single Colon *> (PseudoClass <$> parsePseudoBody)

-- | Parse the body of a pseudo selector.
parsePseudoBody :: Parser PseudoBody
parsePseudoBody =
  IdentPseudo <$> pIdent
    <|> FunctionalPseudo <$> pFunctionName <*> parseExpression

-- | Parse a function token. Returns the name of the function.
pFunctionName :: Parser Text
pFunctionName = token test Set.empty <?> "function"
  where
    test (Function name) = Just name
    test _ = Nothing

-- | Parse an expression valid inside either a functional pseudo-class or
-- pseudo-element selector.
parseExpression :: Parser Balanced
parseExpression = skipWs *> manyBalancedTill (try (skipWs <* single RightParen))
