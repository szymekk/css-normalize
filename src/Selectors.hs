{-# LANGUAGE OverloadedStrings #-}

-- | Type definitions for CSS selectors.
module Selectors
  ( parseSelectorsGroup,
  )
where

import Balanced
import qualified Control.Monad.Combinators.NonEmpty as NE
import Data.CSS.Syntax.Tokens as CSS
import Data.Functor
import Data.Set as Set
import Data.Text
import Parse (pIdent, skipWs)
import Parser
import Selectors.Types
import Text.Megaparsec

parseSelectorsGroup :: Parser SelectorsGroup
parseSelectorsGroup = fmap SelectorsGroup $ parseSelector `NE.sepBy1` single Comma

-- parseSelector = parseSimpleSelectorSeq `sepBy1` parseCombinator
parseSelector :: Parser Selector
parseSelector = do
  first <- parseSimpleSelectorSeq
  rest <- many $ (,) <$> parseCombinator <*> parseSimpleSelectorSeq
  return $ Selector (first, rest)

parseCombinator :: Parser Combinator
parseCombinator =
  choice
    [ single (Delim '+') $> Plus,
      single (Delim '>') $> Greater,
      single (Delim '~') $> Tilde,
      single Whitespace $> Space
    ]

parseSimpleSelectorSeq :: Parser SimpleSelectorSeq
parseSimpleSelectorSeq =
  fmap WithTypeSelector ((,) <$> pTypeSelector <*> many parseSimpleSelector)
    <|> OnlySimpleSelectors <$> NE.some parseSimpleSelector

pCommon :: Parser Common
pCommon =
  choice
    [ CommonClass <$> pClass,
      CommonAttrib <$> parseAttribute,
      CommonId <$> pId,
      CommonPseudo <$> pPseudo
    ]

parseSimpleSelector :: Parser SimpleSelector
parseSimpleSelector =
  CommonSelector <$> pCommon
    <|> NegationSelector <$> pNegation

pNegation :: Parser Negation
pNegation = single Colon *> ((pNot *> skipWs) *> parseNegationArg <* (skipWs <* single RightParen))
  where
    pNot = single (Function "not")
    parseNegationArg =
      NegationTypeSelector <$> pTypeSelector
        <|> NegationCommon <$> pCommon

pTypeSelector :: Parser TypeSelector
pTypeSelector = Universal <$ single (Delim '*') <|> TypeSelector <$> pIdent

pId :: Parser IdSelector
pId = IdSelector <$> pHash

pHash :: Parser Text
pHash = token test Set.empty <?> "hash"
  where
    test (Hash _ text) = Just text
    test _ = Nothing

pClass :: Parser Class
pClass = single (Delim '.') *> (Class <$> pIdent)

pPseudo :: Parser Pseudo
pPseudo = do
  void (single Colon)
  maybeColon <- optional (void $ single Colon)
  case maybeColon of
    Nothing -> PClass . PseudoClass <$> pPseudoBody
    Just _ -> PElement . PseudoElement <$> pPseudoBody

pPseudoBody :: Parser PseudoBody
pPseudoBody =
  IdentPseudo <$> pIdent
    <|> FunctionalPseudo <$> pFunctionName <*> (skipWs *> someTill nonBracket (skipWs <* single RightParen))

---
-- parseSelector :: Parser Selector
-- parseSelector = fmap Selector $ skipWs *> someTill pSelectorToken (try pEnd)
-- where
-- pSelectorToken = noneOf [LeftCurlyBracket, RightCurlyBracket, Comma]
-- pEnd = skipWs <* notFollowedBy pSelectorToken
---

pFunctionName :: Parser Text
pFunctionName = token test Set.empty <?> "function"
  where
    test (Function name) = Just name
    test _ = Nothing

-- data MatchType = PrefixMatch | SuffixMatch | SubstringMatch | EqualsMatch | IncludeMatch | DashMatch
data MatchType = Prefix | Suffix | Substring | Equals | Include | Dash

parseMatch :: Parser (MatchType, Text)
parseMatch = (,) <$> parseMatchType <*> pIdent
  where
    parseMatchType :: Parser MatchType
    parseMatchType =
      choice
        [ single PrefixMatch $> Prefix,
          single SuffixMatch $> Suffix,
          single SubstringMatch $> Substring,
          single (Delim '=') $> Equals,
          single IncludeMatch $> Include,
          single DashMatch $> Dash
        ]

parseAttribute :: Parser Attrib
parseAttribute = do
  skipWs
  --todo: optional namespace_prefix
  ident <- pIdent
  skipWs
  maybeMatch <- optional parseMatch
  return $ case maybeMatch of
    Nothing -> SimpleAttrib ident
    Just (Prefix, attr) -> PrefixAttrib ident attr
    Just (Suffix, attr) -> SuffixAttrib ident attr
    Just (Substring, attr) -> SubstringAttrib ident attr
    Just (Equals, attr) -> EqualsAttrib ident attr
    Just (Include, attr) -> IncludeAttrib ident attr
    Just (Dash, attr) -> DashAttrib ident attr
