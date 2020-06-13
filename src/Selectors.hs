{-# LANGUAGE OverloadedStrings #-}

-- | Type definitions for CSS selectors.
module Selectors
  ( parseSelectorsGroup
  )
where

import Balanced
import qualified Control.Monad.Combinators.NonEmpty as NE
import Data.CSS.Syntax.Tokens as CSS
import Data.List.NonEmpty
import Data.Functor
import Data.Set as Set
import Data.Text
import Parse (skipWs, pIdent)
import Parser
import Text.Megaparsec

-- | A type representing CSS selectors.
newtype SelectorsGroup = SelectorsGroup {unSelectorsGroup :: NonEmpty Selector}
  deriving (Eq, Show)

data Combinator = Plus | Greater | Tilde | Space
  deriving (Eq, Show)

data Selector = Selector (SimpleSelectorSeq, [(Combinator, SimpleSelectorSeq)])
  deriving (Eq, Show)

data SimpleSelectorSeq = WithTypeSelector (TypeSelector, [HCAPN]) | OnlyHCAPN (NonEmpty HCAPN)
  deriving (Eq, Show)

data HCAPN = HCAPN'IS IdSelector | HCAPN'CS Class | HCAPN'AS Attrib | HCAPN'PS Pseudo | HCAPN'NS Negation
  deriving (Eq, Show)

data TypeSelector = TypeSelector Text | Universal
  deriving (Eq, Show)

{-
simple_selector_sequence
  : [ type_selector | universal ]
    [ HASH | class | attrib | pseudo | negation ]*
  | [ HASH | class | attrib | pseudo | negation ]+
  ;
-}

data Attrib = SimpleAttrib Text
  | PrefixAttrib Text Text
  | SuffixAttrib Text Text
  | SubstringAttrib Text Text
  | EqualsAttrib Text Text
  | IncludeAttrib Text Text
  | DashAttrib Text Text
  deriving (Eq, Show)

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
    [single (Delim '+') $> Plus,
     single (Delim '>') $> Greater,
     single (Delim '~') $> Tilde,
     single Whitespace $> Space
    ]

parseSimpleSelectorSeq :: Parser SimpleSelectorSeq
parseSimpleSelectorSeq = (fmap WithTypeSelector $ (,) <$> pTypeSelector <*> many parseHCAPN)
  <|> OnlyHCAPN <$> NE.some parseHCAPN

parseHCAPN :: Parser HCAPN
parseHCAPN = HCAPN'IS <$> pId
  <|> HCAPN'CS <$> pClass
  <|> HCAPN'AS <$> parseAttribute
  <|> HCAPN'NS <$> pNegation
  <|> HCAPN'PS <$> pPseudo

pNegation :: Parser Negation
pNegation = fmap Negation $ single Colon *> ((pNot *> skipWs) *> parseNegationArg <* (skipWs <* single RightParen))
  where
    pNot = single (Function "not")


parseNegationArg :: Parser NegationArg
parseNegationArg = TS <$> pTypeSelector
  <|> IS <$> pId
  <|> CS <$> pClass
  <|> AS <$> parseAttribute
  <|> PS <$> pPseudo

-- data NegationArg = TS TypeSelector | IS IdSelector | CS Class | AS Attrib | PS Pseudo
  -- deriving (Eq, Show)


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

data Pseudo = PElement PseudoElement | PClass PseudoClass
  deriving (Eq, Show)

data PseudoBody = IdentPseudo Text | FunctionalPseudo Text [CSS.Token]
  deriving (Eq, Show)

data PseudoElement = PseudoElement PseudoBody deriving (Eq, Show)
data PseudoClass = PseudoClass PseudoBody deriving (Eq, Show)

pPseudoBody :: Parser PseudoBody
pPseudoBody = IdentPseudo <$> pIdent
  <|> FunctionalPseudo <$> pFunctionName <*> (skipWs *> someTill nonBracket (skipWs <* single RightParen))

{-
negation_arg
  : type_selector | universal | HASH | class | attrib | pseudo
-}

-- data NegationArg = TypeSelector | Universal | Hash | Class | Attrib | Pseudo
data NegationArg = TS TypeSelector | IS IdSelector | CS Class | AS Attrib | PS Pseudo
  deriving (Eq, Show)

data IdSelector = IdSelector Text deriving (Eq, Show)
data Class = Class Text deriving (Eq, Show)
data Negation = Negation NegationArg deriving (Eq, Show)

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
        [single PrefixMatch $> Prefix,
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
    Just (Prefix, attr) -> (PrefixAttrib ident attr)
    Just (Suffix, attr) -> (SuffixAttrib ident attr)
    Just (Substring, attr) -> (SubstringAttrib ident attr)
    Just (Equals, attr) -> (EqualsAttrib ident attr)
    Just (Include, attr) -> (IncludeAttrib ident attr)
    Just (Dash, attr) -> (DashAttrib ident attr)
