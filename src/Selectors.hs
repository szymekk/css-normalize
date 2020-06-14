{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parsing CSS selectors.
module Selectors
  ( parseSelectorsGroup,
  )
where

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
    parseSelector `NE.sepBy1` (single Comma <* skipWs)

-- | Parse a CSS selector.
parseSelector :: Parser Selector
parseSelector = do
  first <- parseSimpleSelectorSeq
  rest <- many $ (,) <$> parseCombinator <*> parseSimpleSelectorSeq
  return $ Selector (first, rest)

-- | Parse a selector combinator.
parseCombinator :: Parser Combinator
parseCombinator =
  choice
    [ single (Delim '+') $> Plus,
      single (Delim '>') $> Greater,
      single (Delim '~') $> Tilde,
      single Whitespace $> Space
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
  CommonSimpleSelector <$> parseCommon
    <|> Not <$> parseNegation

-- | Parse a simple selector.
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
    [ CommonClass <$> parseClass,
      CommonAttribute <$> parseAttribute,
      CommonId <$> parseId,
      CommonPseudoElement <$> parsePseudoElement,
      CommonPseudoClass <$> parsePseudoClass
    ]

-- | Parse an ID selector.
parseId :: Parser IdSelector
parseId = IdSelector <$> pHash

pHash :: Parser Text
pHash = token test Set.empty <?> "hash"
  where
    test (Hash _ text) = Just text
    test _ = Nothing

-- | Parse a class selector.
parseClass :: Parser Class
parseClass = single (Delim '.') *> (Class <$> pIdent)

-- | Attribute selector matching type.
data AttributeMatchType = Prefix | Suffix | Substring | Equals | Include | Dash

-- | Parse the match type of an attribute selector.
parseMatchType :: Parser AttributeMatchType
parseMatchType =
  choice
    [ single PrefixMatch $> Prefix,
      single SuffixMatch $> Suffix,
      single SubstringMatch $> Substring,
      single (Delim '=') $> Equals,
      single IncludeMatch $> Include,
      single DashMatch $> Dash
    ]

-- | Convert an attribute match type into a corresponding data constructor of
-- the 'Attribute' type.
attributeMatchToConstructor :: AttributeMatchType -> (Text -> Text -> Attribute)
attributeMatchToConstructor Prefix = PrefixAttribute
attributeMatchToConstructor Suffix = SuffixAttribute
attributeMatchToConstructor Substring = SubstringAttribute
attributeMatchToConstructor Equals = EqualsAttribute
attributeMatchToConstructor Include = IncludeAttribute
attributeMatchToConstructor Dash = DashAttribute

-- | Parse an attribute selector.
parseAttribute :: Parser Attribute
parseAttribute = squareBrackets parseAttribute'
  where
    parseAttribute' = do
      attribute <- betweenWs pIdent
      maybeValue <- optional $ (,) <$> parseMatchType <*> betweenWs pIdent
      return $
        maybe
          (SimpleAttribute attribute)
          ( \(matchType, value) ->
              let attribCtor = attributeMatchToConstructor matchType
               in attribCtor attribute value
          )
          maybeValue

-- | Parse a pseudo-element.
parsePseudoElement :: Parser PseudoElement
parsePseudoElement = single Colon *> single Colon *> (PseudoElement <$> parsePseudoBody)

-- | Parse a pseudo-class.
parsePseudoClass :: Parser PseudoClass
parsePseudoClass = single Colon *> (PseudoClass <$> parsePseudoBody)

-- | Parse the body of a pseudo selector.
parsePseudoBody :: Parser PseudoBody
parsePseudoBody =
  IdentPseudo <$> pIdent
    <|> FunctionalPseudo <$> pFunctionName <*> (skipWs *> someTill nonBracket (skipWs <* single RightParen))

-- | Parse a function token. Returns the name of the function.
pFunctionName :: Parser Text
pFunctionName = token test Set.empty <?> "function"
  where
    test (Function name) = Just name
    test _ = Nothing
