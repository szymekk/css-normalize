{-# LANGUAGE OverloadedStrings #-}

-- | Parsing CSS files.
module Parse
  ( parseDeclarationValues,
    parseBlockCurly,
    parseOneDeclaration,
    parseQualifiedRule,
    parseStylesheetEof,
    parseStylesheet,
    parseUnknownAtRule,
    parseMediaRule,
    parseSelector,
    parseSelectorsGroup,
    Parser,
  )
where

import Balanced
import Control.Monad
import Data.CSS.Syntax.Tokens as CSS
import Data.Functor
import Data.Set as Set hiding (foldr, null)
import Data.Text hiding (concat)
import Parse.Internal
import Text.Megaparsec
import Types

-- | Parse the value of a CSS declaration (i.e. the part after the colon).
parseDeclarationValues :: Parser Balanced
parseDeclarationValues =
  manyBalancedTill (lookAhead parseEnding)
  where
    parseEnding = try pDeclarationSeparator <|> void (try (skipWs *> single RightCurlyBracket))

pDeclarationSeparator :: Parser ()
pDeclarationSeparator = void $ skipWs *> single Semicolon <* skipWs

-- | Parse a single CSS declaration.
parseOneDeclaration :: Parser Declaration
parseOneDeclaration = do
  propertyName <- Key <$> pIdent
  skipWs
  void $ single Colon
  skipWs
  Declaration propertyName <$> parseDeclarationValues

-- | Parse a qualified rule.
parseQualifiedRule :: Parser QualifiedRule
parseQualifiedRule = do
  selectors <- parseSelectorsGroup
  void (single LeftCurlyBracket)
  skipWs
  QualifiedRule selectors <$> parseDeclarationList

-- | Parse a selector group i.e. a comma separated sequence of selectors.
parseSelectorsGroup :: Parser [Selector]
parseSelectorsGroup = parseSelector `sepBy1` single Comma

-- | Parse a selector. Any leading and trailing whitespace tokens are dropped.
parseSelector :: Parser Selector
parseSelector = fmap Selector $ skipWs *> someTill pSelectorToken (try pEnd)
  where
    pSelectorToken = noneOf [LeftCurlyBracket, RightCurlyBracket, Comma]
    pEnd = skipWs <* notFollowedBy pSelectorToken

parseDeclarationList :: Parser [Declaration]
parseDeclarationList = do
  declarations <- sepEndBy parseOneDeclaration (try pDeclarationSeparator)
  void $ skipWs *> single RightCurlyBracket
  return declarations

-- | Parse a list of tokens enclosed by a pair of curly brackets.
-- Any inner bracket tokens must be properly balanced.
parseBlockCurly :: Parser Block
parseBlockCurly =
  Block . unBalanced <$> betweenCurly manyBalanced
  where
    manyBalanced = someBalanced <|> return mempty

pAtKeyword :: Parser Text
pAtKeyword = token test Set.empty <?> "at-keyword"
  where
    test (AtKeyword str) = Just str
    test _ = Nothing

pAtMedia :: Parser ()
pAtMedia = token test Set.empty <?> "@media"
  where
    test (AtKeyword "media") = Just ()
    test _ = Nothing

-- | Parse an at-rule, according to a generic grammar.
parseUnknownAtRule :: Parser AtRule
parseUnknownAtRule = do
  ruleName <- pAtKeyword
  prelude <- many (noneOf [LeftCurlyBracket, Semicolon])
  BlockAtRule ruleName prelude . unBlock <$> parseBlockCurly
    <|> single Semicolon $> SemicolonAtRule ruleName prelude

-- | Parse an at-rule. Use specialized grammars for known at-rules.
parseAtRule :: Parser AtRule
parseAtRule = (Media <$> parseMediaRule) <|> parseUnknownAtRule

parseStylesheetElement :: Parser StylesheetElement
parseStylesheetElement =
  AtRule <$> parseAtRule
    <|> StyleRule <$> parseQualifiedRule

-- | Parse a CSS stylesheet.
parseStylesheet :: Parser Stylesheet
parseStylesheet = do
  skipWs
  stylesheetElements <- sepEndBy (try parseStylesheetElement) skipWs
  return (Stylesheet stylesheetElements)

-- | Parse a CSS stylesheet followed by end of input.
parseStylesheetEof :: Parser Stylesheet
parseStylesheetEof = parseStylesheet <* eof

-- | Parse a \'@media\' rule.
parseMediaRule :: Parser MediaRule
parseMediaRule = do
  void $ try pAtMedia
  skipWs
  prelude <- manyTill anySingle $ try (skipWs *> single LeftCurlyBracket)
  stylesheet <- parseStylesheet <* single RightCurlyBracket
  return $ MediaRule prelude stylesheet
