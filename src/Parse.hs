{-# LANGUAGE OverloadedStrings #-}

-- | Parsing CSS files.
module Parse
  ( parseDeclarationValues,
    parseBlockCurly,
    parseOneDeclaration,
    parseQualifiedRule,
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
import Parser
import Text.Megaparsec
import Types

-- | Skip and discard zero or more whitespace tokens.
skipWs :: Parser ()
skipWs = skipMany (single Whitespace)

parseDeclarationValues :: Parser Balanced
parseDeclarationValues =
  manyBalancedTill (lookAhead parseEnding)
  where
    parseEnding = try pDeclarationSeparator <|> void (try (skipWs *> single RightCurlyBracket))

pDeclarationSeparator :: Parser ()
pDeclarationSeparator = void $ skipWs *> single Semicolon <* skipWs

pIdent :: Parser Text
pIdent = token test Set.empty <?> "ident"
  where
    test (Ident str) = Just str
    test _ = Nothing

parseOneDeclaration :: Parser Declaration
parseOneDeclaration = do
  propertyName <- Key <$> pIdent
  skipWs
  void $ single Colon
  skipWs
  Declaration propertyName <$> parseDeclarationValues

parseQualifiedRule :: Parser QualifiedRule
parseQualifiedRule = do
  selectors <- parseSelectorsGroup
  void (single LeftCurlyBracket)
  skipWs
  QualifiedRule selectors <$> parseDeclarationList

parseSelectorsGroup :: Parser [Selector]
parseSelectorsGroup = parseSelector `sepBy1` single Comma

-- | Parse a selector, i.e. a list of tokens. Any leading and trailing
-- whitespace tokens are dropped.
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
    betweenCurly = between (single LeftCurlyBracket) (single RightCurlyBracket)

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

parseUnknownAtRule :: Parser AtRule
parseUnknownAtRule = do
  ruleName <- pAtKeyword
  prelude <- many (noneOf [LeftCurlyBracket, Semicolon])
  BlockAtRule ruleName prelude . unBlock <$> parseBlockCurly
    <|> single Semicolon $> SemicolonAtRule ruleName prelude

parseAtRule :: Parser AtRule
parseAtRule = (Media <$> parseMediaRule) <|> parseUnknownAtRule

parseStylesheetElement :: Parser StylesheetElement
parseStylesheetElement =
  AtRule <$> parseAtRule
    <|> StyleRule <$> parseQualifiedRule

parseStylesheetEnd :: Parser a -> Parser Stylesheet
parseStylesheetEnd pEnd = do
  skipWs
  stylesheetElements <- sepEndBy (try parseStylesheetElement) skipWs
  void pEnd
  return (Stylesheet stylesheetElements)

parseStylesheet :: Parser Stylesheet
parseStylesheet = parseStylesheetEnd eof

parseMediaRule :: Parser MediaRule
parseMediaRule = do
  void $ try pAtMedia
  skipWs
  prelude <- manyTill anySingle $ try (skipWs *> single LeftCurlyBracket)
  stylesheet <- parseStylesheetEnd (single RightCurlyBracket)
  return $ MediaRule prelude stylesheet
