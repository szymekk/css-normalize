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
    parseMediaRulePreludeBody,
    parseSelector,
    parseSelectorsGroup,
    Parser,
  )
where

import Balanced
import Control.Monad
import Data.CSS.Syntax.Tokens as CSS
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
pAtKeyword = token test Set.empty <?> "AtKeyword"
  where
    test (AtKeyword str) = Just str
    test _ = Nothing

parseUnknownAtRule :: Text -> Parser AtRule
parseUnknownAtRule name = do
  prelude <- manyTill anySingle $ try (lookAhead parsePreludeEnd)
  terminator <- lookAhead parsePreludeEnd
  case terminator of
    Left _ -> do
      -- LeftCurlyBracket
      (Block blockTokens) <- parseBlockCurly
      return $ BlockAtRule name prelude blockTokens
    Right _ -> do
      -- Semicolon
      void (single Semicolon)
      return $ SemicolonAtRule name prelude
  where
    parsePreludeEnd = void <$> eitherP (single LeftCurlyBracket) (single Semicolon)

parseAtRule :: Parser StylesheetElement
parseAtRule = do
  ruleName <- pAtKeyword
  let result = case ruleName of
        "media" -> Media <$> parseMediaRulePreludeBody
        _ -> parseUnknownAtRule ruleName
  AtRule <$> result

parseStylesheetElement :: Parser StylesheetElement
parseStylesheetElement =
  (try (lookAhead pAtKeyword) >> parseAtRule)
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
  name <- pAtKeyword
  case name of
    "media" -> parseMediaRulePreludeBody
    _ -> fail ""

parseMediaRulePreludeBody :: Parser MediaRule
parseMediaRulePreludeBody = do
  skipWs
  prelude <- manyTill anySingle $ try (skipWs *> single LeftCurlyBracket)
  stylesheet <- parseStylesheetEnd (single RightCurlyBracket)
  return $ MediaRule prelude stylesheet
