{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( parseDeclarationValues,
    parseBlockCurly,
    parseOneDeclaration,
    parseQualifiedRule,
    parseStylesheet,
    parseUnknownAtRule,
    parseMediaRule,
    parseMediaRulePreludeBody,
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

manyWs :: Parser [CSS.Token]
manyWs = many (single Whitespace)

parseDeclarationValues :: Parser Balanced
parseDeclarationValues =
  manyBalancedTill (lookAhead parseEnding)
  where
    parseEnding = try pDeclarationSeparator <|> void (try (manyWs *> single RightCurlyBracket))

pDeclarationSeparator :: Parser ()
pDeclarationSeparator = void $ manyWs *> single Semicolon <* manyWs

pIdent :: Parser Text
pIdent = token test Set.empty <?> "ident"
  where
    test (Ident str) = Just str
    test _ = Nothing

parseOneDeclaration :: Parser Declaration
parseOneDeclaration = do
  propertyName <- Key <$> pIdent
  void manyWs
  void $ single Colon
  void manyWs
  Declaration propertyName <$> parseDeclarationValues

parseQualifiedRule :: Parser QualifiedRule
parseQualifiedRule = do
  prelude <- manyTill anySingle $ try (manyWs *> single LeftCurlyBracket)
  void manyWs
  QualifiedRule prelude <$> parseDeclarationList

parseDeclarationList :: Parser [Declaration]
parseDeclarationList = do
  declarations <- sepEndBy parseOneDeclaration (try pDeclarationSeparator)
  void $ manyWs *> single RightCurlyBracket
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
  void manyWs
  stylesheetElements <- sepEndBy (try parseStylesheetElement) manyWs
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
  void manyWs
  prelude <- manyTill anySingle $ try (manyWs *> single LeftCurlyBracket)
  stylesheet <- parseStylesheetEnd (single RightCurlyBracket)
  return $ MediaRule prelude stylesheet
