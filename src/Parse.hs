{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( parseDeclarationValues,
    parseOneDeclaration,
    parseQualifiedRule,
    parseBlockCurly,
    parseStylesheet,
    parseUnknownAtRule,
    parseMediaRule,
    parseMediaRulePreludeBody,
  )
where

import Control.Monad
import Data.CSS.Syntax.Tokens as CSS
import Data.Functor
import Data.Set as Set hiding (foldr, null)
import Data.Text hiding (concat)
import Text.Megaparsec
import TokenStream ()
import Types

manyWs :: Parser [CSS.Token]
manyWs = many (single Whitespace)

parseDeclarationValues :: Parser [CSS.Token]
parseDeclarationValues =
  manyTill parseDeclarationToken (lookAhead parseEnding)
  where
    parseDeclarationToken :: Parser CSS.Token
    parseDeclarationToken = noneOf [LeftCurlyBracket, LeftSquareBracket, LeftParen]
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
  prelude <- manyTill nonCurly $ try (manyWs *> single LeftCurlyBracket)
  void manyWs
  QualifiedRule prelude <$> parseDeclarationList
  where
    nonCurly = noneOf [LeftCurlyBracket, RightCurlyBracket]

parseDeclarationList :: Parser [Declaration]
parseDeclarationList = do
  declarations <- sepEndBy parseOneDeclaration (try pDeclarationSeparator)
  void $ manyWs *> single RightCurlyBracket
  return declarations

-- | Parse a list of tokens enclosed by a pair of curly brackets.
-- Any inner bracket tokens must be properly balanced.
-- Each 'LeftCurlyBracket' must be closed by a 'RightCurlyBracket'.
-- Each 'LeftSquareBracket' must be closed by a 'RightSquareBracket'.
-- Each 'LeftParen' must be closed by a 'RightParen'.
-- Each 'Function' must be closed by a 'RightParen'.
-- Otherwise the inner tokens are arbitrary.
parseBlockCurly :: Parser Block
parseBlockCurly = do
  void $ lookAhead (single LeftCurlyBracket)
  innerTokens <-
    try (pSimple <?> "simple") $> []
      <|> (pEnclosed <?> "enclosed")
  return $ Block innerTokens

pSimpleWithOuterBrackets :: Parser [CSS.Token]
pSimpleWithOuterBrackets = do
  (opening, closing) <-
    try ((,) <$> single LeftCurlyBracket <*> single RightCurlyBracket)
      <|> try ((,) <$> single LeftSquareBracket <*> single RightSquareBracket)
      <|> ((,) <$> single LeftParen <*> single RightParen)
  return [opening, closing]

pSimple :: Parser ()
pSimple = void pSimpleWithOuterBrackets

pEnclosed :: Parser [CSS.Token]
pEnclosed = snd <$> pEnclosedWithBracketType

pBlockBeginning :: Parser BracketType
pBlockBeginning = token test Set.empty <?> "function"
  where
    test LeftCurlyBracket = Just Curly
    test LeftSquareBracket = Just Square
    test LeftParen = Just Round
    test (Function name) = Just (FunctionToken name)
    test _ = Nothing

pEnclosedWithOuterBrackets :: Parser [CSS.Token]
pEnclosedWithOuterBrackets = do
  (bracketType, ts) <- pEnclosedWithBracketType
  let (opening, closing) = getBrackets bracketType
  return $ opening : ts ++ [closing]
  where
    getBrackets :: BracketType -> (CSS.Token, CSS.Token)
    getBrackets (FunctionToken name) = (Function name, RightParen)
    getBrackets Round = (LeftParen, RightParen)
    getBrackets Curly = (LeftCurlyBracket, RightCurlyBracket)
    getBrackets Square = (LeftSquareBracket, RightSquareBracket)

pEnclosedWithBracketType :: Parser (BracketType, [CSS.Token])
pEnclosedWithBracketType = do
  blockType <- pBlockBeginning
  tokenList <- parseSerial
  void $ case blockType of
    FunctionToken _ -> single RightParen
    Round -> single RightParen
    Curly -> single RightCurlyBracket
    Square -> single RightSquareBracket
  return (blockType, tokenList)

parseSerial :: Parser [CSS.Token]
parseSerial = do
  listOfLists <-
    many $
      try pSimpleFunction
        <|> try pSimpleWithOuterBrackets
        <|> try (fmap pure nonBracket)
        <|> pEnclosedWithOuterBrackets
  return (concat listOfLists)

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

pFunction :: Parser CSS.Token
pFunction = token test Set.empty <?> "function"
  where
    test (Function name) = Just (Function name)
    test _ = Nothing

pSimpleFunction :: Parser [CSS.Token]
pSimpleFunction = do
  function <- pFunction
  closing <- single RightParen
  return [function, closing]

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

parseStylesheet :: Parser Stylesheet
parseStylesheet = Stylesheet <$> (manyWs *> sepEndBy parseStylesheetElement manyWs)

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
  stylesheet <- parseStylesheet <* single RightCurlyBracket
  return $ MediaRule prelude stylesheet
