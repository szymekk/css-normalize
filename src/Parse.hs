{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( parseDeclarationValues,
    parseOneDeclaration,
    parseQualifiedRule,
    Declaration (..),
    Key (..),
    QualifiedRule (..),
    parseBlockCurly,
    parseStylesheet,
    pSimple,
    parseUnknownAtRule,
    parseMediaRule,
    parseMediaRulePreludeBody,
    MediaRule (..),
    StylesheetElement (..),
    Stylesheet (..),
    AtRule (..),
    Parser,
  )
where

import Control.Monad
import Data.CSS.Syntax.Tokens as CSS
import Data.Functor
import Data.Set as Set hiding (foldr, null)
import Data.Text hiding (concat)
import Data.Void
import Text.Megaparsec
import TokenStream ()

type Parser = Parsec Void [CSS.Token]

data QualifiedRule = QualifiedRule [CSS.Token] [Declaration]
  deriving (Eq, Show)

newtype Key = Key Text
  deriving (Show, Eq)

data Declaration = Declaration Key [CSS.Token]
  deriving (Eq, Show)

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

data BracketType = Round | Curly | Square
  deriving (Eq, Show)

newtype Block = Block [CSS.Token]
  deriving (Eq, Show)

parseBlockCurly :: Parser Block
parseBlockCurly = do
  void $ lookAhead (single LeftCurlyBracket)
  innerTokens <-
    try (pSimple <?> "simple") $> []
      <|> (pEnclosed <?> "enclosed")
  return $ Block innerTokens

nonBracket :: Parser CSS.Token
nonBracket = noneOf [LeftCurlyBracket, RightCurlyBracket, LeftSquareBracket, RightSquareBracket, LeftParen, RightParen]

pSimpleWithOuterBrackets :: Parser [CSS.Token]
pSimpleWithOuterBrackets = do
  bracketType <- pSimpleBracketType
  let (opening, closing) = getBrackets bracketType
  return [opening, closing]

getBrackets :: BracketType -> (CSS.Token, CSS.Token)
getBrackets bracketType = case bracketType of
  Round -> (LeftParen, RightParen)
  Curly -> (LeftCurlyBracket, RightCurlyBracket)
  Square -> (LeftSquareBracket, RightSquareBracket)

pSimpleBracketType :: Parser BracketType
pSimpleBracketType = do
  bracketType <- pOpen
  void $ case bracketType of
    Round -> single RightParen
    Curly -> single RightCurlyBracket
    Square -> single RightSquareBracket
  return bracketType

pSimple :: Parser ()
pSimple = void pSimpleBracketType

pEnclosed :: Parser [CSS.Token]
pEnclosed = snd <$> pEnclosedWithBracketType

pEnclosedWithOuterBrackets :: Parser [CSS.Token]
pEnclosedWithOuterBrackets = do
  (bracketType, ts) <- pEnclosedWithBracketType
  let (opening, closing) = getBrackets bracketType
  return $ opening : ts ++ [closing]

pEnclosedWithBracketType :: Parser (BracketType, [CSS.Token])
pEnclosedWithBracketType = do
  bracketType <- pOpen
  tokenList <- parseSerial
  void $ case bracketType of
    Round -> single RightParen
    Curly -> single RightCurlyBracket
    Square -> single RightSquareBracket
  return (bracketType, tokenList)

parseSerial :: Parser [CSS.Token]
parseSerial = do
  listOfLists <- many $ try pSimpleWithOuterBrackets <|> try (fmap pure nonBracket) <|> pEnclosedWithOuterBrackets
  return (concat listOfLists)

pOpen :: Parser BracketType
pOpen = token predicate Set.empty <?> "opening"
  where
    predicate LeftParen = Just Round
    predicate LeftCurlyBracket = Just Curly
    predicate LeftSquareBracket = Just Square
    predicate _ = Nothing

data StylesheetElement = StyleRule QualifiedRule | AtRule AtRule
  deriving (Eq, Show)

newtype Stylesheet = Stylesheet [StylesheetElement]
  deriving (Eq, Show)

pAtKeyword :: Parser Text
pAtKeyword = token test Set.empty <?> "AtKeyword"
  where
    test (AtKeyword str) = Just str
    test _ = Nothing

data AtRule = Media MediaRule | BlockAtRule Text [CSS.Token] [CSS.Token] | SemicolonAtRule Text [CSS.Token]
  deriving (Eq, Show)

data MediaRule = MediaRule [CSS.Token] Stylesheet
  deriving (Eq, Show)

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
