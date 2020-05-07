{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( parseDeclarationValues,
    parseOneDeclaration,
    parseQualifiedRule,
    Declaration (..),
    Key (..),
    QualifiedRule (..),
  )
where

import Control.Monad
import Data.CSS.Syntax.Tokens as CSS
import Data.Set as Set hiding (foldr, null)
import Data.Text
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
  prelude <- manyTill anySingle $ try (manyWs *> single LeftCurlyBracket)
  void manyWs
  QualifiedRule prelude <$> parseDeclarationList

parseDeclarationList :: Parser [Declaration]
parseDeclarationList = do
  declarations <- sepEndBy parseOneDeclaration (try pDeclarationSeparator)
  void $ manyWs *> single RightCurlyBracket
  return declarations
