module Types
  ( Declaration (..),
    Key (..),
    QualifiedRule (..),
    MediaRule (..),
    StylesheetElement (..),
    Stylesheet (..),
    AtRule (..),
    BracketType (..),
    Block (..),
    Parser,
  )
where

import Data.CSS.Syntax.Tokens as CSS
import Data.Text
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void [CSS.Token]

data QualifiedRule = QualifiedRule [CSS.Token] [Declaration]
  deriving (Eq, Show)

newtype Key = Key Text
  deriving (Eq, Show)

data Declaration = Declaration Key [CSS.Token]
  deriving (Eq, Show)

data BracketType = FunctionToken Text | Round | Curly | Square
  deriving (Eq, Show)

newtype Block = Block [CSS.Token]
  deriving (Eq, Show)

data StylesheetElement
  = StyleRule QualifiedRule
  | AtRule AtRule
  deriving (Eq, Show)

newtype Stylesheet = Stylesheet [StylesheetElement]
  deriving (Eq, Show)

data AtRule
  = Media MediaRule
  | BlockAtRule Text [CSS.Token] [CSS.Token]
  | SemicolonAtRule Text [CSS.Token]
  deriving (Eq, Show)

data MediaRule = MediaRule [CSS.Token] Stylesheet
  deriving (Eq, Show)
