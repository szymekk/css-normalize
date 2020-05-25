module Types
  ( Declaration (..),
    Key (..),
    QualifiedRule (..),
    MediaRule (..),
    StylesheetElement (..),
    Stylesheet (..),
    AtRule (..),
    Block (..),
    Selector (..),
    StylesheetOpts (..),
    defaultOpts,
  )
where

import Balanced
import Data.CSS.Syntax.Tokens as CSS
import Data.Text

newtype Selector = Selector {unSelector :: [CSS.Token]}
  deriving (Eq, Show)

data QualifiedRule = QualifiedRule [Selector] [Declaration]
  deriving (Eq, Show)

newtype Key = Key Text
  deriving (Eq, Ord, Show)

data Declaration = Declaration Key Balanced
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

data StylesheetOpts
  = Opts
      { sortSelectors :: Bool,
        sortProperties :: Bool,
        addLeadingZeros :: Bool
      }

defaultOpts :: StylesheetOpts
defaultOpts =
  Opts
    { sortSelectors = True,
      sortProperties = True,
      addLeadingZeros = True
    }
