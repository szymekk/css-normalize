-- | Type definitions.
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
  )
where

import Balanced
import Data.CSS.Syntax.Tokens as CSS
import Data.Text

-- | A type representing CSS selectors.
newtype Selector = Selector {unSelector :: [CSS.Token]}
  deriving (Eq, Show)

-- | A type representing CSS qualified rules. Parsed from style rules.
data QualifiedRule = QualifiedRule [Selector] [Declaration]
  deriving (Eq, Show)

-- | The left hand side of a CSS declaration e.g. @color@ in @color: red@.
newtype Key = Key Text
  deriving (Eq, Ord, Show)

-- | A CSS declaration e.g. @color: red@.
data Declaration = Declaration Key Balanced
  deriving (Eq, Show)

-- | A type for tokens between curly braces in an at-rule rule.
newtype Block = Block {unBlock :: [CSS.Token]}
  deriving (Eq, Show)

-- | A component of a stylesheet i.e. an at-rule or a qualified rule.
data StylesheetElement
  = StyleRule QualifiedRule
  | AtRule AtRule
  deriving (Eq, Show)

-- | A type representing a CSS stylesheet.
newtype Stylesheet = Stylesheet [StylesheetElement]
  deriving (Eq, Show)

-- | A type representing a CSS at-rule e.g. @\@charset "UTF-8";@.
data AtRule
  = Media MediaRule
  | BlockAtRule Text [CSS.Token] [CSS.Token]
  | SemicolonAtRule Text [CSS.Token]
  deriving (Eq, Show)

-- | A type representing a CSS \'\@media\' at-rule e.g. @\@media {}@.
data MediaRule = MediaRule [CSS.Token] Stylesheet
  deriving (Eq, Show)
