-- | Internal data definitions for types related to CSS selectors.
module Selectors.Types
  ( SelectorsGroup (..),
    Combinator (..),
    Selector (..),
    SimpleSelectorSeq (..),
    Common (..),
    SimpleSelector (..),
    TypeSelector (..),
    IdSelector (..),
    Class (..),
    Negation (..),
    Attribute (..),
    Pseudo (..),
    PseudoBody (..),
    PseudoElement (..),
    PseudoClass (..),
  )
where

import Data.CSS.Syntax.Tokens as CSS
import Data.List.NonEmpty
import Data.Text

-- | A type representing a comma separated list of CSS selectors.
newtype SelectorsGroup = SelectorsGroup {unSelectorsGroup :: NonEmpty Selector}
  deriving (Eq, Show)

-- | A combinator appearing between simple selector sequences e.g. @+@ in
-- @div + p@.
data Combinator = Plus | Greater | Tilde | Space
  deriving (Eq, Show)

-- | A type representing CSS selectors i.e. a list of 'SimpleSelectorSeq'
-- separated by combinators.
newtype Selector = Selector (SimpleSelectorSeq, [(Combinator, SimpleSelectorSeq)])
  deriving (Eq, Show)

-- | A type representing simple selector sequence.
data SimpleSelectorSeq
  = SimpleSelectorSeq TypeSelector [SimpleSelector]
  deriving (Eq, Show)

-- | Common class for several simple selector types.
data Common = CommonId IdSelector | CommonClass Class | CommonAttribute Attribute | CommonPseudo Pseudo
  deriving (Eq, Show)

-- | A concrete type selector or a universal selector.
data TypeSelector = TypeSelector Text | Universal
  deriving (Eq, Show)

-- | A simple selector making up 'SimpleSelectorSeq'.
data SimpleSelector = CommonSimpleSelector Common | Not Negation
  deriving (Eq, Show)

-- | An ID selector.
newtype IdSelector = IdSelector Text deriving (Eq, Show)

-- | A class selector.
newtype Class = Class Text deriving (Eq, Show)

-- | A negation pseudo-class selector.
data Negation = NegationTypeSelector TypeSelector | NegationCommon Common
  deriving (Eq, Show)

-- | An attribute selector.
data Attribute
  = SimpleAttribute Text
  | PrefixAttribute Text Text
  | SuffixAttribute Text Text
  | SubstringAttribute Text Text
  | EqualsAttribute Text Text
  | IncludeAttribute Text Text
  | DashAttribute Text Text
  deriving (Eq, Show)

-- | A pseudo-element or pseudo-class.
data Pseudo = PElement PseudoElement | PClass PseudoClass
  deriving (Eq, Show)

-- | Body of a pseudo-element or pseudo-class.
data PseudoBody = IdentPseudo Text | FunctionalPseudo Text [CSS.Token]
  deriving (Eq, Show)

-- | A pseudo-element.
newtype PseudoElement = PseudoElement PseudoBody deriving (Eq, Show)

-- | A pseudo-class.
newtype PseudoClass = PseudoClass PseudoBody deriving (Eq, Show)
