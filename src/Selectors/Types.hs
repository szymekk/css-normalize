-- | Internal data definitions for types related to CSS selectors.
module Selectors.Types
  ( SelectorsGroup (..),
    Selector (..),
    Combinator (..),
    SimpleSelectorSeq (..),
    TypeLikeSelector (..),
    SimpleSelector (..),
    Negation (..),
    Common (..),
    Class (..),
    Id (..),
    Attribute (..),
    AttrValueMatch (..),
    AttributeMatchType (..),
    AttrName (..),
    AttrVal (..),
    PseudoElement (..),
    PseudoClass (..),
    PseudoBody (..),
  )
where

import Balanced
import Data.List.NonEmpty
import Data.Text

-- | Type representing a comma separated list of CSS selectors.
newtype SelectorsGroup = SelectorsGroup {unSelectorsGroup :: NonEmpty Selector}
  deriving (Eq, Show)

-- | Type representing CSS selectors i.e. a list of 'SimpleSelectorSeq'
-- separated by combinators.
data Selector = Selector SimpleSelectorSeq [(Combinator, SimpleSelectorSeq)]
  deriving (Eq, Show)

-- | Type of a combinator appearing between simple selector sequences e.g. @+@
-- in @div + p@.
data Combinator = Plus | Greater | Tilde | Space
  deriving (Eq, Show)

-- | Type representing simple selector sequence.
data SimpleSelectorSeq
  = SimpleSelectorSeq TypeLikeSelector [SimpleSelector]
  deriving (Eq, Show)

-- | 'TypeLikeSelector' represents a concrete type selector or a universal
-- selector.
data TypeLikeSelector = TypeSelector Text | Universal
  deriving (Eq, Show)

-- | Simple selector making up 'SimpleSelectorSeq'.
data SimpleSelector = CommonSimpleSelector Common | Not Negation
  deriving (Eq, Show)

-- | Negation pseudo-class selector.
data Negation = NegationTypeLike TypeLikeSelector | NegationCommon Common
  deriving (Eq, Show)

-- | Common class for several simple selector types.
data Common
  = CommonId Id
  | CommonClass Class
  | CommonAttribute Attribute
  | CommonPseudoElement PseudoElement
  | CommonPseudoClass PseudoClass
  deriving (Eq, Show)

-- | ID selector.
newtype Id = Id Text deriving (Eq, Show)

-- | Class selector.
newtype Class = Class Text deriving (Eq, Show)

-- | Attribute selector.
data Attribute = Attribute AttrName (Maybe AttrValueMatch)
  deriving (Eq, Show)

-- | Name of an attribute.
newtype AttrName = AttrName Text deriving (Eq, Show)

-- | Fragment of an attribute's value.
newtype AttrVal = AttrVal Text deriving (Eq, Show)

-- | Matching rule applied to values of an Attribute.
data AttrValueMatch = AttrValueMatch AttributeMatchType AttrVal
  deriving (Eq, Show)

-- | Attribute selector matching rule type.
data AttributeMatchType = Prefix | Suffix | Substring | Equals | Include | Dash
  deriving (Eq, Show)

-- | Pseudo-element selector.
newtype PseudoElement = PseudoElement PseudoBody deriving (Eq, Show)

-- | Pseudo-class selector.
newtype PseudoClass = PseudoClass PseudoBody deriving (Eq, Show)

-- | Body of a pseudo-element or pseudo-class selector.
data PseudoBody = IdentPseudo Text | FunctionalPseudo Text Balanced
  deriving (Eq, Show)
