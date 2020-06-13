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
    Attrib (..),
    Pseudo (..),
    PseudoBody (..),
    PseudoElement (..),
    PseudoClass (..),
  )
where

import Data.CSS.Syntax.Tokens as CSS
import Data.List.NonEmpty
import Data.Text

-- | A type representing CSS selectors.
newtype SelectorsGroup = SelectorsGroup {unSelectorsGroup :: NonEmpty Selector}
  deriving (Eq, Show)

data Combinator = Plus | Greater | Tilde | Space
  deriving (Eq, Show)

newtype Selector = Selector (SimpleSelectorSeq, [(Combinator, SimpleSelectorSeq)])
  deriving (Eq, Show)

data SimpleSelectorSeq
  = WithTypeSelector (TypeSelector, [SimpleSelector])
  | OnlySimpleSelectors (NonEmpty SimpleSelector)
  deriving (Eq, Show)

data Common = CommonId IdSelector | CommonClass Class | CommonAttrib Attrib | CommonPseudo Pseudo
  deriving (Eq, Show)

data SimpleSelector = CommonSelector Common | NegationSelector Negation
  deriving (Eq, Show)

data TypeSelector = TypeSelector Text | Universal
  deriving (Eq, Show)

{-
negation_arg
  : type_selector | universal | HASH | class | attrib | pseudo
-}

newtype IdSelector = IdSelector Text deriving (Eq, Show)

newtype Class = Class Text deriving (Eq, Show)

data Negation = NegationTypeSelector TypeSelector | NegationCommon Common
  deriving (Eq, Show)

{-
simple_selector_sequence
  : [ type_selector | universal ]
    [ HASH | class | attrib | pseudo | negation ]*
  | [ HASH | class | attrib | pseudo | negation ]+
  ;
-}

data Attrib
  = SimpleAttrib Text
  | PrefixAttrib Text Text
  | SuffixAttrib Text Text
  | SubstringAttrib Text Text
  | EqualsAttrib Text Text
  | IncludeAttrib Text Text
  | DashAttrib Text Text
  deriving (Eq, Show)

data Pseudo = PElement PseudoElement | PClass PseudoClass
  deriving (Eq, Show)

data PseudoBody = IdentPseudo Text | FunctionalPseudo Text [CSS.Token]
  deriving (Eq, Show)

newtype PseudoElement = PseudoElement PseudoBody deriving (Eq, Show)

newtype PseudoClass = PseudoClass PseudoBody deriving (Eq, Show)
