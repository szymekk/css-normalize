{-# LANGUAGE OverloadedStrings #-}

module Normalize where

import Balanced
import Balanced.Internal
import Data.CSS.Syntax.Tokens as CSS
import Data.List
import Data.Text
import Types

-- | Transform a stylesheet into another one in a canonical form.
-- The returned stylesheet is semantically equivalent to the input.
-- The transorm is canonical in the sense that normalization is idempotent.
normalizeStylesheet :: Stylesheet -> Stylesheet
normalizeStylesheet (Stylesheet elements) =
  Stylesheet (fmap normalizeStylesheetElement elements)

normalizeStylesheetElement :: StylesheetElement -> StylesheetElement
normalizeStylesheetElement (StyleRule rule) = StyleRule (normalizeStyleRule rule)
normalizeStylesheetElement (AtRule rule) = AtRule (normalizeAtRule rule)

-- | Normalize a style rule by sorting its' declarations by keys
-- and sorting the selectors of it's prelude.
normalizeStyleRule :: QualifiedRule -> QualifiedRule
normalizeStyleRule (QualifiedRule prelude declarations) =
  QualifiedRule prelude' declarations'
  where
    prelude' = normalizeSelectorsGroup prelude
    declarations' =
      sortBy compareKeys (fmap normalizeOneDeclaration declarations)
    compareKeys (Declaration k1 _) (Declaration k2 _) = k1 `compare` k2
    normalizeOneDeclaration (Declaration key values) =
      let values' = UnsafeBalanced $ fmap normalizeToken (unBalanced values)
       in Declaration key values'

normalizeToken :: CSS.Token -> CSS.Token
normalizeToken = mapNumericToken addLeadingZero

mapNumericToken :: (Text -> Text) -> CSS.Token -> CSS.Token
mapNumericToken f (Number t v) = Number (f t) v
mapNumericToken f (Dimension t nv u) = Dimension (f t) nv u
mapNumericToken f (Percentage t nv) = Percentage (f t) nv
mapNumericToken _ x = x

-- | Add leading zero to strings representing numeric literals with an ommited
-- integer part i.e those starting with a decimal comma.
--
-- >>> addLeadingZero "-.5"
-- "-0.5"
addLeadingZero :: Text -> Text
addLeadingZero text = case Data.Text.uncons text of
  Just (x, xs)
    | x == '.' -> "0" <> text
    | x `elem` ['+', '-'] -> case Data.Text.uncons xs of
      Just (x', _xs') -> if x' == '.' then singleton x <> "0" <> xs else text
      Nothing -> text
    | otherwise -> text
  Nothing -> text

-- | Normalize a group of selectors by sorting its' constituent selectors.
-- The order in which selectors appear in a comma separated group of selectors
-- does not change the semantics of a selection.
normalizeSelectorsGroup :: [Selector] -> [Selector]
normalizeSelectorsGroup = fmap Selector . sort . fmap unSelector

normalizeAtRule :: AtRule -> AtRule
normalizeAtRule (Media rule) = Media (normalizeMediaRule rule)
normalizeAtRule other = other

-- | Normalize a @media rule by normalizing its' inner stylesheet.
normalizeMediaRule :: MediaRule -> MediaRule
normalizeMediaRule (MediaRule prelude stylesheet) =
  MediaRule prelude (normalizeStylesheet stylesheet)
