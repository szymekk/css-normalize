{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Normalize where

import Balanced
import Balanced.Internal
import Data.CSS.Syntax.Tokens as CSS
import Data.List
import Data.Text as T
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

pattern (:.) :: Char -> Text -> Text
pattern x :. xs <- (T.uncons -> Just (x, xs))

infixr 5 :.

-- | Add leading zero to strings representing numeric literals with an ommited
-- integer part i.e those starting with a decimal comma.
--
-- >>> addLeadingZero "-.5"
-- "-0.5"
addLeadingZero :: Text -> Text
addLeadingZero ts = case ts of
  '.' :. _ts' -> "0" <> ts
  s :. '.' :. ts'
    | s `elem` ['+', '-'] -> singleton s <> "0." <> ts'
  _ -> ts

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
