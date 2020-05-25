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
normalizeStylesheet' :: StylesheetOpts -> Stylesheet -> Stylesheet
normalizeStylesheet' opts (Stylesheet elements) =
  Stylesheet (fmap (normalizeStylesheetElement' opts) elements)

normalizeStylesheet :: Stylesheet -> Stylesheet
normalizeStylesheet (Stylesheet elements) =
  Stylesheet (fmap normalizeStylesheetElement elements)

normalizeStylesheetElement' :: StylesheetOpts -> StylesheetElement -> StylesheetElement
normalizeStylesheetElement' opts (StyleRule rule) = StyleRule (normalizeStyleRule' opts rule)
normalizeStylesheetElement' opts (AtRule rule) = AtRule (normalizeAtRule' opts rule)

normalizeStylesheetElement :: StylesheetElement -> StylesheetElement
normalizeStylesheetElement (StyleRule rule) = StyleRule (normalizeStyleRule rule)
normalizeStylesheetElement (AtRule rule) = AtRule (normalizeAtRule rule)

normalizeStyleRule' :: StylesheetOpts -> QualifiedRule -> QualifiedRule
normalizeStyleRule' opts (QualifiedRule prelude declarations) =
  QualifiedRule prelude' declarations'
  where
    prelude' = if sortSelectors opts then normalizeSelectorsGroup prelude else prelude
    declarations' = if sortProperties opts then sortBy compareKeys normalizedDeclarations else normalizedDeclarations
    -- sortBy compareKeys (fmap normalizeOneDeclaration declarations)
    normalizedDeclarations = fmap normalizeOneDeclaration' declarations
    compareKeys (Declaration k1 _) (Declaration k2 _) = k1 `compare` k2
    normalizeOneDeclaration' = if addLeadingZeros opts then normalizeOneDeclaration else id
    normalizeOneDeclaration (Declaration key values) =
      let values' = UnsafeBalanced $ fmap normalizeToken (unBalanced values)
       in Declaration key values'

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
normalizeToken = mapNumericToken (addLeadingZero . stripPlusSign)

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

-- | Strip leading plus sign from numeric literals
stripPlusSign :: Text -> Text
stripPlusSign ts = case ts of
  '+' :. ts' -> ts'
  _ -> ts

-- | Normalize a group of selectors by sorting its' constituent selectors.
-- The order in which selectors appear in a comma separated group of selectors
-- does not change the semantics of a selection.
normalizeSelectorsGroup :: [Selector] -> [Selector]
normalizeSelectorsGroup = fmap Selector . sort . fmap unSelector

normalizeAtRule' :: StylesheetOpts -> AtRule -> AtRule
normalizeAtRule' opts (Media rule) = Media (normalizeMediaRule' opts rule)
normalizeAtRule' _opts other = other

normalizeAtRule :: AtRule -> AtRule
normalizeAtRule (Media rule) = Media (normalizeMediaRule rule)
normalizeAtRule other = other

normalizeMediaRule' :: StylesheetOpts -> MediaRule -> MediaRule
normalizeMediaRule' opts (MediaRule prelude stylesheet) =
  MediaRule prelude (normalizeStylesheet' opts stylesheet)

-- | Normalize a @media rule by normalizing its' inner stylesheet.
normalizeMediaRule :: MediaRule -> MediaRule
normalizeMediaRule (MediaRule prelude stylesheet) =
  MediaRule prelude (normalizeStylesheet stylesheet)
