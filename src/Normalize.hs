module Normalize where

import Data.List
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
    declarations' = sortBy compareKeys declarations
    compareKeys (Declaration k1 _) (Declaration k2 _) = k1 `compare` k2

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
