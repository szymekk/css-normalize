module Normalize
  ( normalizeStylesheet,
  )
where

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

-- | Normalize a style rule by sorting its' declarations by keys.
normalizeStyleRule :: QualifiedRule -> QualifiedRule
normalizeStyleRule (QualifiedRule prelude declarations) =
  QualifiedRule prelude (sortBy compareKeys declarations)
  where
    compareKeys (Declaration k1 _) (Declaration k2 _) = k1 `compare` k2

normalizeAtRule :: AtRule -> AtRule
normalizeAtRule (Media rule) = Media (normalizeMediaRule rule)
normalizeAtRule other = other

-- | Normalize a @media rule by normalizing its' inner stylesheet.
normalizeMediaRule :: MediaRule -> MediaRule
normalizeMediaRule (MediaRule prelude stylesheet) =
  MediaRule prelude (normalizeStylesheet stylesheet)
