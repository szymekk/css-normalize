{-# LANGUAGE OverloadedStrings #-}

-- | Rendering stylesheets and CSS entities as text.
module Render
  ( renderStylesheet,
  )
where

import Balanced (unBalanced)
import Data.CSS.Syntax.Tokens as CSS
import Data.Text as T
import Types

-- | Basic unit of indentation.
indentText :: Text
indentText = "    "

-- | Generate leading indentation that should be prepended at the specified
-- level of indentation.
indent :: Int -> Text
indent n = T.replicate n indentText

renderDeclaration :: Int -> Declaration -> Text
renderDeclaration n (Declaration (Key key) bs) =
  indent n <> key <> ": " <> serialize (unBalanced bs) <> ";"

renderQualifiedRule :: Int -> QualifiedRule -> Text
renderQualifiedRule n (QualifiedRule prelude declarations) =
  indent n <> preludeText <> " {\n"
    <> declarationsText
    <> "\n"
    <> indent n
    <> "}"
  where
    preludeText = intercalate ", " (serialize . unSelector <$> prelude)
    declarationsText = intercalate "\n" (fmap (renderDeclaration (n + 1)) declarations)

renderAtRule :: Int -> AtRule -> Text
renderAtRule n atRule = case atRule of
  (BlockAtRule name prelude blockTokens) -> renderBlockAtRule n name prelude blockTokens
  (SemicolonAtRule name ts) -> renderSemicolonAtRule n name ts
  Media (MediaRule prelude stylesheet) -> renderMediaRule n prelude stylesheet
  where
    renderBlockAtRule m name prelude blockTokens =
      indent m <> "@" <> name <> serialize prelude <> "{" <> serialize blockTokens <> "}"
    renderSemicolonAtRule m name ts =
      indent m <> "@" <> name <> serialize ts <> ";"
    renderMediaRule m prelude stylesheet =
      indent m <> "@media " <> serialize prelude <> "{\n"
        <> renderStylesheet (m + 1) stylesheet
        <> "\n"
        <> indent m
        <> "}"

-- | Render a stylesheet at a specified indentation level.
renderStylesheet ::
  -- | indentation level
  Int ->
  -- | stylesheet to render
  Stylesheet ->
  Text
renderStylesheet n (Stylesheet elements) =
  intercalate "\n" (fmap (renderStylesheetElement n) elements)

renderStylesheetElement :: Int -> StylesheetElement -> Text
renderStylesheetElement n (StyleRule qualifiedRule) = renderQualifiedRule n qualifiedRule
renderStylesheetElement n (AtRule atRule) = renderAtRule n atRule
