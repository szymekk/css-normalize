{-# LANGUAGE OverloadedStrings #-}

module Render
  ( renderStylesheet,
  )
where

import Data.CSS.Syntax.Tokens as CSS
import Data.Text as T
import Parse

indentText :: Text
indentText = "    "

indent :: Int -> Text
indent n = T.replicate n indentText

renderDeclaration :: Int -> Declaration -> Text
renderDeclaration n (Declaration (Key key) ts) =
  indent n <> key <> ": " <> serialize ts <> ";"

renderQualifiedRule :: Int -> QualifiedRule -> Text
renderQualifiedRule n (QualifiedRule prelude declarations) =
  indent n <> preludeText <> " {\n"
    <> declarationsText
    <> "\n"
    <> indent n
    <> "}"
  where
    preludeText = serialize prelude
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

renderStylesheet :: Int -> Stylesheet -> Text
renderStylesheet n (Stylesheet elements) =
  intercalate "\n" (fmap (renderStylesheetElement n) elements)

renderStylesheetElement :: Int -> StylesheetElement -> Text
renderStylesheetElement n (StyleRule qualifiedRule) = renderQualifiedRule n qualifiedRule
renderStylesheetElement n (AtRule atRule) = renderAtRule n atRule
