{-# LANGUAGE OverloadedStrings #-}

module NormalizeSpec where

import Data.CSS.Syntax.Tokens
import Normalize
import Test.Hspec hiding (Selector)
import Types

spec :: Spec
spec =
  describe "Normalize" $ do
    describe "style rule normalization" $ do
      let qualifiedRule = QualifiedRule [] (mkEmptyDecl <$> ["b", "a", "aa"])
      it "sorts declaration lists by keys" $
        normalizeStyleRule qualifiedRule `shouldBe` QualifiedRule [] (mkEmptyDecl <$> ["a", "aa", "b"])
      it "is idempotent" $
        normalizeStyleRule `isIdempotentOn` qualifiedRule
    describe "selectors group normalization" $ do
      let input :: [Selector]
          input = [Selector [Ident "c", ws, Ident "a", ws, Ident "b"], Selector [Ident "a"], Selector [Ident "c"], Selector [Ident "b"]]
          sorted :: [Selector]
          sorted = [Selector [Ident "a"], Selector [Ident "b"], Selector [Ident "c"], Selector [Ident "c", ws, Ident "a", ws, Ident "b"]]
      it "sorts selectors" $ do
        normalizeSelectorsGroup (Selector <$> [t "c a b", t "a", t "c", t "b"]) `shouldBe` (Selector <$> [t "a", t "b", t "c", t "c a b"])
        normalizeSelectorsGroup input `shouldBe` sorted
      it "is idempotent" $ do
        normalizeSelectorsGroup `isIdempotentOn` input
        normalizeSelectorsGroup `isIdempotentOn` sorted
        normalizeSelectorsGroup `isIdempotentOn` []
    describe "normalize tokens" $ do
      it "does not modify strings containing numeric values" $ do
        normalizeToken <$> t "\'.2\'" `shouldBe` [String ".2"]
        normalizeToken <$> t "\".2\"" `shouldBe` [String ".2"]
      it "does not modify normalized numeric values" $ do
        normalizeToken <$> t "0.1 +0.2 -0.3" `shouldBe` t "0.1 +0.2 -0.3"
        normalizeToken <$> t "1.1 +1.2 -1.3" `shouldBe` t "1.1 +1.2 -1.3"
      it "adds leading zero in numeric values" $ do
        normalizeToken <$> t ".1 +.2 -.3" `shouldBe` t "0.1 +0.2 -0.3"
        normalizeToken <$> t ".1% +.2% -.3%" `shouldBe` t "0.1% +0.2% -0.3%"
        normalizeToken <$> t ".1px +.2px -.3px" `shouldBe` t "0.1px +0.2px -0.3px"
        normalizeToken <$> t ".1" `shouldBe` [Number "0.1" (NVNumber 0.1)]
        normalizeToken <$> t "+.1" `shouldBe` [Number "+0.1" (NVNumber 0.1)]
        normalizeToken <$> t "-.1" `shouldBe` [Number "-0.1" (NVNumber (-0.1))]
        normalizeToken <$> t ".2%" `shouldBe` [Percentage "0.2" (NVNumber 0.2)]
        normalizeToken <$> t "+.2%" `shouldBe` [Percentage "+0.2" (NVNumber 0.2)]
        normalizeToken <$> t "-.2%" `shouldBe` [Percentage "-0.2" (NVNumber (-0.2))]
        normalizeToken <$> t ".3em" `shouldBe` [Dimension "0.3" (NVNumber 0.3) "em"]
        normalizeToken <$> t "+.3em" `shouldBe` [Dimension "+0.3" (NVNumber 0.3) "em"]
        normalizeToken <$> t "-.3em" `shouldBe` [Dimension "-0.3" (NVNumber (-0.3)) "em"]
      it "is idempotent" $ do
        normalizeToken `isIdempotentOn` Semicolon
        normalizeToken `isIdempotentOn` Colon
        normalizeToken `isIdempotentOn` Delim '!'
        normalizeToken `isIdempotentOn` LeftCurlyBracket
        fmap normalizeToken `isIdempotentOn` t "+.3em"
        fmap normalizeToken `isIdempotentOn` t "+0.3em"
        fmap normalizeToken `isIdempotentOn` t "+1.3em"
        fmap normalizeToken `isIdempotentOn` t "-.3em"
        fmap normalizeToken `isIdempotentOn` t "-0.3em"
        fmap normalizeToken `isIdempotentOn` t "-1.3em"
        fmap normalizeToken `isIdempotentOn` t ".3em"
        fmap normalizeToken `isIdempotentOn` t "0.3em"
        fmap normalizeToken `isIdempotentOn` t "1.3em"
  where
    ws = Whitespace
    mkEmptyDecl k = Declaration (Key k) mempty
    t = tokenize
    isIdempotentOn op input = op input `shouldBe` (op . op) input
