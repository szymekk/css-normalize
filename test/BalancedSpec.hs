{-# LANGUAGE OverloadedStrings #-}

module BalancedSpec where

import Balanced
import Data.CSS.Syntax.Tokens
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

spec :: Spec
spec =
  describe "Balanced" $ do
    it "fails on empty input" $
      parse' someBalanced `shouldFailOn` t ""
    it "parses matching brackets" $ do
      parse' someBalanced `shouldSucceedOn` t "()"
      parse' someBalanced `shouldSucceedOn` t "{}"
      parse' someBalanced `shouldSucceedOn` t "[]"
      parse' someBalanced `shouldSucceedOn` t "([{}({}[])])"
    it "parses non-bracket tokens" $
      parse' someBalanced `shouldSucceedOn` t "some"
    it "parses matching brackets interleaved with non-bracket tokens" $ do
      parse' someBalanced `shouldSucceedOn` t "(some)"
      parse' someBalanced `shouldSucceedOn` t "([some])"
      parse' someBalanced `shouldSucceedOn` t "(some[nonsense]with)"
      parse' someBalanced `shouldSucceedOn` t "(some[nonsense]with)brackets"
      parse' someBalanced `shouldSucceedOn` t "(some[very{nice}nonsense]with)brackets"
      parse' someBalanced `shouldSucceedOn` t "a{b{c}b}a"
      parse' someBalanced' (t "a}b{c") `shouldParse` [Ident "a"]
    it "fails on unmatched brackets" $ do
      parse' someBalanced `shouldFailOn` t "}{"
      parse' someBalanced `shouldFailOn` t ")("
      parse' someBalanced `shouldFailOn` t "]["
      parse' someBalanced `shouldFailOn` t "[(])"
      parse' someBalanced `shouldFailOn` t "a[b(c]d)e"
      parse' someBalanced `shouldFailOn` t "body {k:v)}"
  where
    parse' p = parse p ""
    t = tokenize
    someBalanced' = fmap unBalanced someBalanced
