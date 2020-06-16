{-# LANGUAGE OverloadedStrings #-}

module SelectorsSpec where

import Balanced.Internal (Balanced(UnsafeBalanced))
import Data.CSS.Syntax.Tokens
import Data.List.NonEmpty
import Selectors
import Selectors.Types
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

spec :: Spec
spec =
  describe "Parse selectors" $ do
    describe "parse selectors" $
      it "parses valid selectors" $ do
        parse' parseSelector [i "a"]
          `shouldParse` Selector sssA []
        parse' parseSelector [i "a", ws, i "b"]
          `shouldParse` Selector sssA [(Space, sssB)]
        parse' parseSelector [i "a", Delim '+', i "b"]
          `shouldParse` Selector sssA [(Plus, sssB)]
        parse' parseSelector [i "a", ws, Delim '+', ws, i "b"]
          `shouldParse` Selector sssA [(Plus, sssB)]
        parse' parseSelector [i "a", Delim '+', i "b", Delim '~', Delim '*']
          `shouldParse` Selector sssA [(Plus, sssB), (Tilde, sssU)]
        parse' parseSelector [i "a", ws, Delim '+', ws, i "b", ws, Delim '~', ws, Delim '*']
          `shouldParse` Selector sssA [(Plus, sssB), (Tilde, sssU)]
    describe "parse simple selector sequences" $ do
      it "parses valid simple selector sequences" $ do
        parse' parseSSS [i "a"] `shouldParse` sssA
        parse' parseSSS [i "a", Hash HId "x", dot, i "c"]
          `shouldParse` sss (TypeSelector "a") [mkId "x", mkClass "c"]
      it "with attribute selectors" $ do
        let mkAttr :: AttrName -> Maybe AttrValueMatch -> SimpleSelector
            mkAttr name matcher = (CommonSimpleSelector . CommonAttribute) (Attribute name matcher)
        let mkAttrMatchVal matchType = mkAttr (AttrName "attr") $ Just (AttrValueMatch matchType $ AttrVal "val")
        let attrPrefixVal = mkAttrMatchVal Prefix
        let attrSuffixVal = mkAttrMatchVal Suffix
        let attrSubstringVal = mkAttrMatchVal Substring
        let attrEqualsVal = mkAttrMatchVal Equals
        let attrIncludeVal = mkAttrMatchVal Include
        let attrDashVal = mkAttrMatchVal Dash
        parse' parseSSS [d '*', opS, i "attr", clS]
          `shouldParse` sss Universal [mkAttr (AttrName "attr") Nothing]
        parse' parseSSS [d '*', opS, i "attr", PrefixMatch, i "val", clS]
          `shouldParse` sss Universal [attrPrefixVal]
        parse' parseSSS [d '*', opS, i "attr", SuffixMatch, i "val", clS]
          `shouldParse` sss Universal [attrSuffixVal]
        parse' parseSSS [d '*', opS, i "attr", SubstringMatch, i "val", clS]
          `shouldParse` sss Universal [attrSubstringVal]
        parse' parseSSS [d '*', opS, i "attr", Delim '=', i "val", clS]
          `shouldParse` sss Universal [attrEqualsVal]
        parse' parseSSS [d '*', opS, i "attr", IncludeMatch, i "val", clS]
          `shouldParse` sss Universal [attrIncludeVal]
        parse' parseSSS [d '*', opS, i "attr", DashMatch, i "val", clS]
          `shouldParse` sss Universal [attrDashVal]
        parse' parseSSS [d '*', opS, ws, i "attr", ws, DashMatch, ws, i "val", ws, clS]
          `shouldParse` sss Universal [attrDashVal]
      it "with pseudo" $ do
        let mkPseudoElement name = (CommonSimpleSelector . CommonPseudoElement) (PseudoElement $ IdentPseudo name)
        let mkPseudoClass name = (CommonSimpleSelector . CommonPseudoClass) (PseudoClass $ IdentPseudo name)
        let mkPseudoElementFn name ts = (CommonSimpleSelector . CommonPseudoElement) (PseudoElement $ FunctionalPseudo name (UnsafeBalanced ts))
        let mkPseudoClassFn name ts = (CommonSimpleSelector . CommonPseudoClass) (PseudoClass $ FunctionalPseudo name (UnsafeBalanced ts))
        let mkNotClass = Not . NegationCommon . CommonClass . Class
        let num2 = Number "2" (NVInteger 2)
        parse' parseSSS [d '*', Colon, Colon, i "before"]
          `shouldParse` sss Universal [mkPseudoElement "before"]
        parse' parseSSS [d '*', Colon, Colon, fn "slotted", d '*', clR]
          `shouldParse` sss Universal [mkPseudoElementFn "slotted" [d '*']]
        parse' parseSSS [d '*', Colon, i "active"]
          `shouldParse` sss Universal [mkPseudoClass "active"]
        parse' parseSSS [d '*', Colon, fn "nth-child", num2, clR]
          `shouldParse` sss Universal [mkPseudoClassFn "nth-child" [num2]]
        parse' parseSSS [d '*', Colon, fn "not", i "a", clR]
          `shouldParse` sss Universal [Not $ NegationTypeLike (TypeSelector "a")]
        parse' parseSSS [d '*', Colon, fn "not", d '*', clR]
          `shouldParse` sss Universal [Not $ NegationTypeLike Universal]
        parse' parseSSS [d '*', Colon, fn "not", dot, i "c", clR]
          `shouldParse` sss Universal [mkNotClass "c"]
        parse' parseSSS [d '*', Colon, fn "not", ws, dot, i "c", ws, clR]
          `shouldParse` sss Universal [mkNotClass "c"]
        parse' parseSSS [i "a", Hash HId "x", dot, i "c", Colon, fn "not", dot, i "c", clR]
          `shouldParse` sss (TypeSelector "a") [mkId "x", mkClass "c", mkNotClass "c"]
    describe "parse selector groups" $ do
      it "parses valid" $ do
        let mkGroup = SelectorsGroup . fromList
        let selA = Selector sssA []
        let selB = Selector sssB []
        parse' parseSelectorsGroup [i "a"] `shouldParse` mkGroup [selA]
        parse' parseSelectorsGroup [i "a", Comma, i "b"] `shouldParse` mkGroup [selA, selB]
        parse' parseSelectorsGroup [i "a", ws, Comma, ws, i "b"] `shouldParse` mkGroup [selA, selB]
  where
    ws = Whitespace
    i = Ident
    d = Delim
    parse' p = parse p ""
    clR = RightParen
    opS = LeftSquareBracket
    clS = RightSquareBracket
    fn = Function
    dot = Delim '.'
    parseSSS = parseSimpleSelectorSeq
    sss = SimpleSelectorSeq
    mkId = CommonSimpleSelector . CommonId . Id
    mkClass = CommonSimpleSelector . CommonClass . Class
    sssA = SimpleSelectorSeq (TypeSelector "a") []
    sssB = SimpleSelectorSeq (TypeSelector "b") []
    sssU = SimpleSelectorSeq Universal []
