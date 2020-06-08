{-# LANGUAGE OverloadedStrings #-}

module ParseSpec where

import Balanced (unBalanced)
import Balanced.Internal (Balanced (UnsafeBalanced))
import Data.CSS.Syntax.Tokens
import Data.List
import Parse
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Types

spec :: Spec
spec =
  describe "Parse" $ do
    describe "parse tokens" $ do
      it "parses property values" $ do
        let parseDeclarationValues' = fmap unBalanced parseDeclarationValues
        testParse parseDeclarationValues' [i "a", Semicolon]
          `shouldParse` [i "a"]
        testParse parseDeclarationValues' [i "a", Semicolon, ws]
          `shouldParse` [i "a"]
        testParse parseDeclarationValues' [i "a", ws, Semicolon, ws]
          `shouldParse` [i "a"]
        testParse parseDeclarationValues' [i "a", ws, i "b", Semicolon]
          `shouldParse` [i "a", ws, i "b"]
        testParse parseDeclarationValues' [i "a", ws, i "b", ws, Semicolon]
          `shouldParse` [i "a", ws, i "b"]
      it "parses declarations" $ do
        testParse parseOneDeclaration [i "key", Colon, i "a", Semicolon]
          `shouldParse` unsafeMkDecl "key" [i "a"]
        testParse parseOneDeclaration [i "key", ws, Colon, ws, i "a", ws, Semicolon, ws]
          `shouldParse` unsafeMkDecl "key" [i "a"]
      it "parses qualified rule" $ do
        testParse parseQualifiedRule [i "body", curlyL, curlyR]
          `shouldParse` QualifiedRule [Selector [i "body"]] []
        testParse parseQualifiedRule [i "body", curlyL, i "key", Colon, i "v", curlyR]
          `shouldParse` QualifiedRule [Selector [i "body"]] [unsafeMkDecl "key" [i "v"]]
    describe "parse from text" $ do
      it "parses declaration" $ do
        let one = Number "1" (NVInteger 1)
            function = [Function "rgb", one, Comma, one, Comma, one, RightParen]
            parsedDeclaration = unsafeMkDecl "color" function
            parsedDeclaration' = unsafeMkDecl "color" (intersperse ws function)
        parseText parseOneDeclaration "color:rgb(1,1,1);" `shouldParse` parsedDeclaration
        parseText parseOneDeclaration "color : rgb( 1 , 1 , 1 ) ;" `shouldParse` parsedDeclaration'
      it "parses qualified rule" $ do
        testParse parseQualifiedRule `shouldFailOnText` ""
        testParse parseQualifiedRule `shouldFailOnText` " "
        parseText parseQualifiedRule "body{}" `shouldParse` QualifiedRule [Selector [i "body"]] []
        parseText parseQualifiedRule "body{/*test*/}" `shouldParse` QualifiedRule [Selector [i "body"]] []
        parseText parseQualifiedRule "*{k:v}" `shouldParse` QualifiedRule [Selector [d '*']] [unsafeMkDecl "k" [i "v"]]
        let parsedRule =
              QualifiedRule
                [Selector [i "a", ws, i "b"]]
                [ unsafeMkDecl
                    "margin"
                    [ Dimension "30" (NVInteger 30) "px",
                      ws,
                      Number "0" (NVInteger 0)
                    ]
                ]
        parseText parseQualifiedRule "a b{margin:30px 0}" `shouldParse` parsedRule
        parseText parseQualifiedRule "a b{margin:30px 0;}" `shouldParse` parsedRule
        parseText parseQualifiedRule "a b { margin : 30px 0 ; } " `shouldParse` parsedRule
        parseText parseQualifiedRule "a b  {  margin  :  30px 0  ;  } " `shouldParse` parsedRule
        parseText parseQualifiedRule ".a {}"
          `shouldParse` QualifiedRule [Selector [d '.', i "a"]] []
        parseText parseQualifiedRule "a > b{}"
          `shouldParse` QualifiedRule [Selector [i "a", ws, d '>', ws, i "b"]] []
        -- insert comments to prevent collapsing whitespace
        parseText parseQualifiedRule "a /**/ > /**/ b /**/ { /**/ } "
          `shouldParse` QualifiedRule [Selector [i "a", ws, ws, d '>', ws, ws, i "b"]] []
        let parsedRule2 =
              QualifiedRule
                [Selector [i "a"]]
                [ unsafeMkDecl "k1" [i "v1"],
                  unsafeMkDecl "k2" [i "v2"]
                ]
        parseText parseQualifiedRule "a{k1:v1;k2:v2} " `shouldParse` parsedRule2
        parseText parseQualifiedRule "a{k1:v1;k2:v2;} " `shouldParse` parsedRule2
        parseText parseQualifiedRule "a {k1 : v1 ; k2 : v2 } " `shouldParse` parsedRule2
        parseText parseQualifiedRule "a {k1 : v1 ; k2 : v2 ; } " `shouldParse` parsedRule2
      it "parses multiple selectors" $ do
        let parsedRule3 =
              QualifiedRule
                [Selector [i "a"], Selector [i "b", ws, i "c"], Selector [i "d"]]
                []
        parseText parseQualifiedRule "a , b c , d {}" `shouldParse` parsedRule3
        parseText parseQualifiedRule "a,b c,d{}" `shouldParse` parsedRule3
    describe "parse selectors group" $ do
      it "parses selectors group" $ do
        let parseSelectorsGroup' = (fmap . fmap) unSelector parseSelectorsGroup
        parse' parseSelectorsGroup' (t "a") `shouldParse` [[i "a"]]
        parse' parseSelectorsGroup' (t " a ") `shouldParse` [[i "a"]]
        parse' parseSelectorsGroup' (t "a,b") `shouldParse` [[i "a"], [i "b"]]
        parse' parseSelectorsGroup' (t " a , b ") `shouldParse` [[i "a"], [i "b"]]
        parse' parseSelectorsGroup' (t "a b,c d") `shouldParse` [[i "a", ws, i "b"], [i "c", ws, i "d"]]
        parse' parseSelectorsGroup' (t "a b") `shouldParse` [[i "a", ws, i "b"]]
      it "fails on empty selectors group" $ do
        parse' parseSelectorsGroup `shouldFailOn` t ""
        parse' parseSelectorsGroup `shouldFailOn` t " "
        parse' parseSelectorsGroup `shouldFailOn` t "a,,b"
        parse' parseSelectorsGroup `shouldFailOn` t "a , , b"
        parse' parseSelectorsGroup `shouldFailOn` t " ,b"
        parse' parseSelectorsGroup `shouldFailOn` t "a,"
      it "fails on selectors group containing invalid characters" $ do
        parse' (parseSelectorsGroup <* eof) `shouldFailOn` t "{"
        parse' (parseSelectorsGroup <* eof) `shouldFailOn` t "}"
        parse' (parseSelectorsGroup <* eof) `shouldFailOn` t "a }"
    describe "parse selector" $ do
      it "fails on empty selector" $ do
        parse' parseSelector `shouldFailOn` t ""
        parse' parseSelector `shouldFailOn` t " "
      it "parses valid selector" $ do
        let parseSelector' = fmap unSelector parseSelector
        parse' parseSelector' (t "a") `shouldParse` [i "a"]
        parse' parseSelector' (t " a ") `shouldParse` [i "a"]
        parse' parseSelector' (t " a+b ") `shouldParse` [i "a", d '+', i "b"]
        parse' parseSelector' (t " a > b ") `shouldParse` [i "a", ws, d '>', ws, i "b"]
        parse' parseSelector' (t "*") `shouldParse` [d '*']
        parse' parseSelector' (t " * ") `shouldParse` [d '*']
        parse' parseSelector' (t " #abc") `shouldParse` [Hash HId "abc"]
        parse' parseSelector' (t "#abc ") `shouldParse` [Hash HId "abc"]
    describe "parse blocks between curly brackets" $ do
      it "from tokens" $ do
        parse' parseBlockCurly `shouldFailOn` []
        parse' parseBlockCurly `shouldFailOn` [Comma, Comma, Comma]
        parse' parseBlockCurly `shouldFailOn` [close, open] -- reversed brackets no good.
        parse' parseBlockCurly `shouldFailOn` [open, a, open, a, a, a, close]
        parseInitial parseBlockCurly [open, a, open, a, a, a, close, close, close] `succeedsLeaving` [close]
        parseInitial parseBlockCurly [open, close] `succeedsLeaving` []
        parseInitial parseBlockCurly [open, close, close] `succeedsLeaving` [close]
        parse' parseBlockCurly `shouldSucceedOn` [open, a, a, a, close]
        parse' parseBlockCurly `shouldSucceedOn` [open, open, a, a, a, close, close]
        parse' parseBlockCurly `shouldSucceedOn` [open, a, a, open, a, close, a, a, close]
        parse' parseBlockCurly `shouldSucceedOn` [open, a, a, open, a, open, a, a, close, close, open, a, close, a, close]
        parse' parseBlockCurly `shouldSucceedOn` [open, a, a, open, a, close, a, a, close, a]
        parse' parseBlockCurly `shouldFailOn` [opR, opS, open, close, opR, open, close, opS, clS, clR, clS, clR]
        parse' parseBlockCurly `shouldSucceedOn` [open, opR, opS, clS, open, opS, clS, opR, clR, close, clR, close]
        parse' parseBlockCurly `shouldSucceedOn` [open, close, opR, open, close, opS, clS, clR]
      it "from Text" $ do
        parse' parseBlockCurly `shouldFailOn` t ""
        parse' parseBlockCurly `shouldFailOn` t "abc"
        parse' parseBlockCurly `shouldFailOn` t "}{" -- reversed brackets no good.
        parse' parseBlockCurly `shouldFailOn` t "{a{abc}"
        parseInitial parseBlockCurly (t "{a{abc}}}") `succeedsLeaving` t "}"
        parseInitial parseBlockCurly (t "{a(abc)}}") `succeedsLeaving` t "}"
        parseInitial parseBlockCurly (t "{}") `succeedsLeaving` t ""
        parseInitial parseBlockCurly (t "{}}") `succeedsLeaving` t "}"
        parseInitial parseBlockCurly (t "{}function(") `succeedsLeaving` t "function("
        parse' parseBlockCurly `shouldSucceedOn` t "{some}"
        parse' parseBlockCurly `shouldSucceedOn` t "{[some]}"
        parse' parseBlockCurly `shouldSucceedOn` t "{func(some)}"
        parse' parseBlockCurly `shouldSucceedOn` t "{some[nonsense]with}"
        parse' parseBlockCurly `shouldSucceedOn` t "{some func(nonsense)with}"
        parse' parseBlockCurly `shouldSucceedOn` t "{some[no[nsense]](w)ith}"
        parse' parseBlockCurly `shouldSucceedOn` t "{some[no[nsense]]func(w)ith}"
        parse' parseBlockCurly `shouldSucceedOn` t "{some[nonsense]with}brackets"
        parse' parseBlockCurly `shouldSucceedOn` t "{some func(nons[e]nse)wi[t]h}brackets"
        parse' parseBlockCurly `shouldFailOn` t "([{}({}[])])"
        parse' parseBlockCurly `shouldFailOn` t "func([{}({}[])])"
        parse' parseBlockCurly `shouldSucceedOn` t "{([]{[]()})}"
        parse' parseBlockCurly `shouldSucceedOn` t "{([]{func()()})}"
        parse' parseBlockCurly `shouldSucceedOn` t "{}({}[])"
        parse' parseBlockCurly `shouldSucceedOn` t "{}(func({})[])"
    it "parses unknown at-rules" $ do
      parse' parseUnknownAtRule `shouldSucceedOn` t "@name a b{k:v}"
      parse' parseUnknownAtRule `shouldSucceedOn` t "@name {}"
      parse' parseUnknownAtRule (t "@name a b{k:v}") `shouldParse` BlockAtRule "name" [ws, a, ws, b] [Ident "k", Colon, Ident "v"]
      parse' parseUnknownAtRule `shouldSucceedOn` t "@name a;"
      parse' parseUnknownAtRule `shouldSucceedOn` t "@name a b ;"
      parse' parseUnknownAtRule (t "@name a;") `shouldParse` SemicolonAtRule "name" [ws, a]
      parse' parseUnknownAtRule (t "@name a b ;") `shouldParse` SemicolonAtRule "name" [ws, a, ws, b, ws]
    describe "stylesheets" $ do
      it "parses empty stylesheets" $ do
        parse' parseStylesheetEof (t "") `shouldParse` Stylesheet []
        parse' parseStylesheetEof (t " ") `shouldParse` Stylesheet []
      it "parses valid stylesheets" $ do
        parse' parseStylesheetEof `shouldSucceedOn` t "body{k:v} @rule a b{ss}"
        parse' parseStylesheetEof `shouldSucceedOn` t "@rule a b{ss!;x} body{k:v}"
        parse' parseStylesheetEof `shouldSucceedOn` t "body{k:v} @rule a b{ss'xyz'}"
        parse' parseStylesheetEof `shouldSucceedOn` t "@rule a b{ss!;''} body{k:v a}"
        parse' parseStylesheetEof `shouldSucceedOn` t " body x {k1:v1;k2:v2} @rule{...} "
        parse' parseStylesheetEof `shouldSucceedOn` t " body x {k1:v1;k2:v2} @media{} "
        parse' parseStylesheetEof `shouldSucceedOn` t " body x {k1:v1;k2:v2} @media{ } "
      it "parses stylesheets containing function tokens" $ do
        parse' parseStylesheetEof `shouldSucceedOn` t "body {k:func(a,b,c)}"
        parse' parseStylesheetEof `shouldSucceedOn` t "@media{ body{ key: func() } } "
      it "fails on unbalanced brackets" $
        parse' parseStylesheetEof `shouldFailOn` t "body {k:v)}"
      it "fails on stylesheet with invalid media rule" $
        parse' parseStylesheetEof `shouldFailOn` t " body x {k1:v1;k2:v2} @media{...} "
      it "parses stylesheet" $
        parse' parseStylesheetEof (t "@media{}") `shouldParse` Stylesheet [AtRule $ Media $ MediaRule [] (Stylesheet [])]
    it "parses media rule" $ do
      parse' parseMediaRule `shouldSucceedOn` t "@media a b { body x {k1:v1;k2:v2} @rule{...} } "
      parse' parseMediaRule (t "@media a b { body x {k1:v1;k2:v2} @rule{...} } ")
        `shouldParse` MediaRule
          [a, ws, b]
          ( Stylesheet
              [ StyleRule $
                  QualifiedRule
                    [Selector [Ident "body", ws, Ident "x"]]
                    [unsafeMkDecl "k1" [Ident "v1"], unsafeMkDecl "k2" [Ident "v2"]],
                AtRule $ BlockAtRule "rule" [] [dot, dot, dot]
              ]
          )
      parse' parseMediaRule `shouldFailOn` t "@media a b { body x {k1:v1;k2:v2} @media{...} }"
      parse' parseMediaRule (t "@media a b { body x {k1:v1;k2:v2} @rule {...} @media { } }")
        `shouldParse` MediaRule
          [a, ws, b]
          ( Stylesheet
              [ StyleRule $
                  QualifiedRule
                    [Selector [Ident "body", ws, Ident "x"]]
                    [unsafeMkDecl "k1" [Ident "v1"], unsafeMkDecl "k2" [Ident "v2"]],
                AtRule $ BlockAtRule "rule" [ws] [dot, dot, dot],
                (AtRule . Media) $ MediaRule [] (Stylesheet [])
              ]
          )
      let mediaResult =
            MediaRule
              [a, ws, b]
              ( Stylesheet
                  [ StyleRule $
                      QualifiedRule
                        [Selector [Ident "body", ws, Ident "x"]]
                        [unsafeMkDecl "k1" [Ident "v1"], unsafeMkDecl "k2" [Ident "v2"]],
                    AtRule $ BlockAtRule "rule" [] [dot, dot, dot]
                  ]
              )
      parse' parseMediaRule (t "@media a b{body x{k1:v1;k2:v2}@rule{...}}")
        `shouldParse` mediaResult
      parse' parseMediaRule (t "@media a b { body x { k1 : v1 ; k2 : v2 } @rule{...} } ")
        `shouldParse` mediaResult
  where
    unsafeMkDecl key ts = Declaration (Key key) (UnsafeBalanced ts)
    testParse p = parse p ""
    shouldFailOnText x text = shouldFailOn x (tokenize text)
    parseText p text = parse p "" (tokenize text)
    ws = Whitespace
    i = Ident
    d = Delim
    curlyL = LeftCurlyBracket
    curlyR = RightCurlyBracket
    parse' p = parse p ""
    t = tokenize
    open = LeftCurlyBracket
    close = RightCurlyBracket
    -- opC = LeftCurlyBracket
    -- clC = RightCurlyBracket
    opR = LeftParen
    clR = RightParen
    opS = LeftSquareBracket
    clS = RightSquareBracket
    a = Ident "a"
    b = Ident "b"
    dot = Delim '.'
    parseInitial p input = runParser' p (initialState input)
