{-# LANGUAGE OverloadedStrings #-}

module ParseSpec where

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
        testParse parseDeclarationValues [i "a", Semicolon]
          `shouldParse` [i "a"]
        testParse parseDeclarationValues [i "a", Semicolon, ws]
          `shouldParse` [i "a"]
        testParse parseDeclarationValues [i "a", ws, Semicolon, ws]
          `shouldParse` [i "a"]
        testParse parseDeclarationValues [i "a", ws, i "b", Semicolon]
          `shouldParse` [i "a", ws, i "b"]
        testParse parseDeclarationValues [i "a", ws, i "b", ws, Semicolon]
          `shouldParse` [i "a", ws, i "b"]
      it "parses declarations" $ do
        testParse parseOneDeclaration [i "key", Colon, i "a", Semicolon]
          `shouldParse` Declaration (Key "key") [i "a"]
        testParse parseOneDeclaration [i "key", ws, Colon, ws, i "a", ws, Semicolon, ws]
          `shouldParse` Declaration (Key "key") [i "a"]
      it "parses qualified rule" $ do
        testParse parseQualifiedRule [i "body", curlyL, curlyR]
          `shouldParse` QualifiedRule [i "body"] []
        testParse parseQualifiedRule [i "body", curlyL, i "key", Colon, i "v", curlyR]
          `shouldParse` QualifiedRule [i "body"] [Declaration (Key "key") [i "v"]]
    describe "parse from text" $ do
      it "parses declaration" $ do
        let one = Number "1" (NVInteger 1)
            function = [Function "rgb", one, Comma, one, Comma, one, RightParen]
            parsedDeclaration = Declaration (Key "color") function
            parsedDeclaration' = Declaration (Key "color") (intersperse ws function)
        parseText parseOneDeclaration "color:rgb(1,1,1);" `shouldParse` parsedDeclaration
        parseText parseOneDeclaration "color : rgb( 1 , 1 , 1 ) ;" `shouldParse` parsedDeclaration'
      it "parses qualified rule" $ do
        testParse parseQualifiedRule `shouldFailOnText` ""
        testParse parseQualifiedRule `shouldFailOnText` " "
        parseText parseQualifiedRule "body{}" `shouldParse` QualifiedRule [i "body"] []
        parseText parseQualifiedRule "body{/*test*/}" `shouldParse` QualifiedRule [i "body"] []
        parseText parseQualifiedRule "*{k:v}" `shouldParse` QualifiedRule [d '*'] [Declaration (Key "k") [i "v"]]
        let parsedRule =
              QualifiedRule
                [i "a", ws, i "b"]
                [ Declaration
                    (Key "margin")
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
          `shouldParse` QualifiedRule [d '.', i "a"] []
        parseText parseQualifiedRule "a > b{}"
          `shouldParse` QualifiedRule [i "a", ws, d '>', ws, i "b"] []
        -- insert comments to prevent collapsing whitespace
        parseText parseQualifiedRule "a /**/ > /**/ b /**/ { /**/ } "
          `shouldParse` QualifiedRule [i "a", ws, ws, d '>', ws, ws, i "b"] []
        let parsedRule2 =
              QualifiedRule
                [i "a"]
                [ Declaration (Key "k1") [i "v1"],
                  Declaration (Key "k2") [i "v2"]
                ]
        parseText parseQualifiedRule "a{k1:v1;k2:v2} " `shouldParse` parsedRule2
        parseText parseQualifiedRule "a{k1:v1;k2:v2;} " `shouldParse` parsedRule2
        parseText parseQualifiedRule "a {k1 : v1 ; k2 : v2 } " `shouldParse` parsedRule2
        parseText parseQualifiedRule "a {k1 : v1 ; k2 : v2 ; } " `shouldParse` parsedRule2
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
      parse' (parseUnknownAtRule "name") `shouldSucceedOn` t " a b{k:v}"
      parse' (parseUnknownAtRule "name") `shouldSucceedOn` t " {}"
      parse' (parseUnknownAtRule "name") (t " a b{k:v}") `shouldParse` BlockAtRule "name" [ws, a, ws, b] [Ident "k", Colon, Ident "v"]
    describe "stylesheets" $ do
      it "parses valid stylesheets" $ do
        parse' parseStylesheet `shouldSucceedOn` t "body{k:v} @rule a b{ss}"
        parse' parseStylesheet `shouldSucceedOn` t "@rule a b{ss!;x} body{k:v}"
        parse' parseStylesheet `shouldSucceedOn` t "body{k:v} @rule a b{ss'xyz'}"
        parse' parseStylesheet `shouldSucceedOn` t "@rule a b{ss!;''} body{k:v a}"
        parse' parseStylesheet `shouldSucceedOn` t " body x {k1:v1;k2:v2} @rule{...} "
        parse' parseStylesheet `shouldSucceedOn` t " body x {k1:v1;k2:v2} @media{} "
        parse' parseStylesheet `shouldSucceedOn` t " body x {k1:v1;k2:v2} @media{ } "
      it "parses stylesheets containing function tokens" $ do
        parse' parseStylesheet `shouldSucceedOn` t "body {k:func(a,b,c)}"
        parse' parseStylesheet `shouldSucceedOn` t "@media{ body{ key: func() } } "
      it "fails on unbalanced brackets" $
        parse' parseStylesheet `shouldFailOn` t "body {k:v)}"
      it "fails on stylesheet with invalid media rule" $
        parse' parseStylesheet `shouldFailOn` t " body x {k1:v1;k2:v2} @media{...} "
      it "parses stylesheet" $
        parse' parseStylesheet (t "@media{}") `shouldParse` Stylesheet [AtRule $ Media $ MediaRule [] (Stylesheet [])]
    it "parses media rule" $ do
      parse' parseMediaRulePreludeBody `shouldSucceedOn` t "a b { body x {k1:v1;k2:v2} @rule{...} } "
      parse' parseMediaRulePreludeBody (t "a b { body x {k1:v1;k2:v2} @rule{...} } ")
        `shouldParse` MediaRule
          [a, ws, b]
          ( Stylesheet
              [ StyleRule $
                  QualifiedRule
                    [Ident "body", ws, Ident "x"]
                    [Declaration (Key "k1") [Ident "v1"], Declaration (Key "k2") [Ident "v2"]],
                AtRule $ BlockAtRule "rule" [] [dot, dot, dot]
              ]
          )
      parse' parseMediaRulePreludeBody `shouldFailOn` t "a b { body x {k1:v1;k2:v2} @media{...} }"
      parse' parseMediaRulePreludeBody (t "a b { body x {k1:v1;k2:v2} @rule {...} @media { } }")
        `shouldParse` MediaRule
          [a, ws, b]
          ( Stylesheet
              [ StyleRule $
                  QualifiedRule
                    [Ident "body", ws, Ident "x"]
                    [Declaration (Key "k1") [Ident "v1"], Declaration (Key "k2") [Ident "v2"]],
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
                        [Ident "body", ws, Ident "x"]
                        [Declaration (Key "k1") [Ident "v1"], Declaration (Key "k2") [Ident "v2"]],
                    AtRule $ BlockAtRule "rule" [] [dot, dot, dot]
                  ]
              )
      parse' parseMediaRule (t "@media a b{body x{k1:v1;k2:v2}@rule{...}}")
        `shouldParse` mediaResult
      parse' parseMediaRule (t "@media a b { body x { k1 : v1 ; k2 : v2 } @rule{...} } ")
        `shouldParse` mediaResult
  where
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
