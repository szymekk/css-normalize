{-# LANGUAGE OverloadedStrings #-}

import Data.CSS.Syntax.Tokens
import Data.List
import Parse
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

main :: IO ()
main = hspec spec

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
  where
    testParse p = parse p ""
    shouldFailOnText x text = shouldFailOn x (tokenize text)
    parseText p text = parse p "" (tokenize text)
    ws = Whitespace
    i = Ident
    d = Delim
    curlyL = LeftCurlyBracket
    curlyR = RightCurlyBracket
