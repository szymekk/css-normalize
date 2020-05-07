{-# LANGUAGE OverloadedStrings #-}

import Data.CSS.Syntax.Tokens
import Parse
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Parse"
    $ describe "parse tokens"
    $ do
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
  where
    testParse p = parse p ""
    ws = Whitespace
    i = Ident
