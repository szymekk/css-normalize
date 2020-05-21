{-# LANGUAGE OverloadedStrings #-}

import Data.CSS.Syntax.Tokens (Token, tokenize)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Normalize
import Parse
import Render
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
  args <- getArgs
  let source :: Either String (IO Text)
      source = T.readFile <$> safeHead args
      tokens :: Either String (IO [Token])
      tokens = fmap (fmap tokenize) source
  case tokens of
    Right ts ->
      either putStrLn T.putStrLn . parseAndNormalizeAndRender =<< ts
    Left _ -> putStrLn "usage: app <filename>"
  where
    pElem :: Parser a -> [Token] -> Either String a
    pElem p ts = case parse p "" ts of
      Right element -> Right element
      Left parseErr -> Left (errorBundlePretty parseErr)
    tryParseElemToText r p = fmap r . pElem p
    parseAndNormalizeAndRender :: [Token] -> Either String Text
    parseAndNormalizeAndRender = tryParseElemToText (renderStylesheet 0) (normalizeStylesheet <$> parseStylesheet)

safeHead :: [a] -> Either String a
safeHead [] = Left "empty list"
safeHead (x : _xs) = Right x
