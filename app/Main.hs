import Data.CSS.Syntax.Tokens (tokenize)
import qualified Data.Text.IO as T
import Normalize
import Parse
import Render
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty, parse)
import Types

main :: IO ()
main = do
  args <- getArgs
  let maybeFilename = safeHead args
  maybe (putStrLn "usage: app <filename>") processFile maybeFilename
  where
    processFile :: FilePath -> IO ()
    processFile fname = do
      parsedStylesheet <- parseFromFile parseStylesheet fname
      let normalized = fmap normalizeStylesheet parsedStylesheet
      either (putStrLn . errorBundlePretty) displayStylesheet normalized
    parseFromFile p file = parse p file <$> tokensFromFile file
    tokensFromFile file = tokenize <$> T.readFile file
    displayStylesheet :: Stylesheet -> IO ()
    displayStylesheet = T.putStrLn . renderStylesheet 0

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _xs) = Just x
