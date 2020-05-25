import CLI
import Control.Monad (join)
import Data.CSS.Syntax.Tokens (tokenize)
import Data.Text
import qualified Data.Text.IO as T
import Normalize
import Options.Applicative as A
import Parse
import Render
import Text.Megaparsec (errorBundlePretty, parse)
import Types

main :: IO ()
main =
  join . customExecParser (prefs showHelpOnError) $
    info
      (helper <*> parser)
      ( fullDesc
          <> header "CSS normalize - a tool for normalizing CSS files"
          <> progDesc "Normalize and pretty pring CSS files"
      )
  where
    parser :: A.Parser (IO ())
    parser =
      work <$> pInput

readInput :: Input -> (IO Text, FilePath)
readInput (FileInput file) = (T.readFile file, file)
readInput StdInput = (T.getContents, "<stdin>")

work :: Input -> IO ()
work input = do
  let (textIO, filename) = readInput input
  text <- textIO
  processFile text filename
  where
    processFile :: Text -> FilePath -> IO ()
    processFile text filename = do
      parsedStylesheet <- parseFromFile' parseStylesheet text filename
      let normalized = fmap normalizeStylesheet parsedStylesheet
      either (putStrLn . errorBundlePretty) displayStylesheet normalized
    parseFromFile' p text filename = return $ parse p filename (tokenize text)
    displayStylesheet :: Stylesheet -> IO ()
    displayStylesheet = T.putStrLn . renderStylesheet 0
