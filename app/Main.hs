import CommandLine (Command (..), Input (..), cssnParseCommand, cssnPrintHelpText)
import Data.CSS.Syntax.Tokens (tokenize)
import Data.Text
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import Normalize
import Parse
import Paths_css_normalize (version)
import Render
import System.IO
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
  cmd <- cssnParseCommand
  case cmd of
    VersionCommand -> putStrLn $ showVersion version
    RunCommand input -> processInput input

processInput :: Input -> IO ()
processInput (FileInput filename) =
  T.putStrLn . transformStylesheet' =<< T.readFile filename
  where
    transformStylesheet' = transformStylesheet filename
processInput StdInput = do
  isTerminalStdIn <- hIsTerminalDevice stdin
  if isTerminalStdIn
    then cssnPrintHelpText
    else T.interact transformStylesheet' *> putStrLn ""
  where
    transformStylesheet' = transformStylesheet "<stdin>"

transformStylesheet :: FilePath -> Text -> Text
transformStylesheet inputName inputText =
  let parsedStylesheet = parseFromText parseStylesheetEof inputName inputText
      normalized = fmap normalizeStylesheet parsedStylesheet
   in either renderParseError renderStylesheet' normalized
  where
    parseFromText p filename text = parse p filename (tokenize text)
    renderParseError = pack . errorBundlePretty
    renderStylesheet' = renderStylesheet 0
