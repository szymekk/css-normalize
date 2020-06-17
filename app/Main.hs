import CommandLine
import Data.CSS.Syntax.Tokens (tokenize)
import Data.Text
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import Normalize
import Options.Applicative
import Parse
import Paths_css_normalize (version)
import Render
import System.IO
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
  cmd <- customExecParser pPrefs pInfo
  case cmd of
    VersionCommand -> putStrLn $ showVersion version
    RunCommand input -> processInput input

pPrefs :: ParserPrefs
pPrefs = prefs showHelpOnError

pInfo :: ParserInfo (Command Input)
pInfo =
  info
    (helper <*> pCommand)
    ( fullDesc
        <> header "CSS normalize - a tool for normalizing CSS files"
        <> progDesc
          "Normalize and pretty print a CSS stylesheet read from FILE. \
          \If no FILE, read standard input."
    )

processInput :: Input -> IO ()
processInput (FileInput filename) =
  T.putStrLn . transformStylesheet' =<< T.readFile filename
  where
    transformStylesheet' = transformStylesheet filename
processInput StdInput = do
  isTerminalStdIn <- hIsTerminalDevice stdin
  if isTerminalStdIn
    then
      handleParseResult . Failure $
        parserFailure pPrefs pInfo ShowHelpText mempty
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
