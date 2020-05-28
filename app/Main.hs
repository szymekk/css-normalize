import CommandLine
import Control.Monad (join)
import Data.CSS.Syntax.Tokens (tokenize)
import Data.Text
import qualified Data.Text.IO as T
import Normalize
import Options.Applicative as A
import Parse
import Render
import System.IO
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = join $ customExecParser pPrefs pInfo

pPrefs :: ParserPrefs
pPrefs = prefs showHelpOnError

pInfo :: ParserInfo (IO ())
pInfo =
  info
    (helper <*> parser)
    ( fullDesc
        <> header "CSS normalize - a tool for normalizing CSS files"
        <> progDesc
          "Normalize and pretty print a CSS stylesheet read from FILE. \
          \If no FILE, read standard input."
    )
  where
    parser :: A.Parser (IO ())
    parser = processInput <$> pInput

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
    else T.interact transformStylesheet'
  where
    transformStylesheet' = transformStylesheet "<stdin>"

transformStylesheet :: FilePath -> Text -> Text
transformStylesheet inputName inputText =
  let parsedStylesheet = parseFromText parseStylesheet inputName inputText
      normalized = fmap normalizeStylesheet parsedStylesheet
   in either renderParseError renderStylesheet' normalized
  where
    parseFromText p filename text = parse p filename (tokenize text)
    renderParseError = pack . errorBundlePretty
    renderStylesheet' = renderStylesheet 0
