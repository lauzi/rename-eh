import Data.Char (isSpace)
import Control.Monad (when, forM_)
import Text.Regex.Applicative
import Text.Printf

data Test = Test
  { testFilePath :: String
  , testAuthor :: String
  , testTitle :: String
  } deriving (Eq, Show)


parseTests :: [String] -> [Test]
parseTests (a : b : c : _ : xs) = Test a b c : parseTests xs
parseTests _ = []


-- regex = r'^.*?\[([^]]+)\]\s*([^([\s][^([]*)(?: - \[)?.*$'

fileNameRE, filePathRE :: RE Char Title
fileNameRE = Title <$> (few anySym *> bracket <* spaces) <*> title <* trash
  where
    spaces = many (psym isSpace)
    trash = spaces *> optional (string "- [") *> many anySym
    title = (++) <$> many notBracket <*> (return <$> notSpaceBracket)
    bracket = sym '[' *> many (psym (/= ']')) <* sym ']'
    notBracket = psym (\c -> c /= '[' && c /= '(')
    notSpaceBracket = psym (\c -> c /= '[' && c /= '(' && not (isSpace c))

filePathRE = many anySym *> sym '/' *> fileNameRE

data Title = Title
  { parsedAuthor :: String
  , parsedTitle :: String
  } deriving (Eq, Show)


runTest :: Test -> IO ()
runTest (Test filePath author title) = case filePath =~ filePathRE of
  Nothing -> printf "%s FAKD UP\n" filePath
  Just (Title author' title') -> do
    when (author /= author') $ printf "%s != %s QQ\n" author author'
    when (title /= title') $ printf "%s != %s QQ\n" title title'


main :: IO ()
main = do
  fileContent <- readFile "test2.txt"
  let tests = parseTests . lines $ fileContent
  forM_ tests runTest
