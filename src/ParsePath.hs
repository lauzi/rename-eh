module ParsePath (Title(..), parsePath) where

import Data.Char (isSpace)
import Text.Regex.Applicative

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

parsePath :: String -> Maybe Title
parsePath path = path =~ filePathRE
