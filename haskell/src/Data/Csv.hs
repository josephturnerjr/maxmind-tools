module Data.Csv
  (parseCSV, readCSVFile) where

import Text.ParserCombinators.Parsec
import System.IO

parseCSV :: String -> Maybe [[String]]
parseCSV s = case parse parseLines "CSV Parser" s of
                     (Right b) -> Just b
                     (Left err) -> Nothing

parseLines = parseLine `endBy` (char '\n') <* eof

parseLine = chainl field comma []

comma = char ',' >> return (++)

field = (:[]) <$> ((try quotedField) <|> unquotedField)
unquotedField = many (noneOf ",\n")
quotedField = char '"' *> (many (noneOf "\"")) <* char '"'

readCSVFile :: FilePath ->  IO (Maybe [[String]])
readCSVFile path = do
  h <- openFile path ReadMode 
  hSetEncoding h latin1
  lines <- hGetContents h
  return $ parseCSV lines
