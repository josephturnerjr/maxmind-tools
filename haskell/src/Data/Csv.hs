{-# LANGUAGE OverloadedStrings #-}
module Data.Csv
  (parseCSV, readCSVFile)
  where

import Text.ParserCombinators.Parsec
import System.IO

data HandleHeader = DiscardHeader | KeepHeader

parseCSV :: String -> Maybe [[String]]
parseCSV s = case parse parseLines "CSV Parser" s of
                     (Right b) -> Just $! b
                     (Left err) -> Nothing

parseLines = parseLine `endBy` (char '\n') <* eof

parseLine = chainl field comma []

comma = char ',' >> (return $! (++))

field = (:[]) <$> ((try quotedField) <|> unquotedField)
unquotedField = many (noneOf ",\n")
quotedField = char '"' *> (many (try escapedQuote <|> noneOf "\"")) <* char '"'

escapedQuote = char '"' *> char '"'

readCSVFile :: FilePath -> IO (Maybe [[String]])
readCSVFile path = do
  h <- openFile path ReadMode 
  hSetEncoding h latin1
  lines <- hGetContents h
  return $! parseCSVFile DiscardHeader lines

parseCSVFile :: HandleHeader -> String -> Maybe [[String]]
parseCSVFile DiscardHeader ls = do
  (headers:fieldLines) <- parseCSV ls
  return $! fieldLines
parseCSVFile KeepHeader ls = parseCSV ls
