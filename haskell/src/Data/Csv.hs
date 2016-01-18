{-# LANGUAGE OverloadedStrings #-}
module Data.Csv
  (parseCSV, readCSVFile)
  where

import Text.ParserCombinators.Parsec
import System.IO
import Data.Maybe

data HandleHeader = DiscardHeader | KeepHeader

parseCSV :: String -> [[String]]
parseCSV = (mapMaybe parseCSVLine) . lines

parseCSVLine :: String -> Maybe [String]
parseCSVLine s = case parse parseLine "CSV Parser" s of
                     (Right b) -> Just $! b
                     (Left err) -> Nothing

parseLine = chainl field comma [] <* eof

comma = char ',' >> (return $ (++))

field = (:[]) <$> ((try quotedField) <|> unquotedField)
unquotedField = many (noneOf ",\n")
quotedField = char '"' *> (many (try escapedQuote <|> noneOf "\"")) <* char '"'

escapedQuote = char '"' *> char '"'

readCSVFile :: FilePath -> IO [[String]]
readCSVFile path = do
  h <- openFile path ReadMode 
  hSetEncoding h latin1
  lines <- hGetContents h
  return $! parseCSVFile DiscardHeader lines

parseCSVFile :: HandleHeader -> String -> [[String]]
parseCSVFile DiscardHeader ls = fieldLines where
  (headers:fieldLines) = parseCSV ls
parseCSVFile KeepHeader ls = parseCSV ls
