{-# LANGUAGE OverloadedStrings #-}
module Data.Csv
  (readCSVFile)
  where

import Text.Parsec
import Text.Parsec.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO
import Data.Maybe

data HandleHeader = DiscardHeader | KeepHeader

parseCSVLine :: B.ByteString -> Maybe [B.ByteString]
parseCSVLine s = case parse parseLine "CSV Parser" s of
                     (Right b) -> Just b
                     (Left err) -> Nothing

parseLine :: Parser [B.ByteString]
parseLine = chainl field comma [] <* eof

comma = char ',' >> (return $ (++))

field = ((:[]) . B.pack) <$> ((try quotedField) <|> unquotedField)
unquotedField = many (noneOf ",\n")
quotedField = char '"' *> (many (try escapedQuote <|> noneOf "\"")) <* char '"'

escapedQuote = char '"' *> char '"'

readCSVFile :: FilePath -> IO [[B.ByteString]]
readCSVFile path = do
  h <- openFile path ReadMode 
  hSetEncoding h latin1
  contents <- B.hGetContents h
  let lines = B.split '\n' contents
  return $! parseCSVFile DiscardHeader lines

parseCSVFile :: HandleHeader -> [B.ByteString] -> [[B.ByteString]]
parseCSVFile DiscardHeader (l:ls) = mapMaybe parseCSVLine ls
parseCSVFile KeepHeader ls = mapMaybe parseCSVLine ls
