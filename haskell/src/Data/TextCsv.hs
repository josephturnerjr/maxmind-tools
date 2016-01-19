{-# LANGUAGE OverloadedStrings #-}
module Data.TextCsv
  (readCSVFile)
  where

import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import System.IO
import Data.Maybe

data HandleHeader = DiscardHeader | KeepHeader

parseCSVLine :: T.Text -> Maybe [T.Text]
parseCSVLine s = case parse parseLine "CSV Parser" s of
                     (Right b) -> Just b
                     (Left err) -> Nothing

parseLine :: Parser [T.Text]
parseLine = chainl field comma [] <* eof

comma = char ',' >> (return $ (++))

field = ((:[]) . T.pack) <$> ((try quotedField) <|> unquotedField)
unquotedField = many (noneOf ",\n")
quotedField = char '"' *> (many (try escapedQuote <|> noneOf "\"")) <* char '"'

escapedQuote = char '"' *> char '"'

readCSVFile :: FilePath -> IO [[T.Text]]
readCSVFile path = do
  h <- openFile path ReadMode 
  hSetEncoding h latin1
  contents <- TI.hGetContents h
  let lines = T.lines contents
  putStrLn "Contents got"
  getLine
  putStrLn $ show lines
  getLine
  return $! parseCSVFile DiscardHeader lines

parseCSVFile :: HandleHeader -> [T.Text] -> [[T.Text]]
parseCSVFile DiscardHeader (l:ls) = mapMaybe parseCSVLine ls
parseCSVFile KeepHeader ls = mapMaybe parseCSVLine ls
