{-# LANGUAGE OverloadedStrings #-}
module Data.Csv
  (readCSVFile)
  where

--import Text.Parsec
--import Text.Parsec.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO
import Data.Maybe
import Data.Attoparsec.ByteString.Char8 -- Text.Parsec
import qualified Data.Attoparsec.ByteString.Lazy as L --Text.Parsec.ByteString.Lazy
import Control.Applicative (Alternative, many, (<|>), (<*>), (<*), (*>), (<$>))

data HandleHeader = DiscardHeader | KeepHeader

parseCSVLine :: B.ByteString -> Maybe [B.ByteString]
parseCSVLine s = {-# SCC "parscsvline" #-} L.maybeResult $ L.parse parseLine s 

parseLine :: Parser [B.ByteString]
parseLine = chainl field comma [] <* endOfInput

comma = char ',' >> (return $ (++))

field = ((:[]) . B.pack) <$> ((try quotedField) <|> unquotedField)

unquotedField = many (satisfy (notInClass",\n"))

quotedField = char '"' *> (many (try escapedQuote <|> satisfy (notInClass "\""))) <* char '"'

escapedQuote = char '"' *> char '"'

chainl :: Alternative m => m a -> m (a -> a -> a) -> a -> m a
chainl p op x = chainl1 p op <|> pure x
{-# INLINE chainl #-}

chainl1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainl1 p op = scan where
  scan = flip id <$> p <*> rst
  rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id
{-# INLINE chainl1 #-}

readCSVFile :: FilePath -> IO [[B.ByteString]]
readCSVFile path = {-# SCC "readcsvfile" #-}do
  h <- openFile path ReadMode 
  hSetEncoding h latin1
  contents <- B.hGetContents h
  let lines = B.split '\n' contents
  return $! parseCSVFile DiscardHeader lines

parseCSVFile :: HandleHeader -> [B.ByteString] -> [[B.ByteString]]
parseCSVFile DiscardHeader (l:ls) = mapMaybe parseCSVLine ls
parseCSVFile KeepHeader ls = mapMaybe parseCSVLine ls
