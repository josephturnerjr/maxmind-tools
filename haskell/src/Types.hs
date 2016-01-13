{-# LANGUAGE OverloadedStrings #-}
module Types
  (
    IPv4, parseIPv4
  ) where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<*>), (<*), (*>), (<$>))
import Text.Read
import Data.Word
import Data.Bits

newtype IPv4 = IPv4 Word32 deriving (Show)

ipv4 :: String -> String -> String -> String -> Maybe IPv4
ipv4 o1 o2 o3 o4 = do
  o1' <- readMaybe o1 :: Maybe Word32
  o2' <- readMaybe o2 :: Maybe Word32
  o3' <- readMaybe o3 :: Maybe Word32
  o4' <- readMaybe o4 :: Maybe Word32
  return $ IPv4 $ (o1' `shift` 24) + (o2' `shift` 16) + (o3' `shift` 8) + o4'
  

parseIPv4:: String -> Maybe IPv4
parseIPv4 s = case parse parseIPv4' "IP Parser" s of
                     (Right b) -> b
                     (Left err) -> Nothing

parseIPv4' = ipv4 <$> (octet <* char '.') <*> (octet <* char '.') <*> (octet <* char '.') <*> octet <* eof

octet = (try twoOctet)
    <|> (try oneOctet)
    <|> (try zeroPadOne)
    <|> (try zeroPadTwo)
    <|> (try twoDigit)
    <|> (try oneDigit) where
  twoDigit = do
    first <- digit
    second <- digit
    return [first, second]
  oneDigit = do
    dig <- digit
    return [dig]
  twoOctet = do
    char '2'
    second <- oneOf ['0'..'5']
    third <- digit
    return ['2', second, third]
  oneOctet = do
    char '1'
    second <- digit
    third <- digit
    return ['1', second, third]
  zeroPadTwo = do
    char '0'
    d1 <- digit
    d2 <- digit
    return [d1, d2]
  zeroPadOne = do
    char '0'
    char '0'
    d <- digit
    return [d]
