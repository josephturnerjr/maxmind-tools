{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.IP
  (
    IPv4(..), IPv4Range(..), IPv4RangeSegment(..), parseIPv4, parseCIDR, findIP
  ) where

import Text.Parsec
import Text.Parsec.Text
import Control.Applicative ((<*>), (<*), (*>), (<$>))
import Text.Read
import Text.Show
import Data.Word
import Data.Bits
import Data.Aeson
import Data.Aeson.TH
import Data.List (intercalate)
import qualified Data.Text as T

--data IPv4 = IPv4 {-# UNPACK #-} !Word32 deriving (Eq)
newtype IPv4 = IPv4 Word32 deriving (Eq)

instance Show IPv4 where
  show (IPv4 ip) = intercalate "." (map (show . (mask ip)) [24, 16, 8, 0]) where
    mask ip b = ((ip .&. (0xff `shift` b)) `shift` (-b))

instance Ord IPv4 where
  compare (IPv4 a) (IPv4 b) = compare a b

data IPv4Range = IPv4Range {-# UNPACK #-} !IPv4 {-# UNPACK #-} !IPv4 deriving (Show)
data IPv4RangeSegment a = IPv4RangeSegment {-# UNPACK #-} !IPv4Range !a deriving (Show)

data SegmentOrdering = LowerThan | Within | HigherThan

$(deriveJSON defaultOptions ''IPv4)
$(deriveJSON defaultOptions ''IPv4Range)

instance ToJSON a => ToJSON (IPv4RangeSegment a) where
  toJSON (IPv4RangeSegment (IPv4Range start end) a) = object ["subnet" .= (object ["start" .= start, "end" .= end]), "details" .= a]

cmpRange :: IPv4Range -> IPv4 -> SegmentOrdering
cmpRange (IPv4Range start end) ip
  | ip < start = HigherThan
  | ip > end = LowerThan
  | ip >= start && ip <= end = Within

findIP :: [IPv4RangeSegment a] -> IPv4 -> Maybe (IPv4RangeSegment a)
findIP (iprs@(IPv4RangeSegment range a):iprsss) ip = handleCmp (cmpRange range ip) where
  handleCmp Within = Just iprs
  handleCmp _ = findIP iprsss ip
findIP [] _ = Nothing

ipv4 :: String -> String -> String -> String -> Maybe IPv4
{-# INLINE ipv4 #-}
ipv4 o1 o2 o3 o4 = do
  o1' <- readMaybe o1 :: Maybe Word32
  o2' <- readMaybe o2 :: Maybe Word32
  o3' <- readMaybe o3 :: Maybe Word32
  o4' <- readMaybe o4 :: Maybe Word32
  return $! IPv4 $ (o1' `shift` 24) + (o2' `shift` 16) + (o3' `shift` 8) + o4'

cidr :: String -> String -> String -> String -> String -> Maybe IPv4Range
{-# INLINE cidr #-}
cidr o1 o2 o3 o4 netmask = do
  (IPv4 ip) <- {-# SCC "cidrip" #-}ipv4 o1 o2 o3 o4
  netmask' <- {-# SCC "cidrnetm" #-}readMaybe netmask :: Maybe Int
  let mask = {-# SCC "cidrmask" #-}(0xffffffff `shift` (32 - netmask')) :: Word32
  let bottom = {-# SCC "cidrbot" #-}ip .&. mask
  let top = {-# SCC "cidrtop" #-}bottom + (complement mask)
  return $! IPv4Range (IPv4 bottom) (IPv4 top)
  
parseIPv4:: T.Text -> Maybe IPv4
{-# INLINE parseIPv4 #-}
parseIPv4 s = case parse parseIPv4' "IP Parser" s of
                     (Right b) -> b
                     (Left err) -> Nothing

parseCIDR:: T.Text -> Maybe IPv4Range
{-# INLINE parseCIDR #-}
parseCIDR s = case parse parseCIDR' "IP Parser" s of
                     (Right b) -> b
                     (Left err) -> Nothing

parseIPv4' = ipv4 <$> (octet <* char '.') <*> (octet <* char '.') <*> (octet <* char '.') <*> octet <* eof
parseCIDR' = cidr <$> (octet <* char '.') <*> (octet <* char '.') <*> (octet <* char '.') <*> (octet <* char '/') <*> netmask <* eof

octet = (try twoOctet)
    <|> (try oneOctet)
    <|> (try zeroPadOne)
    <|> (try zeroPadTwo)
    <|> (try twoDigit)
    <|> (try oneDigit) where
  twoDigit = do
    first <- digit
    second <- digit
    return $! [first, second]
  oneDigit = do
    dig <- digit
    return $! [dig]
  twoOctet = do
    char '2'
    second <- oneOf ['0'..'5']
    third <- digit
    return $! ['2', second, third]
  oneOctet = do
    char '1'
    second <- digit
    third <- digit
    return $! ['1', second, third]
  zeroPadTwo = do
    char '0'
    d1 <- digit
    d2 <- digit
    return $! [d1, d2]
  zeroPadOne = do
    char '0'
    char '0'
    d <- digit
    return $! [d]

netmask = (try thirty) <|> (try twenty) <|> (try ten) <|> fmap (:[]) digit where
  thirty = do
    char '3'
    d <- oneOf ['0'..'2']
    return $! ['3', d]
  twenty = do
    char '2'
    d <- digit
    return $! ['2', d]
  ten = do
    char '1'
    d <- digit
    return $! ['2', d]
