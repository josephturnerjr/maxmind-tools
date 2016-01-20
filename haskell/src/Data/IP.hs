{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.IP
  (
    IPv4(..), IPv4Range(..), IPv4RangeSegment(..), parseIPv4, parseCIDR, cmpRange, SegmentOrdering(..)
  ) where

import Data.Attoparsec.ByteString -- Text.Parsec
import Data.Attoparsec.ByteString.Lazy as L --Text.Parsec.ByteString.Lazy
import Control.Applicative ((<*>), (<*), (*>), (<$>))
import Text.Read
import Text.Show
import Data.Word
import Data.Bits
import Data.Aeson
import Data.Aeson.TH
import Data.List (intercalate)
import qualified Data.ByteString.Lazy.Char8 as B
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

data SegmentOrdering = LowerThan | Within | HigherThan deriving (Show, Eq)

$(deriveJSON defaultOptions ''IPv4)
$(deriveJSON defaultOptions ''IPv4Range)

instance ToJSON a => ToJSON (IPv4RangeSegment a) where
  toJSON (IPv4RangeSegment (IPv4Range start end) a) = object ["subnet" .= (object ["start" .= start, "end" .= end]), "details" .= a]

cmpRange :: IPv4Range -> IPv4 -> SegmentOrdering
cmpRange (IPv4Range start end) ip
  | ip < start = HigherThan
  | ip > end = LowerThan
  | ip >= start && ip <= end = Within

ipv4 :: String -> String -> String -> String -> Maybe IPv4
ipv4 o1 o2 o3 o4 = do
  o1' <- parseOctet o1 :: Maybe Word32
  o2' <- parseOctet o2 :: Maybe Word32
  o3' <- parseOctet o3 :: Maybe Word32
  o4' <- parseOctet o4 :: Maybe Word32
  return $! IPv4 $ (o1' `shift` 24) + (o2' `shift` 16) + (o3' `shift` 8) + o4' where
    parseOctet o = readMaybe o >>= validOctet
    validOctet o | o >= 0 && o <= 255 = Just o
                 | otherwise = Nothing

cidr :: Maybe IPv4 -> String -> Maybe IPv4Range
cidr mip netmask = do
  (IPv4 ip) <- mip
  netmask' <- parseNetmask netmask :: Maybe Int
  let mask = (0xffffffff `shift` (32 - netmask')) :: Word32
  let bottom = ip .&. mask
  let top = bottom + (complement mask)
  return $! IPv4Range (IPv4 bottom) (IPv4 top) where
    parseNetmask n = readMaybe n >>= validNetmask
    validNetmask n | n >= 0 && n <= 32 = Just n
                   | otherwise = Nothing
  
parseIPv4:: T.Text -> Maybe IPv4
parseIPv4 s = case L.parse (parseIPv4' <* eof) "IP Parser" (B.pack . T.unpack $ s) of
                     (Right b) -> b
                     (Left err) -> Nothing

parseCIDR:: B.ByteString -> Maybe IPv4Range
parseCIDR s = case L.parse parseCIDR' "IP Parser" s of
                     (Right b) -> b
                     (Left err) -> Nothing

parseIPv4' = ipv4 <$> (octet <* char '.') <*> (octet <* char '.') <*> (octet <* char '.') <*> octet
parseCIDR' = cidr <$> (parseIPv4' <* char '/') <*> netmask <* eof

octet = many digit
netmask = many digit
