{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.IP
  (
    IPv4(..), IPv4Range(..), IPv4RangeSegment(..), parseIPv4, parseCIDR, randomIPv4
  ) where

import Text.Read
import Text.Show
import Data.Word
import Data.Bits
import Data.Aeson
import Data.Aeson.TH
import Data.List (intercalate)
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import System.Random

--data IPv4 = IPv4 {-# UNPACK #-} !Word32 deriving (Eq)
newtype IPv4 = IPv4 Word32 deriving (Eq)

instance Show IPv4 where
  show (IPv4 ip) = intercalate "." (map (show . (mask ip)) [24, 16, 8, 0]) where
    mask ip b = ((ip .&. (0xff `shift` b)) `shift` (-b))

instance Ord IPv4 where
  compare (IPv4 a) (IPv4 b) = compare a b

data IPv4Range = IPv4Range {-# UNPACK #-} !IPv4 {-# UNPACK #-} !IPv4 deriving (Show)
data IPv4RangeSegment a = IPv4RangeSegment {-# UNPACK #-} !IPv4Range !a deriving (Show)

$(deriveJSON defaultOptions ''IPv4)
$(deriveJSON defaultOptions ''IPv4Range)

instance ToJSON a => ToJSON (IPv4RangeSegment a) where
  toJSON (IPv4RangeSegment (IPv4Range start end) a) = object ["subnet" .= (object ["start" .= start, "end" .= end]), "details" .= a]

ipv4 :: [B.ByteString] -> Maybe IPv4
ipv4 (o1:o2:o3:o4:[]) = do
  o1' <- parseOctet o1 :: Maybe Word32
  o2' <- parseOctet o2 :: Maybe Word32
  o3' <- parseOctet o3 :: Maybe Word32
  o4' <- parseOctet o4 :: Maybe Word32
  return $! IPv4 $ (o1' `shift` 24) + (o2' `shift` 16) + (o3' `shift` 8) + o4' where
    parseOctet = fmap toEnum . valBS validOctet
    {-# INLINE parseOctet #-}
    validOctet o = o >= 0 && o <= 255
    {-# INLINE validOctet #-}
ipv4 _ = Nothing

cidr :: [B.ByteString] -> Maybe IPv4Range
cidr (o1:o2:o3:o4:netmask:[]) = do
  (IPv4 ip) <- ipv4 [o1,o2,o3,o4]
  netmask' <- parseNetmask netmask :: Maybe Int
  let mask = (0xffffffff `shift` (32 - netmask')) :: Word32
  let bottom = ip .&. mask
  let top = bottom + (complement mask)
  return $! IPv4Range (IPv4 bottom) (IPv4 top) where
    parseNetmask = valBS validNetmask
    {-# INLINE parseNetmask #-}
    validNetmask n = n >= 0 && n <= 32
    {-# INLINE validNetmask #-}
cidr _ = Nothing

valBS p s = B.readInt s >>= p' where
  p' (a, "") | p a = Just a
             | otherwise = Nothing
{-# INLINE valBS #-}
  
parseIPv4:: T.Text -> Maybe IPv4
parseIPv4 = parseIPv4' . B.pack . T.unpack

parseCIDR:: B.ByteString -> Maybe IPv4Range
parseCIDR = cidr . (B.splitWith delim) where
  delim a = a == '.' || a == '/'
{-# INLINE parseCIDR #-}

parseIPv4' = ipv4 . (B.split '.')
{-# INLINE parseIPv4' #-}

randomIPv4 :: IO IPv4
randomIPv4 = randomIO >>= (return . IPv4)
