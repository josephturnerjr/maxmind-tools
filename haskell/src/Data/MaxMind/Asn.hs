{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.MaxMind.Asn
  (ASN, parseASNField, asnLookup) where

import Data.IP (IPv4RangeSegment(..), IPv4Range(..), IPv4(..))
import Data.Word
import Data.Aeson
import Data.Aeson.TH
import Data.Csv (readCSVFile)
import Data.Maybe
import Text.Read
import Data.Char
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import Control.Applicative ((<*>), (<*), (*>), (<$>))
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Short as BS (ShortByteString, toShort, unpack)
import qualified Data.Aeson.Types as AT
import qualified Data.Text as T
import Data.IPLookup

import Debug.Trace

type ASNLookup = IPLookup ASNDetails
type ASN = IPv4RangeSegment ASNDetails

data ASNDetails = ASNDetails
  {
    netOwner :: {-# UNPACK #-} !NetOwner,
    netAsn :: {-# UNPACK #-} !NetASN
  } deriving (Show)

type NetOwner = BS.ShortByteString
type NetASN = BS.ShortByteString

instance ToJSON BS.ShortByteString where
  toJSON = AT.String . T.pack . (map (chr . fromEnum)) . BS.unpack

$(deriveToJSON defaultOptions ''ASNDetails)

asnLookup :: FilePath -> IO ASNLookup
asnLookup f = do
  fieldLines <- readCSVFile f
  let els = mapMaybe parseASNFields fieldLines
  return $! fromList els

parseASNFields :: [B.ByteString] -> Maybe ASN
parseASNFields [startF, endF, asnF] = do
  start <- readMaybe (B.unpack startF) :: Maybe Word32
  end <- readMaybe (B.unpack endF) :: Maybe Word32
  asn <- parseASNField asnF
  return $! IPv4RangeSegment (IPv4Range (IPv4 start) (IPv4 end)) asn
parseASNFields fs = trace ("Error with this: " ++ (show fs)) Nothing

parseASNField :: B.ByteString -> Maybe ASNDetails
parseASNField s = case parse parseASNField' "ASN Parser" s of
                     (Right b) -> Just $! b
                     (Left err) -> Nothing

parseASNField' = makeDetails <$> optionMaybe asn <*> (try spaces *> optionMaybe owner) <* eof
  where makeDetails asn owner = ASNDetails (fillEmpty asn) (fillEmpty owner)
        fillEmpty :: Maybe B.ByteString -> BS.ShortByteString
        fillEmpty =  BS.toShort . B.toStrict . fromMaybe "" 

asn :: Parser B.ByteString
asn = do 
  string "AS"
  digits <- many digit
  return $ B.pack ("AS" ++ digits)

owner :: Parser B.ByteString
owner = B.pack <$> manyTill anyChar eof
