{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.MaxMind.Asn
  (ASN, parseASNField, asnLookup) where

import Data.IP (IPv4RangeSegment(..), IPv4Range(..), IPv4(..))
import Data.Word
import Data.Aeson
import Data.Aeson.TH
import Data.TextCsv (readCSVFile)
import Data.Maybe
import Text.Read
import Data.Char
import Text.Parsec
import Text.Parsec.Text
import Control.Applicative ((<*>), (<*), (*>), (<$>))
import qualified Data.Text as T

type ASNLookup = [ASN]
type ASN = IPv4RangeSegment ASNDetails

data ASNDetails = ASNDetails
  {
    netOwner :: {-# UNPACK #-} !NetOwner,
    netAsn :: {-# UNPACK #-} !NetASN
  } deriving (Show)

type NetOwner = T.Text
type NetASN = T.Text

asnLookup :: FilePath -> IO ASNLookup
asnLookup f = do
  fieldLines <- readCSVFile f
  return $! mapMaybe parseASNFields fieldLines

parseASNFields :: [T.Text] -> Maybe ASN
parseASNFields [startF, endF, asnF] = do
  start <- readMaybe (T.unpack startF) :: Maybe Word32
  end <- readMaybe (T.unpack endF) :: Maybe Word32
  asn <- parseASNField asnF
  return $! IPv4RangeSegment (IPv4Range (IPv4 start) (IPv4 end)) asn

parseASNField :: T.Text -> Maybe ASNDetails
parseASNField s = case parse parseASNField' "ASN Parser" s of
                     (Right b) -> Just $! b
                     (Left err) -> Nothing

parseASNField' = makeDetails <$> optionMaybe asn <*> (try spaces *> optionMaybe owner) <* eof
  where makeDetails asn owner = ASNDetails (fillEmpty asn) (fillEmpty owner)
        fillEmpty :: Maybe T.Text -> T.Text
        fillEmpty = fromMaybe ""

asn :: Parser T.Text
asn = do 
  string "AS"
  digits <- many digit
  return $ T.pack ("AS" ++ digits)

owner :: Parser T.Text
owner = T.pack <$> manyTill anyChar eof

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = (map toLower) . (drop 3)} ''ASNDetails)
