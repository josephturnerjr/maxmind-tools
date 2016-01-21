{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.MaxMind
  (MaxMindIPSearch(..), IPDetails, maxMindIPSearch, asnLookup) where

import Text.Read
import Data.Word
import qualified Data.Map as M
import Data.Char
import Data.Maybe
import Data.IP (IPv4, IPv4RangeSegment(..))
import Data.IPLookup
import Data.MaxMind.City (Location, cityLookup)
import Data.MaxMind.Asn (ASN, asnLookup)
import Data.Aeson
import Data.Aeson.TH


data MaxMindIPSearch = MaxMindIPSearch
  {
    lookupIP :: !(IPv4 -> IPDetails)
  }

data IPDetails = IPDetails
  {
    location :: Maybe Location,
    asn :: Maybe ASN
  } deriving (Show)

instance ToJSON IPDetails where
  toJSON (IPDetails l a) = object ["location" .= l, "asn" .= a]

maxMindIPSearch :: IO MaxMindIPSearch
maxMindIPSearch = do
  asnL <- {-# SCC "asnlookup" #-}asnLookup ".data/GeoASN.csv"
  putStrLn "Done with asnL"
  cityL <- {-# SCC "citylookup" #-}cityLookup ".data/GeoCity/GeoLite2-City-Locations-en.csv" ".data/GeoCity/GeoLite2-City-Blocks-IPv4.csv"
  putStrLn "Done with cityL"
  return $ MaxMindIPSearch $ \ip ->
    IPDetails {location = (fmap loc (findIP cityL ip)), asn = (findIP asnL ip)} where
      loc (IPv4RangeSegment _ a) = a 
