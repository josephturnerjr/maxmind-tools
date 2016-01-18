{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.MaxMind.City
  (CityLookup, cityLookup, Location) where

import Data.IP (IPv4RangeSegment(..), parseCIDR)
import Data.Aeson
import Data.Aeson.TH
import Data.Csv (readCSVFile)
import qualified Data.Map.Strict as M
import Data.Maybe
import Text.Read
import qualified Data.Text as T

type GeoID = Int

type CityLookup = [LocationRange]
type LocationRange = IPv4RangeSegment Location

data Location = Location
  {
    details :: {-# UNPACK #-} !LocationDetails,
    latitude :: {-# UNPACK #-} !Latitude,
    longitude :: {-# UNPACK #-} !Longitude
  } deriving (Show)

type Latitude = Double
type Longitude = Double

data LocationDetails = LocationDetails
  {
    continent :: {-# UNPACK #-} !Continent,
    country :: {-# UNPACK #-} !Country,
    r1 :: {-# UNPACK #-} !Region,
    r2 :: {-# UNPACK #-} !Region,
    city :: {-# UNPACK #-} !City 
  } deriving (Show)

data CodedName = CodedName {-# UNPACK #-} !T.Text {-# UNPACK #-} !T.Text deriving (Show)
type Continent = CodedName
type Country = CodedName
type Region = CodedName
type City = T.Text

$(deriveJSON defaultOptions ''CodedName)
$(deriveJSON defaultOptions ''LocationDetails)
$(deriveJSON defaultOptions ''Location)

cityLookup :: FilePath -> FilePath -> IO CityLookup
cityLookup locF blockF = do
  -- yolo on partial
  locFieldLines <- readCSVFile locF
  blockFieldLines <- readCSVFile blockF
  let m = M.fromList $ mapMaybe parseCityFields locFieldLines
  return $! mapMaybe (parseBlockFields m) blockFieldLines

parseCityFields:: [String] -> Maybe (GeoID, LocationDetails)
parseCityFields (geoname_id:locale_code:continent_code:continent_name:country_iso_code:
                 country_name:s1_iso_code:s1_name:
                 s2_iso_code:s2_name:city_name:metro_code:time_zone:[]) = do
  geoID <- readMaybe geoname_id :: Maybe Int
  return $! (geoID, LocationDetails {continent=p2m continent_code continent_name, country=p2m country_iso_code country_name, r1=p2m s1_iso_code s1_name, r2=p2m s2_iso_code s2_name, city=T.pack city_name})
  where p2m code name = CodedName (T.pack code) (T.pack name)

parseBlockFields :: M.Map GeoID LocationDetails -> [String] -> Maybe LocationRange
parseBlockFields detailLookup (network:geoname_id:_:_:_:_:_:latitude:longitude:[]) = do
  range <- parseCIDR network
  geoID <- readMaybe geoname_id :: Maybe Int
  lat <- readMaybe latitude :: Maybe Double
  lon <- readMaybe longitude :: Maybe Double
  details <- M.lookup geoID detailLookup
  return $! IPv4RangeSegment range $ Location {latitude=lat, longitude=lon, details=details}
