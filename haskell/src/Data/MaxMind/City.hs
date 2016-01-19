{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.MaxMind.City
  (CityLookup, cityLookup, Location) where

import Data.IP (IPv4RangeSegment(..), parseCIDR)
import Data.Aeson
import Data.Aeson.TH
import Data.TextCsv (readCSVFile)
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
  putStrLn "done reading locations"
  getLine
  let m = M.fromList $ mapMaybe parseCityFields locFieldLines
  putStrLn "done parsing locations"
  getLine
  putStrLn $ show m
  putStrLn "done showing locations"
  getLine
  blockFieldLines <- readCSVFile blockF
  putStrLn "done reading blocks"
  getLine
  putStrLn $ show blockFieldLines
  putStrLn "done showing blocks"
  getLine
  let blockFields = mapMaybe (parseBlockFields m) blockFieldLines
  putStrLn "done showing blocks"
  getLine
  return $! blockFields

tReadMaybe :: Read a => T.Text -> Maybe a
tReadMaybe = readMaybe . T.unpack

parseCityFields:: [T.Text] -> Maybe (GeoID, LocationDetails)
parseCityFields (geoname_id:locale_code:continent_code:continent_name:country_iso_code:
                 country_name:s1_iso_code:s1_name:
                 s2_iso_code:s2_name:city_name:metro_code:time_zone:[]) = {-# SCC "parselocations" #-} do
  geoID <- tReadMaybe geoname_id :: Maybe Int
  return $! (geoID, LocationDetails {continent=p2m continent_code continent_name, country=p2m country_iso_code country_name, r1=p2m s1_iso_code s1_name, r2=p2m s2_iso_code s2_name, city=city_name})
  where p2m code name = CodedName code name

parseBlockFields :: M.Map GeoID LocationDetails -> [T.Text] -> Maybe LocationRange
parseBlockFields detailLookup (network:geoname_id:_:_:_:_:_:latitude:longitude:[]) = {-# SCC "parseblocks" #-} do
  range <- {-# SCC "parsecidr" #-} parseCIDR network
  geoID <- {-# SCC "parsegeo" #-}tReadMaybe geoname_id :: Maybe Int
  lat <- {-# SCC "parselat" #-}tReadMaybe latitude :: Maybe Double
  lon <- {-# SCC "parselon" #-}tReadMaybe longitude :: Maybe Double
  details <- {-# SCC "lookupdeets" #-}M.lookup geoID detailLookup
  return $! IPv4RangeSegment range $ Location {latitude=lat, longitude=lon, details=details}
