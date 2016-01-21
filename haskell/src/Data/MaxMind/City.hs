{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.MaxMind.City
  (CityLookup, cityLookup, Location) where

import Data.IP (IPv4RangeSegment(..), parseCIDR)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Aeson.Types as AT
import Data.Csv (readCSVFile)
import qualified Data.Map.Strict as M
import Data.Maybe
import Text.Read
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Short as BS (ShortByteString, toShort, fromShort, unpack)
import qualified Data.Text as T
import Debug.Trace
import Data.Char
import Data.IPLookup
import Data.List

type GeoID = B.ByteString

type CityLookup = IPLookup Location
type LocationRange = IPv4RangeSegment Location

data Location = Location
  {
    details :: !LocationDetails,
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

data CodedName = CodedName {-# UNPACK #-} !BS.ShortByteString {-# UNPACK #-} !BS.ShortByteString deriving (Show)
type Continent = CodedName
type Country = CodedName
type Region = CodedName
type City = BS.ShortByteString

instance ToJSON BS.ShortByteString where
  toJSON = AT.String . T.pack . (map (chr . fromEnum)) . BS.unpack

$(deriveToJSON defaultOptions ''CodedName)
$(deriveToJSON defaultOptions ''LocationDetails)
$(deriveToJSON defaultOptions ''Location)

cityLookup :: FilePath -> FilePath -> IO CityLookup
cityLookup locF blockF = do
  -- yolo on partial
  locFieldLines <- readCSVFile locF
  putStrLn "done reading locations"
  let m =  M.fromList $ mapMaybe parseCityFields locFieldLines
  putStrLn "done parsing locations"
  blockFieldLines <- readCSVFile blockF
  putStrLn "done reading blocks"
  let blockFields = mapMaybe (parseBlockFields m) blockFieldLines
  putStrLn "done doing blocks"
  return $! fromList blockFields

parseCityFields:: [B.ByteString] -> Maybe (GeoID, LocationDetails)
parseCityFields (geoname_id:locale_code:continent_code:continent_name:country_iso_code:
                 country_name:s1_iso_code:s1_name:
                 s2_iso_code:s2_name:city_name:metro_code:time_zone:[]) = {-# SCC "parselocations" #-} do
  return $! (geoname_id, LocationDetails {continent=p2m continent_code continent_name, country=p2m country_iso_code country_name, r1=p2m s1_iso_code s1_name, r2=p2m s2_iso_code s2_name, city=(f city_name)})
  where p2m code name = CodedName (f code) (f name)
        f = BS.toShort . B.toStrict
parseCityFields fs = trace ("Error in city fields: " ++ (show fs)) Nothing
{-# INLINE parseCityFields #-}

parseBlockFields :: M.Map GeoID LocationDetails -> [B.ByteString] -> Maybe LocationRange
parseBlockFields detailLookup (network:geoname_id:_:_:_:_:_:latitude:longitude:[]) = {-# SCC "parseblocks" #-} do
  range <- parseCIDR network
  lat <- tReadMaybe latitude :: Maybe Double
  lon <- tReadMaybe longitude :: Maybe Double
  details <- M.lookup geoname_id detailLookup
  return $! IPv4RangeSegment range $ Location {latitude=lat, longitude=lon, details=details}
parseBlockFields _ fs = trace ("Error in block fields: " ++ (show fs)) Nothing
{-# INLINE parseBlockFields #-}

tReadMaybe :: Read a => B.ByteString -> Maybe a
tReadMaybe = {-# SCC "treadmaybe" #-}readMaybe . B.unpack
{-# INLINE tReadMaybe #-}

