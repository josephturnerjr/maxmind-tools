{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.MaxMind.City
  (CityLookup, cityLookup, Location) where

import Data.IP (IPv4RangeSegment, parseCIDR)
import Data.Aeson
import Data.Aeson.TH
import Data.Csv (readCSVFile)
import qualified Data.Map as M
import Data.Maybe

type GeoID = Int

type CityLookup = [LocationRange]
type LocationRange = IPv4RangeSegment Location

data Location = Location
  {
    locationDetails :: LocationDetails,
    latitude :: Latitude,
    longitude :: Longitude
  } deriving (Show)

type Latitude = Double
type Longitude = Double

data LocationDetails = LocationDetails
  {
    continent :: Continent,
    country :: Country,
    r1 :: Region,
    r2 :: Region,
    city :: City 
  } deriving (Show)

type Continent = Maybe (String, String)
type Country = Maybe (String, String)
type Region = Maybe (String, String)
type City = Maybe String

$(deriveJSON defaultOptions{omitNothingFields = True} ''LocationDetails)
$(deriveJSON defaultOptions{omitNothingFields = True} ''Location)

parseCityFields:: [String] -> Maybe (GeoID, Location)
parseCityFields [network, geoname_id, registered_country_geoname_id,
                 represented_country_geoname_id, is_anonymous_proxy,
                 is_satellite_provider, postal_code, latitude,
                 longitude] = do
  range <- parseCIDR network
  return (undefined, undefined)

cityLookup :: FilePath -> FilePath -> IO CityLookup
cityLookup blockF locF = do
  -- yolo on partial
  (Just locFieldLines) <- (readCSVFile locF)
  (Just blockFieldLines) <- (readCSVFile blockF)
  let m = M.fromList $ mapMaybe parseCityFields locFieldLines
  return $ mapMaybe (parseBlockFields m) blockFieldLines

parseBlockFields :: M.Map GeoID Location -> [String] -> Maybe LocationRange
parseBlockFields ls = undefined

