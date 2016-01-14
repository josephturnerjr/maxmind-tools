{-# LANGUAGE TemplateHaskell #-}
module Data.MaxMind
  (IPDetails, maxMindIPSearch) where

import Data.IP
import Text.Read
import Data.Word
import qualified Data.Map as M
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Csv
import Data.Maybe

details :: IPv4 -> IPDetails
details _ = IPDetails (Location (Just ("NA", "North America")) (Just ("US", "United States")) (Just ("SC", "South Carolina")) Nothing (Just "Greenville")) (ASN (Just "Google, Inc.") (Just "AS01234"))

data MaxMindIPSearch = MaxMindIPSearch ASNLookup CityLookup

maxMindIPSearch :: IO (IPv4 -> IPDetails)
maxMindIPSearch = do
  asnL <- asnLookup "../python/.data/GeoASN.csv"
  cityL <- cityLookup "../python/.data/GeoCity/GeoLite2-City-Locations-en.csv" "../python/.data/GeoCity/GeoLite2-City-Blocks-IPv4.csv"
  return $ lookupIP (MaxMindIPSearch asnL cityL)

lookupIP :: MaxMindIPSearch -> IPv4 -> IPDetails
lookupIP = undefined

parseASNFields :: [String] -> Maybe ASNDetails
parseASNFields [startF, endF, asnF] = do
  start <- readMaybe startF :: Maybe Word32
  end <- readMaybe endF :: Maybe Word32
  asn <- parseASNField asnF
  return $ IPv4RangeSegment (IPv4Range (IPv4 start) (IPv4 end)) asn

parseASNField :: String -> Maybe ASN
parseASNField = undefined

parseBlockFields :: M.Map GeoID Location -> [String] -> Maybe LocationDetails
parseBlockFields = undefined

parseCityFields:: [String] -> (GeoID, Location)
parseCityFields = undefined
  
type ASNLookup = [ASNDetails]
type CityLookup = [LocationDetails]

asnLookup :: FilePath -> IO ASNLookup
asnLookup f = do
  -- yolo on partial
  (Just fieldLines) <- (readCSVFile f)
  return $ mapMaybe parseASNFields fieldLines

cityLookup :: FilePath -> FilePath -> IO CityLookup
cityLookup blockF locF = do
  -- yolo on partial
  (Just locFieldLines) <- (readCSVFile locF)
  (Just blockFieldLines) <- (readCSVFile blockF)
  let m = M.fromList $ map parseCityFields locFieldLines
  return $ mapMaybe (parseBlockFields m) blockFieldLines

data Location = Location 
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

data IPBlock = IPBlock
  {
    location:: Location,
    latitude :: Latitude,
    longitude :: Longitude
  }

type Latitude = Double
type Longitude = Double

data ASN = ASN
  {
    netOwner :: NetOwner,
    netAsn :: NetASN
  } deriving (Show)

type NetOwner = Maybe String
type NetASN = Maybe String
data IPDetails = IPDetails
  {
    cityLocation :: Location,
    asn :: ASN
  } deriving (Show)

data IPv4RangeSegment a = IPv4RangeSegment IPv4Range a

type ASNDetails = IPv4RangeSegment ASN
type LocationDetails = IPv4RangeSegment IPBlock

type GeoID = Int

$(deriveJSON defaultOptions{omitNothingFields = True} ''Location)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = (map toLower) . (drop 3)} ''ASN)
$(deriveJSON defaultOptions{omitNothingFields = True} ''IPDetails)

