{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.Trans.Either
import Data.Char
import Types
import qualified Data.Text as T


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

data ASN = ASN
  {
    netOwner :: NetOwner,
    netAsn :: NetASN
  } deriving (Show)

type NetOwner = Maybe String
type NetASN = Maybe String
data IPDetails = IPDetails
  {
    location :: Location,
    asn :: ASN
  } deriving (Show)

data IPv4RangeLookup a = IPv4RangeLookup Int Int a

instance FromText IPv4 where
  fromText = parseIPv4 . T.unpack

$(deriveJSON defaultOptions{omitNothingFields = True} ''Location)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = (map toLower) . (drop 3)} ''ASN)
$(deriveJSON defaultOptions{omitNothingFields = True} ''IPDetails)

details :: IPv4 -> IPDetails
details _ = IPDetails (Location (Just ("NA", "North America")) (Just ("US", "United States")) (Just ("SC", "South Carolina")) Nothing (Just "Greenville")) (ASN (Just "Google, Inc.") (Just "AS01234"))

type API = Capture "ip" IPv4 :> Get '[JSON] IPDetails

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = deets
  where deets :: IPv4 -> EitherT ServantErr IO IPDetails
        deets ipv4 = return $ details ipv4
