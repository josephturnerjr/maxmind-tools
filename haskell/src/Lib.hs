{-# LANGUAGE OverloadedStrings #-}
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
import Data.IP
import qualified Data.Text as T
import Data.MaxMind
import Control.Monad.Reader (ReaderT, runReaderT, asks)

instance FromText IPv4 where
  fromText = parseIPv4

type AppM = ReaderT Config (EitherT ServantErr IO)

type API = Capture "ip" IPv4 :> Get '[JSON] IPDetails

data Config = Config {
  search :: !MaxMindIPSearch
}

startApp :: IO ()
startApp = do
  ipSearch <- maxMindIPSearch
  let config = Config {search = ipSearch}
  run 8080 $ app config

app :: Config -> Application
app config = serve api (readerServer config)

api :: Proxy API
api = Proxy

readerServer :: Config -> Server API
readerServer config = enter (readerToEither config) server

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

server :: ServerT API AppM
server = lookup where
  lookup :: IPv4 -> AppM IPDetails
  lookup ip = asks (\s -> lookupIP (search s) ip)
