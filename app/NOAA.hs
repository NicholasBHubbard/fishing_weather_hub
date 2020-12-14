{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module NOAA
  ( TideData(..)
  , parseAndProcessTideData
  )
  where

import qualified DatabaseSupport as DB
import qualified MyPrelude as MYP
import           Data.Aeson 
import qualified Network.HTTP.Conduit as HTTP
import           Network.HTTP.Types.Header 
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T 
import           Data.Time (getCurrentTime)
import           GHC.Generics (Generic)
import           Data.Maybe (fromJust)
import qualified Data.Map as M
import           Data.List.Split (splitOn)


-- |
-- This module manages everything related to NOAA's api's.
-- It is a little confusing how their api's work. There is a
-- difference between tide stations and meteorlogical stations.
-- All stations have an id. Tide stations ids are 5 digits prefixed with
-- two letters while meteor stations are just a string of seven
-- digits. These stations provide different datas.
-- We use 'DatabaseSupport.hs' to query our station postgres database.
-- Both tide stations and meteorlogical stations are in the same
-- database table. 
-- We can't accurately predict what station a user wants data
-- from. This is because their names don't correspond to city names,
-- There are not stations for every city in the United States.
-- The solution is to ask them using a dynamically generated form.
-- https://api.tidesandcurrents.noaa.gov/api/prod/

type Url = String

------------------------------------------------------------
-------------------- TIDE DATA PARSING ---------------------
------------------------------------------------------------

-- Takes a station id as a string. This string is
-- produced in Base.hs by user selection from dropdown menu that
-- that is rendered after they submit their initial location form.
-- Data comes back parsed and processed. We have the station id
-- at this point.
parseAndProcessTideData :: String -> IO [TideData]
parseAndProcessTideData stationId = do

  t <- getCurrentTime -- We will parse the date out.

  let urlDate = concat $ take 3 $ words $ MYP.parseUtcTime t
      url     = mkTideUrl stationId urlDate

  decoded <- eitherDecode <$> HTTP.simpleHttp url ::
               IO (Either String TideRawJson)

  case decoded of
    Left err -> error $ show err
    Right (TidesRawJson (IntermediateTidePreds xs)) ->
      pure (map convertMessyTideData xs)

mkTideUrl :: String -> String -> String
mkTideUrl stId todaydate =
     "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"
  ++ "?station=" ++ stId
  ++ "&begin_date=" ++ todaydate
  ++ "&range=120"
  ++ "&product=currents_predictions"
  ++ "&interval=MAX_SLACK"
  ++ "&units=metric"
  ++ "&time_zone=lst"
  ++ "&application=web_services"
  ++ "&format=json"

data TideRawJson =
  TidesRawJson
    { current_predictions :: IntermediateTidePreds
    }
  deriving (Eq, Show, Generic, FromJSON)

-- Intermediate type because of the nested nature of
-- NOAA's json.
data IntermediateTidePreds =
  IntermediateTidePreds 
    { cp :: [MessyTideData]
    }
  deriving (Eq, Show, Generic, FromJSON)
  
data MessyTideData =
  MessyTideData
    { time'        :: String
    , ebbFlood'    :: String
    , curVelocity' :: Double -- CentimeterPerSec 
    } 
  deriving (Eq, Show)

-- We can't derive generically because NOAA's tide api
-- has data fields that are capitalized.
instance FromJSON MessyTideData where
  parseJSON =
    withObject "" $ \obj -> do
      time'       <- obj .: "Time"
      ebbOrFlood' <- obj .: "Type"
      curVel'     <- obj .: "Velocity_Major"
      pure $
        MessyTideData time' ebbOrFlood' curVel'

data TideData =
  TideData
    { time              :: String
    , state             :: String -- slack/flood/ebb
    , currentVelocity   :: Double -- CentimeterPerSec 
    } deriving (Eq, Show)

convertMessyTideData :: MessyTideData -> TideData
convertMessyTideData (MessyTideData t st vel) = TideData
  t
  (st ++ " (" ++ MYP.parseHourMin t ++ ")")
  vel

------------------------------------------------------------
------------------------ TESTING ---------------------------
------------------------------------------------------------

-- Getting Woods Hole MA data.
testParsedAndProcessedTideData :: IO ()
testParsedAndProcessedTideData = do
  tides <- parseAndProcessTideData  "ca0101"
  go tides
  where
    go [] = pure ()
    go (x:xs) = print (time x) >> go xs
    
testUrl :: String
testUrl =
      "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"
  ++ "?station=" ++ "8447930"
  ++ "&begin_date=" ++ "20201004"
  ++ "&range=24"
  ++ "&product=predictions"
  ++ "&units=metric"
  ++ "&datum=STND"
  ++ "&time_zone=lst"
  ++ "&application=web_services"
  ++ "&format=json"
