{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

-- This module is responsible for all processing of the Open Weather
--- Maps Five Day forecast Api. This module deals with the production
-- of api request Url's, the extraction of the json from the urls,
-- parsing the json using aeson, and ultimately producing [MetricWeather]'s,
-- that hold 40 MetricWeather data items that hold weather data in increments
-- of 3 hours over the span of five days, starting at the time the request
-- is made. This module utilizes the PostgreSQL cities database, namely the
-- db_city table. For more information on OWM Five Dat api see this link:
-- "https://openweathermap.org/forecast5"
module OpenWeatherMapsFiveDay 
  ( MetricWeatherData(..)
  , parseAndProcessJson  
  )
  where

import qualified MyPrelude as MYP
import qualified DatabaseSupport as DB
import           Data.Aeson 
import           Network.HTTP.Conduit (simpleHttp) 
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T 
import           GHC.Generics (Generic)
import           Data.List.Split (splitOn)

type Url = String

-- This function is used in Html handlers to deal with
-- the transition of turning plain city/country names into
-- weather information from OpenWeatherMaps five day api.
-- The issue of whether the user inputted a state code is
-- dealt with (state code only required in US/Canada)
-- through use of a Maybe type. Postgres Queries are
-- made in order to find a locations latitude and longitude
-- 'DatabaseSupport.hs' connects to this applications database
-- where there is a table of every cities latitude/longitude
-- in over 6 countries. We then use the lat/lon to construct a
-- url for the OpenWeatherMaps api.
-- From here we process the JSON using the Aeson Haskell library
-- We let Base.hs manage what is done with this data.
-- https://openweathermap.org/forecast5
parseAndProcessJson :: String
                    -> Maybe String --state code not required for all countries
                    -> String
                    -> IO [MetricWeatherData] 
parseAndProcessJson city stCode country = do

  dbCity <- case stCode of

              Just st -> do
                x <- DB.queryByCityCtryStCode
                          city
                          (MYP.countryCode country)
                          st
                pure x

              Nothing  -> do
                x <- DB.queryByCityCtry
                          city
                          (MYP.countryCode country) 
                pure x 
  print $ mkFiveDayOwmApiUrl dbCity
  fiveDayJson <- eitherDecode <$>
                   getJSON (mkFiveDayOwmApiUrl dbCity) ::
                   IO (Either String AllJsonCategories)

  case fiveDayJson of
   Left err -> error $ err ++ " should not get to this point"
   Right (AllJsonCategories json) -> 
     pure $ map rawWeatherToMetric json

getJSON :: String -> IO LBS.ByteString
getJSON = simpleHttp 

------------------------------------------------------------
----------------------- URL CREATION -----------------------
------------------------------------------------------------

apiKey :: String
apiKey = "93120a85abf28f8fb1cdae14ffd7435d"

mkFiveDayOwmApiUrl :: DB.DbCity -> Url
mkFiveDayOwmApiUrl city = 
     "https://api.openweathermap.org/data/2.5/forecast?lat="
  ++ show (DB.dbCityLatitude city)
  ++ "&lon="
  ++ show (DB.dbCityLongitude city)
  ++ "&appid="
  ++ apiKey
  ++ "&units=metric"

------------------------------------------------------------
----------------------- JSON PARSING -----------------------
------------------------------------------------------------

data AllJsonCategories =
  AllJsonCategories 
    { list :: [RawWeatherObj] }
  deriving (Eq, Show, Generic, FromJSON)

data RawWeatherObj = 
  RawWeatherObj 
    { main       :: MainJsonNested
    , wind       :: WindJsonNested
    , visibility :: Double --Meters
    , dt_txt     :: String
    , weather    :: Value --Contains Description
    }
  deriving (Eq, Show, Generic, FromJSON)

data MainJsonNested =
  MainJsonNested
    { temp       :: Double --Celsius
    , feels_like :: Double --Celsius
    , pressure   :: Int --Hpa
    , humidity   :: Int --Percent
    }
  deriving (Eq, Show, Generic, FromJSON)

data WindJsonNested =
  WindJsonNested
    { speed :: Double --MetersPerSec
    , deg   :: Int --CompassDegree
    }
  deriving (Eq, Show, Generic, FromJSON)

data MetricWeatherData =
  MetricWeatherData
    { time             :: String
    , description      :: String
    , temperature      :: Double --Celsius
    , feelsLike        :: Double --Celsius
    , hpaPressure      :: Int --Hpa
    , humidityPercent  :: Int --Percent
    , visibilityMeters :: Int --Meters
    , windSpeed        :: Int--Kph
    , windDirection    :: String --Compass Cardinality
    }
  deriving (Eq, Show)

------------------------------------------------------------
--------------------- DATA CONVERSION ----------------------
------------------------------------------------------------

-- Dirty hack to get the description out of the @weather@ field
-- of 'RawWeatherObj'
parseDescription :: Value -> String
parseDescription val = (unwords . tail . words) $
  filter (\x -> elem x ['a'..'z'] || x == ' ') ((last . splitOn "," . show) val)

rawWeatherToMetric :: RawWeatherObj -> MetricWeatherData
rawWeatherToMetric (RawWeatherObj (MainJsonNested temp feel pres hum) (WindJsonNested spd dir) vis time wthrVal) =
  MetricWeatherData
    time
    (parseDescription wthrVal)
    temp
    feel
    pres
    hum
    (ceiling vis)
    (ceiling (MYP.mpsToKph spd))
    (MYP.convertCompass dir)
                                  
------------------------------------------------------------
------------------------- TESTING --------------------------
------------------------------------------------------------

testFiveDay :: IO ()
testFiveDay = do
  --x :: [MetricWeatherData]
  x <- parseAndProcessJson "New Bedford" (Just "MA") "United States"
  case x of
    xs -> go xs
  where
    go [] = print ""
    go (x:xs) = print (time x) >> go xs
