{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

-- |
-- This module is responsible for all processing of the
-- Open Weather Maps Current Api, including the production
-- of api request Url's, extraction of the json from the urls,
-- parsing the json using aeson, and for ultimately returning
-- [MetricWeather], that hold 48 hours of weather data in metric
-- format, along with names for column headers. This module utilizes
-- the PostgreSQL cities database, namely the db_city table. 
-- in preperation for the data to be transformed into tables.
-- For more information on OWM Current api see this link:
-- "https://openweathermap.org/current"
module OpenWeatherMapsCurrent
  ( MetricWeatherData(..)
  , parseAndProcessJson    
  )
  where
  
import qualified MyPrelude as MYP
import qualified DatabaseSupport as DB
import           Data.Aeson 
import           Network.HTTP.Conduit (simpleHttp) 
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T 
import           GHC.Generics (Generic)
import           Data.List.Split (splitOn)

type Url                = String

-- 'parseAndProccessJson' is used in Base Html handlers to deal with
-- the transition of turning plain city/country names into
-- weather information from OpenWeatherMaps current api.
-- The issue of whether the user inputted a state code is
-- dealt with (state code only required in US/Canada)
-- through use of a Maybe type. Postgres Queries are
-- made in order to find a locations latitude and longitude
-- 'DatabaseSupport.hs' connects to this applications database
-- where there is a table of every cities latitude and longitude
-- in over 6 countries. We then use the lat and lon to construct 
-- a url for the OpenWeatherMaps api.
-- From here we process the JSON using the Aeson Haskell library
-- We let Base.hs manage what is done with this data.
-- https://openweathermap.org/current
parseAndProcessJson :: String
                    -> Maybe String
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
  curAndHrlyJson <- eitherDecode <$>
                    getJSON (mkCurrentOwmApiUrl dbCity) ::
                    IO (Either String CurrentAndHourly)

  case curAndHrlyJson of
   Left err -> error $ err ++ " Should never get to this point"
   Right (CurrentAndHourly offset cur hrly) ->
     pure $ map (rawWeatherToMetric offset) (tail (cur : hrly))

getJSON :: String -> IO LBS.ByteString
getJSON = simpleHttp 

------------------------------------------------------------
----------------------- URL CREATION -----------------------
------------------------------------------------------------
apiKey :: String
apiKey = "93120a85abf28f8fb1cdae14ffd7435d"

mkCurrentOwmApiUrl :: DB.DbCity -> Url
mkCurrentOwmApiUrl city =
     "https://api.openweathermap.org/data/2.5/onecall?lat="
  ++ show (DB.dbCityLatitude city)
  ++ "&lon="
  ++ show (DB.dbCityLongitude city)
  ++ "&exclude=minutely&appid="
  ++ apiKey
  ++ "&units=metric"

------------------------------------------------------------
----------------------- JSON PARSING -----------------------
------------------------------------------------------------

-- This type is needed because OpenWeatherMaps Current Weather
-- Data formats their api such that they send back their data
-- in a hierarchical format in which the first object is titled
-- 'current' for the weather at the time the request was sent.
-- They also send back a list of 48 objects that correspond to
-- the weather data for the next 48 hours, We also grab the timezone
-- offset which is used to render times that are human readable
-- in 'rawWeatherToMetric'.
data CurrentAndHourly =
  CurrentAndHourly
    { timezone_offset :: Integer
    , current         :: RawWeatherObj 
    , hourly          :: [RawWeatherObj]
    }
  deriving (Eq, Show, Generic, FromJSON)

-- This type alligns with the JSON from
-- OpenWeatherMaps Current Weather Data API.
data RawWeatherObj =
  RawWeatherObj
    { dt         :: Integer -- Posix Time
    , temp       :: Double -- Celsius
    , feels_like :: Double -- Celsius
    , pressure   :: Int -- Hpa
    , humidity   :: Int -- Percent
    , visibility :: Double -- Meters
    , wind_speed :: Double -- MetersPerSec
    , wind_deg   :: Int -- Compass Degree
    , weather    :: Value -- Contains the description ("scattered clouds")
    }
  deriving (Eq, Show, Generic, FromJSON)

-- Raw weather data is transformed into this MetricWeather type.
-- The first field represents the names of the headers that
-- will be displayed when this type is made into table data.
data MetricWeatherData =
  MetricWeatherData
    { time             :: String
    , description      :: String
    , temperature      :: Double -- Celsius
    , feelsLike        :: Double -- Celsius
    , hpaPressure      :: Int -- Hpa
    , humidityPercent  :: Int -- Percent
    , visibilityMeters :: Int -- Meters
    , windSpeed        :: Int -- Kph
    , windDirection    :: String -- Compass Cardinality
    }
  deriving (Eq, Show)

------------------------------------------------------------
--------------------- DATA CONVERSION ----------------------
------------------------------------------------------------

-- Dirty hack to get the description out of the @weather@ field
-- of 'RawWeatherObj'
parseDescription :: Value -> String
parseDescription val = (unwords . tail . words) $
  filter
  (\x -> elem x ['a'..'z'] || x == ' ') ((last . splitOn "," . show) val)

-- The time offset is given by the api. We then apply that offset
-- to a function that gives us back the local time of the
-- location in which the api request was made to. (24hr time).
rawWeatherToMetric :: Integer -> RawWeatherObj -> MetricWeatherData
rawWeatherToMetric timeOffset obj =
  MetricWeatherData
    (dropUtcChars (show (MYP.makeLocalTime timeOffset (dt obj))))
    (parseDescription (weather obj))
    (temp obj)
    (feels_like obj)
    (pressure obj)
    (humidity obj)
    (ceiling ((visibility obj) / 100))
    (ceiling (MYP.mpsToKph (wind_speed obj)))
    (MYP.convertCompass (wind_deg obj))
    where
      dropUtcChars = MYP.drop_last_n_elems 4

------------------------------------------------------------
------------------------- TESTING --------------------------
------------------------------------------------------------

testParseAndProcessJson :: IO ()
testParseAndProcessJson = do
  --x :: [MetricWeatherData]
  x <- parseAndProcessJson "new bedford" Nothing "United States"
  case x of
    xs -> go xs
  where
    go [] = print ""
    go (x:xs) = print (time x) >> go xs
