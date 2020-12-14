{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Tables
  ( produceHtmlTable
  ) where

import           Yesod hiding (Header)
import qualified OpenWeatherMapsCurrent as OWMC
import qualified OpenWeatherMapsFiveDay as OWM5
import qualified WeatherDataCache as WC
import qualified MyPrelude as MYP
import qualified Data.Text as T
import           Text.Tabular 
import           Text.Tabular.Html
import qualified Text.Tabular.AsciiArt as ASCII
import           Text.Html
import           Data.List (sortBy)
import           Control.Lens
import           Data.Maybe (fromJust)
import qualified Data.Map as M        
import qualified Data.Text.Lazy.Builder as B
 
data TableRow =
  TableRow
    { _time      :: String
    , _descrip   :: Maybe String
    , _temp      :: Maybe Int 
    , _feels     :: Maybe Int
    , _pressure  :: Maybe Int 
    , _humid     :: Maybe Int 
    , _visib     :: Maybe Int
    , _windSpd   :: Maybe Int 
    , _windDir   :: Maybe String
    , _tideState :: Maybe String 
    , _tideVel   :: Maybe Int
    }
  deriving (Eq, Show)
makeLenses ''TableRow

-- Convert a row of metric data to imperial data where applicable. Note
-- that there is no function to convert a row from imperial to metric. This
-- is because data by default is metric and is only converted if the user
-- specifies they want imperial. If the user wants metric then no conversions
-- are made.
metricRowToImperal :: TableRow -> TableRow
metricRowToImperal tr =
  tr & over temp (fmap (truncate . MYP.celsiusToFahr . fromIntegral))
     & over feels (fmap (truncate . MYP.celsiusToFahr . fromIntegral))
     & over windSpd (fmap (truncate . MYP.kphToMph . fromIntegral))        
     & over tideVel (fmap (truncate . MYP.cmsPerSecToInchesPerSec . fromIntegral))        

sortRowsByTime :: [TableRow] -> [TableRow]
sortRowsByTime = sortBy orderTableRow

orderTableRow :: TableRow -> TableRow -> Ordering
orderTableRow r1 r2 = MYP.orderTime (r1^.time) (r2^.time)

-- Temp and feels_like, and tide_velocity are kept as doubles in the
-- cache but are converted to Int's for human readability.
cacheRowToTableRow :: WC.WthrDataCache -> TableRow
cacheRowToTableRow
  (WC.WthrDataCache _ t des temp feel pres hum vis wspd wdir st vel) =
    TableRow t des (fmap truncate temp) (fmap truncate feel) pres hum vis wspd wdir st (fmap truncate vel)

metricColumns :: [String]
metricColumns =
  [ "time","description","temp (celsius)","feels_like (celsius)","pressure (hpa)"
  , "humidity","visibility","wind_speed (kph)","wind_direct"
  , "current","current_velocity (cm-per-sec)"
  ]

imperialColumns :: [String]
imperialColumns = 
  [ "time","description","temp (fahr)","feels_like (fahr)","pressure (hpa)"
  , "humidity","visibility","wind_speed (mph)","wind_direct"
  , "current","current_velocity (inches-per-sec)"
  ]

formulateTable :: String -> [WC.WthrDataCache] -> Table String String String
formulateTable unit ws = Table 
  (Group DoubleLine
     [ Group DoubleLine
         (mkHeaders null)
     ])
  (Group DoubleLine
     [ Group DoubleLine
         (mkHeaders columns)
     ])
   (finalizeTableRows (sortRowsByTime rows))
    where
      ws' = map cacheRowToTableRow ws
      null = map (\_ -> "") ws
      columns = if unit == "Imperial" then imperialColumns else metricColumns 
      rows = if unit == "Imperial" then map metricRowToImperal ws' else ws'
      mkHeaders = map (\x -> Header x)
  
produceHtmlTable :: String -> [WC.WthrDataCache] -> WidgetFor site ()
produceHtmlTable unit wthrs = do
  toWidget . preEscapedToMarkup . renderHtml $ tbl
  toWidget . CssBuilder . B.fromString $ defaultCss
  where
    tbl = render stringToHtml stringToHtml stringToHtml (formulateTable unit wthrs)
    renderHtml h = foldr ($) "" [renderHtml' 0 e | e <- getHtmlElements h]

maybeToRowData :: (Show a) => Maybe a -> String
maybeToRowData Nothing  = ""
maybeToRowData (Just a) = show a
  
finalizeTableRows :: [TableRow] -> [[String]]
finalizeTableRows =
  map (\(TableRow t des temp feel pres hum vis wspd wdir st vel) ->
      [ t, maybeToRowData des, maybeToRowData temp
      , maybeToRowData feel, maybeToRowData pres
      , (maybeToRowData hum)++"%", (maybeToRowData vis)++"%"
      , maybeToRowData wspd, maybeToRowData wdir
      , maybeToRowData st, maybeToRowData vel
      ]) 


------------------------------------------------------------
------------------------- TESTING --------------------------
------------------------------------------------------------

-- Needs to updated if the cache is emptied because data with the
-- test key has been deleted. You must look in the cache table to
-- get a new key.
testCacheKey :: String
testCacheKey = "J65P5L1HF"

testTable :: IO ()
testTable = do

  cacheData <- WC.getTestCache testCacheKey

  let sortedCacheData = WC.sortCacheByTime cacheData
      table = formulateTable "Metric" sortedCacheData 

--  putStrLn $ ASCII.render id id id table
  mapM_ (print . WC.wthrDataCacheTime) sortedCacheData 

