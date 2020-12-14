{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module DatabaseSupport 
  ( DbCity(..) 
  , NoaaStations(..)
  , queryByCityCtryStCode 
  , queryByCityCtry  
  , queryNoaaTideStations  
  , showDbCity
  , stationMap 
  )
  where

import qualified MyPrelude as MYP
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger   (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.TH
import           Database.Persist.Postgresql
import qualified Data.Conduit.List as CL
import           Data.Conduit
import qualified Data.Text as T (Text, pack, unpack)

connStr :: ConnectionString
connStr = "host=localhost dbname=fishing_web_app user=nick password=null123 port=5432"

-- These types allign with the SQL tables noaa_stations and db_city.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

NoaaStations 
  stationid T.Text
  name T.Text
  state T.Text
  deriving Show Read Eq

DbCity 
  name T.Text
  state_code T.Text
  country_code T.Text
  latitude Double
  longitude Double
  deriving Show Read Eq

|]  

showDbCity :: DbCity -> String
showDbCity (DbCity name stCode counCode lat lon) =
     T.unpack name ++ ", "
  ++ T.unpack stCode ++ ", "
  ++ MYP.codeCountry (T.unpack counCode)
  ++ ", latitude = " ++ show lat
  ++ ", longitude = " ++ show lon

-- Returns all stations that match the inputted state-code. (US only)
-- Tide stations have a letter as the first character in their id string.
-- Meteorlogical stations have a digit as their first character.
-- Input cleansing of state code happens here. ("ma" -> "MA").
queryNoaaTideStations :: String -> IO [NoaaStations]
queryNoaaTideStations st = runStderrLoggingT $ withPostgresqlPool connStr 10 $
  \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do

      ss <- selectList [NoaaStationsState -- All stations in the state.
                            ==. T.pack (MYP.allUpper st)] []
      case ss of
        []  -> error $ "Apparently there are no tide stations in "
                    ++ st
        xs  -> pure $
                 let ss'  = map (\(Entity k a) -> a) xs
                     sids = map getStationId ss'
                     smap = zipWith (,) sids ss'
                     p    = (flip elem) ['a'..'z'] . head 
                 in
                   foldr (\(k,v) acc -> if p k then v : acc
                                        else acc) [] smap

getStationId :: NoaaStations -> String
getStationId (NoaaStations id _ _) = T.unpack id 
                          
stationMap :: [NoaaStations] -> [(T.Text,T.Text)]
stationMap =
  foldr (\(NoaaStations id name state) acc -> (name, id) : acc) []

----------------------------------------------------------
--------------------- CITIES TABLE -----------------------
----------------------------------------------------------

-- Query (db_city) SQL table.
-- This table has all cities in about 10 countries.
-- This function queries by city name and country code only.
-- Ex: New Bedford, US
-- Ex: new bedford, US (input cleansing happens here).
queryByCityCtry :: String -> String -> IO DbCity
queryByCityCtry city country = runStderrLoggingT $ withPostgresqlPool connStr 10 $
  \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
      cities <- selectList [ DbCityName ==.
                               (T.pack . MYP.upperWords) city
                           , DbCityCountry_code ==.
                               (T.pack . MYP.upperWords) country
                           ] []
      case cities of
        []     -> error $ "Could not find location. Database had no hits on "
                       ++ city ++ ", " ++ country

        (x:[]) -> pure $ (\(Entity k a) -> a) x

        _      -> error $ "ambigous occurrence of " ++ city
                        ++ ", " ++ country
                        ++ "We did not account for this nick"
                        
-- Query cities with city name, country name and two digit state code.
-- Ex: New Bedford, MA, United States...
-- or: new beford, ma, united states (input cleansing here)
queryByCityCtryStCode :: String -> String -> String -> IO DbCity
queryByCityCtryStCode city country stCode = runStderrLoggingT $ withPostgresqlPool connStr 10 $
  \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
      cities <- selectList [ DbCityName ==.
                               (T.pack . MYP.upperWords) city
                           , DbCityCountry_code ==.
                               (T.pack . MYP.upperWords) country
                           , DbCityState_code ==.
                               (T.pack . MYP.allUpper) stCode 
                           ] []
      case cities of
        []     -> error $ "Could not find location. Database had no hits on "
                       ++ city ++ ", " ++ stCode ++ ", " ++ MYP.codeCountry country
        (x:[]) -> pure $ (\(Entity k a) -> a) x
  
       
        _      -> error $ "ambigous occurrence of " ++ city
                       ++ ", " ++ MYP.codeCountry country
                       ++ stCode ++ " We did not account for this nick"
