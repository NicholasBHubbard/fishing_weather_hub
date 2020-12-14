{-# LANGUAGE OverloadedStrings          #-}
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

module WeatherDataCache 
  ( WthrDataCache(..)
  , cacheOwmData  
  , cacheNoaaTideData  
  , getCacheWithKey  
  , getTestCache 
  , sortCacheByTime
  )
  where

import qualified MyPrelude as MYP
import qualified Helpers as H
import qualified OpenWeatherMapsCurrent as OWMC
import qualified OpenWeatherMapsFiveDay as OWM5
import qualified NOAA as NOAA
import qualified Data.Text as T (Text, pack, unpack)
import           Data.Time (getCurrentTime)
import           Database.Persist
import           Database.Persist.TH
import           Database.Persist.Postgresql
import           Data.Maybe (fromJust)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger   (runStderrLoggingT)
import           Data.List(sortBy)

type CacheKey = String

connStr :: ConnectionString
connStr = "host=localhost dbname=fishing_web_app user=nick password=null123 port=5432"

-- Alligns with the wthr_data_cache table in the SQL database.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
WthrDataCache 
  key String
  time String
  description String Maybe
  temp Double Maybe
  feels_like Double Maybe
  pressure Int Maybe
  humidity Int Maybe
  visibility Int Maybe
  wind_speed Int Maybe
  wind_dir String Maybe
  tide_state String Maybe 
  tide_velocity Double Maybe 
  deriving Show Read Eq
|]  

------------------------------------------------------------
------------------ CACHE DATA REASONING --------------------
------------------------------------------------------------

sortCacheByTime :: [WthrDataCache] -> [WthrDataCache]
sortCacheByTime = sortBy orderCacheWithTime

orderCacheWithTime :: WthrDataCache
                   -> WthrDataCache
                   -> Ordering
orderCacheWithTime w1 w2 = MYP.orderTime t1 t2
  where
    t1 = wthrDataCacheTime w1
    t2 = wthrDataCacheTime w2
  
------------------------------------------------------------
---------------------- DATA CONVERTING ---------------------
------------------------------------------------------------

convertOWMC :: CacheKey -> OWMC.MetricWeatherData -> WthrDataCache
convertOWMC key (OWMC.MetricWeatherData time desc temp feel pr hum vis wspd wdir) =
  WthrDataCache
    key
    time
    (Just desc)
    (Just temp)
    (Just feel)
    (Just pr)
    (Just hum)
    (Just vis)
    (Just wspd)
    (Just wdir)
    Nothing
    Nothing

convertOWM5 :: CacheKey -> OWM5.MetricWeatherData -> WthrDataCache
convertOWM5 key (OWM5.MetricWeatherData time desc temp feel pr hum vis wspd wdir) =
  WthrDataCache
    key
    time
    (Just desc)
    (Just temp)
    (Just feel)
    (Just pr)
    (Just hum)
    (Just vis)
    (Just wspd)
    (Just wdir)
    Nothing
    Nothing

convertNoaaTide :: CacheKey -> NOAA.TideData -> WthrDataCache
convertNoaaTide key (NOAA.TideData time tstate vel) =
   WthrDataCache 
     key
     time
     Nothing
     Nothing
     Nothing
     Nothing
     Nothing
     Nothing
     Nothing
     Nothing
     (Just tstate)
     (Just vel)
    
getCacheWithKey :: CacheKey -> IO [WthrDataCache]
getCacheWithKey k = runStderrLoggingT $ withPostgresqlPool connStr 10 $
  \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
      wthrs <- selectList [WthrDataCacheKey ==. k] []
      pure $ map (\(Entity k a) -> a) wthrs

------------------------------------------------------------
-------------------- OPEN WEATHER MAPS DATA ----------------
------------------------------------------------------------

cacheOwmData :: CacheKey
             -> [OWMC.MetricWeatherData]
             -> [OWM5.MetricWeatherData]
             -> IO ()
cacheOwmData key curs fives = runStderrLoggingT $ withPostgresqlPool connStr 10 $
  \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
      let owms = convertAndCombineOwmWeathers key curs fives
      mapM_ insert owms

convertAndCombineOwmWeathers :: CacheKey
                             -> [OWMC.MetricWeatherData]
                             -> [OWM5.MetricWeatherData]
                             -> [WthrDataCache]
convertAndCombineOwmWeathers key curs fives = mappend cs fs
  where
    cs     = map (convertOWMC key) curs 
    fs     = map (convertOWM5 key) (drop 14 fives)

------------------------------------------------------------
----------------------- NOAA TIDE DATA ---------------------
------------------------------------------------------------

cacheNoaaTideData :: CacheKey -> [NOAA.TideData] -> IO ()
cacheNoaaTideData key tides = runStderrLoggingT $ withPostgresqlPool connStr 10 $
  \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
      
      keyMatches <- selectList [ WthrDataCacheKey ==. key
                               ] []
      
      liftIO $ go compareTideAndEntityTimes 0 tides keyMatches keyMatches 

      where

        -- a bit of a dirty hack to insert the slack and ebbs (which do not
        -- come in hour intervals), into the proper rows in the cache.
        go _ _ [] _ _        = pure ()
        go p n ts [] es'     = go p (n+1) ts es' es'
        go p n (t:ts) (e:es) es'
          | n > 3     = go p 0 ts es' es'
          | (p n) t e = updateCacheEntityWithTide t e >> go p 0 ts es' es'
          | otherwise = go p n (t:ts) es es'
          
updateCacheEntityWithTide :: NOAA.TideData
                          -> Entity WthrDataCache
                          -> IO ()
updateCacheEntityWithTide (NOAA.TideData _ st vel) (Entity k _) =
  runStderrLoggingT $ withPostgresqlPool connStr 10 $
    \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        update k [ WthrDataCacheTide_state =. (Just st)
                 , WthrDataCacheTide_velocity =. (Just vel)
                 ]
  
-- Returns true if the NOAA.TideData is withing n hours of the
-- database entity time. For example if n = 0, then the times must be the
-- same. If n = 1 then the times must be within one hour of each other.
compareTideAndEntityTimes :: Integer
                          -> NOAA.TideData
                          -> Entity WthrDataCache
                          -> Bool
compareTideAndEntityTimes n t e = MYP.withinNHours n t' e'
  where
    t' = NOAA.time t
    e' = (wthrDataCacheTime . entityVal) e

  

------------------------------------------------------------
------------------------- TESTING --------------------------
------------------------------------------------------------

--This needs to be updated with a real key to run tests.
testKey :: CacheKey 
testKey = "IP0VN4CS4"

testCacheNoaaTideData :: IO ()
testCacheNoaaTideData = runStderrLoggingT $ withPostgresqlPool connStr 10 $
  \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do

      tides <- liftIO $ NOAA.parseAndProcessTideData  "ca0101"
  
      entities <- selectList [ WthrDataCacheKey ==. testKey
                             ] []

      liftIO $ go tides
      liftIO $ print ""
      liftIO $ print ""
      liftIO $ go' entities
  
      where
        go [] = print ""
        go (x:xs) =
          print x >> go xs
        
        go' [] = print ""
        go' (x:xs) =
          print ((wthrDataCacheTime . entityVal) x) >> go' xs

testCacheNoaaTideData2 :: IO () 
testCacheNoaaTideData2 = runStderrLoggingT $ withPostgresqlPool connStr 10 $
  \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
      
      entities <- selectList [ WthrDataCacheKey ==. testKey
                             ] []
      let naked = map (wthrDataCacheTime . entityVal) entities
          ordered = sortBy MYP.orderTime naked

      liftIO $ mapM_ print ordered


getTestCache :: CacheKey -> IO [WthrDataCache]
getTestCache k = runStderrLoggingT $ withPostgresqlPool connStr 10 $
  \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
      
      entities <- selectList [ WthrDataCacheKey ==. k
                             ] []

      pure (map entityVal entities)
