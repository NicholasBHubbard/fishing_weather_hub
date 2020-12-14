{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Base where

import qualified Helpers as H
import qualified OpenWeatherMapsCurrent as OWMC
import qualified OpenWeatherMapsFiveDay as OWM5
import qualified NOAA as NOAA
import qualified DatabaseSupport as DB
import qualified WeatherDataCache as WC
import qualified MyPrelude as MYP
import qualified Tables as TAB
import           Data.ByteString.Internal(unpackChars)
import           Yesod 
import qualified Data.Text as T (Text, pack, unpack, find)
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Monad (when)
import           Control.Exception (throwIO)
import           Data.Maybe (fromMaybe, fromJust)
import           Database.Persist.Postgresql 
import qualified Data.Map as M        


------------------------------------------------------------
------------------------- FOUNDATION -----------------------
------------------------------------------------------------

runSite :: IO ()
runSite = warp 3000 Base

data Base = Base 
instance Yesod Base 
  
mkYesod
  "Base"
  [parseRoutes|
/ HomeR GET
/process-home-loc-form ProcessLocationFormR POST
/tide-data-processing TideDataProcessingR POST
/tables TablesR GET
|]

instance RenderMessage Base FormMessage where
  renderMessage _ _ = defaultFormMessage



------------------------------------------------------------
-------------------------- FORMS ---------------------------
------------------------------------------------------------

-- 'LocationForm' and 'locationForm' are combined to make a form that
-- the user can interact with that is displayed on the sites home page.
-- The form input is used to query the "postgres cities" database,
-- specifically the db_city table. The fields must be 'T.Text' in order
-- to use the 'textField' function in 'locationForm'.
data Unit = Metric | Imperial
  deriving (Eq, Show)

data LocationForm = LocationForm  
    { plainCity    :: T.Text
    , plainStCode  :: Maybe T.Text
    , plainCountry :: T.Text
    , units        :: Unit
    }
    deriving (Eq, Show)

locationForm :: AForm Handler LocationForm 
locationForm = LocationForm 
  <$> areq textField "City:" Nothing
  <*> aopt textField "state code (US/Canada)" Nothing
  <*> areq (H.selectFieldList' H.supportedCountriesMap) "Country:" Nothing
  <*> areq (H.selectFieldList' [("Imperial" :: T.Text,Imperial),("Metric" :: T.Text,Metric)]) "Units: " Nothing

-- Monadically generate '[(T.Text,T.Text)]'. 
-- The map is made from official noaa meteor station ids
-- We need the id in order to form urls for api queries
-- The user selects from a dropdown menu.
noaaTideStationForm :: [(T.Text,T.Text)]
                    -> Html
                    -> MForm Handler (FormResult TideStation, Widget)
noaaTideStationForm stMap = renderDivs $ TideStation
  <$> areq (H.selectFieldList' stMap) "" Nothing

newtype TideStation =
  TideStation T.Text -- Component of api url's.
  deriving (Eq, Show)

------------------------------------------------------------
--------------------- ROUTE HANDLERS -----------------------
------------------------------------------------------------

getHomeR :: Handler Html    
getHomeR = do

  clearSession

  key <- liftIO $ MYP.randomString 9

  setSession "cache_key" (T.pack key) 

  sess <- getSession

  -- Generate the location form
  (widget, enctype) <- generateFormPost $ renderDivs locationForm

  defaultLayout $ do

    -- set <title> tag
    setTitle $ toHtml ("Fishing Weather Hub" :: T.Text)

    -- configure <head> tag
    toWidgetHead
      [hamlet|
<meta name="description" content="Completely free fishing weather data provided by NOAA and Open Weather Maps. Designed to be simple and easy to use.">
<meta name="keywords" content="fishing weather,fishing,weather">
<meta name="robots" content="index,follow">
|]

    [whamlet|
<header>
  <center style="font-size:30px">Fishing Weather Hub
  <br>
  <form method=post action=@{ProcessLocationFormR} enctype=#{enctype}>
    ^{widget}
    <button>Submit
  <a href="https://www.anglerwise.com/2010/02/09/how-does-barometric-pressure-affect-fishing/">How does barometric pressure affect fishing?
|]
    --CSS
    toWidget
      [lucius|
form {
margin: 0 auto;
width:250px;
}
|]

-- Get the result from the hompage location form.
-- Make api calls to OpenWeatherMaps 5 day and Current.
-- Cache the data in postgresql relation 'Wthr_Data_Cache'.        
-- If user is in United states we dynamically ask them what
-- NOAA tide station they want. If this is the case we call
-- NOAA's api to get tide data, and then cache it as well.
-- Control is then directed to Tables.
postProcessLocationFormR :: Handler Html
postProcessLocationFormR = do 

  ((result, _), _) <- runFormPost (renderDivs locationForm)

  case result of 

    FormSuccess (LocationForm name st ctry unit) -> do

        -- Throw an error if an invalid form was submitted.
        liftIO $
          when (False == H.isLocationFormValid ctry st)
               (error "invalid location form")
  
        setSession "unit" (T.pack (show unit))

        sess <- getSession

        let name' = MYP.upperWords (T.unpack name)
            ctry' = MYP.upperWords (T.unpack ctry)
            key   = unpackChars $ fromJust $ M.lookup "cache_key" sess
            unit  = unpackChars $ fromJust $ M.lookup "unit" sess

        if ctry' == "United States" then do
  
            owmCurData <- liftIO $ OWMC.parseAndProcessJson
                                          name'
                                          (fmap T.unpack st)
                                          ctry'

            owmFiveData <- liftIO $ OWM5.parseAndProcessJson
                                           name'
                                           (fmap T.unpack st)
                                           ctry'

            liftIO $ WC.cacheOwmData key owmCurData owmFiveData

            runNoaaTideStationForm $ T.unpack (fromJust st)
    
        else do
  
             owmCurData <- liftIO $ OWMC.parseAndProcessJson
                                           name'
                                           (fmap T.unpack st)
                                           ctry'

             owmFiveData <- liftIO $ OWM5.parseAndProcessJson
                                            name'
                                            (fmap T.unpack st)
                                            ctry'
  
             liftIO $ WC.cacheOwmData key owmCurData owmFiveData

             cacheData <- liftIO $ WC.getCacheWithKey key 

             defaultLayout $ TAB.produceHtmlTable unit cacheData

    -- FormFailure/FormMissing Should never get to this point.
    _ -> error $ "Somehow there was a form failure from Yesod."
              ++ " This should not ever happen."

-- Route that handles input used for making an api request to noaa tides.
-- After caching the tide data we redirect to tables handler.
postTideDataProcessingR :: Handler Html
postTideDataProcessingR = do
  ((result, _), _) <- runFormPost $ renderDivs tideStationRecieverF 

  sess <- getSession

  let key = unpackChars $ fromJust $ M.lookup "cache_key" sess
  
  -- Process the result of tide station drop-down form.
  case result of
      FormSuccess (TideStation id) -> do

          tdata <- liftIO $ NOAA.parseAndProcessTideData
                                   ((MYP.numsAndCharsOnly . T.unpack) id)

          liftIO $ WC.cacheNoaaTideData key tdata

          redirect TablesR

      _ -> error $ "somehow the noaa tide station dropdown form failed"
          

  

-- This is called from LocationR. Starts the process of dynamically
-- generating a list of tide stations, and allowing the user to
-- select one.
runNoaaTideStationForm :: String -> Handler Html
runNoaaTideStationForm st = do
  ss <- liftIO $ DB.queryNoaaTideStations st

  -- If state has no NOAA tide station then just redirect to Tables
  -- and accept that the user will not have tide data available to them.
  if ss == [] then (redirect TablesR) else do

    (widget, enctype) <- generateFormPost $
                           noaaTideStationForm (DB.stationMap ss)
    defaultLayout $
      [whamlet|
<header>
<p>Pick a NOAA Tide Station
<form method=post action=@{TideDataProcessingR} enctype=#{enctype}>
  ^{widget}
  <button>Submit
|]

-- Invisible reciever that grabs the value from the noaaTideStationForm.
-- This function is used to pass the station that the user selects
-- in the postTideDataProcessingR handler.
tideStationRecieverF :: AForm Handler TideStation
tideStationRecieverF = TideStation <$>
  areq textField "irrelevant" Nothing

----------------------------------------------------------
------------------------- TABLES -------------------------
----------------------------------------------------------

-- All of the data has been gathered and proccessed by the time
-- this route is ever called.
getTablesR :: Handler Html
getTablesR = do 

  sess <- getSession
  
  let key  = unpackChars $ fromJust $ M.lookup "cache_key" sess
      unit = unpackChars $ fromJust $ M.lookup "unit" sess

  cacheData <- liftIO $ WC.getCacheWithKey key 

  defaultLayout $ TAB.produceHtmlTable unit cacheData 
