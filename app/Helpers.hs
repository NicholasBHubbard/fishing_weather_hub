{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE CPP                   #-}

module Helpers where

import Yesod
import qualified MyPrelude as MYP
import qualified Data.Text as T
import qualified OpenWeatherMapsCurrent as OWMC
import qualified OpenWeatherMapsFiveDay as OWM5
import           Data.Char (toUpper)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import           Text.Trifecta 
import           Control.Applicative ((<*>), (<|>))
import           Data.List 
import           Data.List.Split (splitOn)
import           Data.Foldable


-- My modified version of selectFieldList where you are given back the
-- the v in Map k v instead of an index number of the
-- generated list. The change really is in 'optionsPairs'.
-- Why would Yesod do that? Not sure.
selectFieldList' ::
     (Eq a, Show a, RenderMessage site FormMessage, RenderMessage site msg)
  => [(msg, a)]
  -> Field (HandlerFor site) a
selectFieldList' = selectField . optionsPairs'

optionsPairs' ::
     (Show a, MonadHandler m, RenderMessage (HandlerSite m) msg)
  => [(msg, a)]
  -> m (OptionList a)
optionsPairs' opts = do
  mr <- getMessageRender
  let mkOption external (display, internal) =
          Option { optionDisplay       = mr display
                 , optionInternalValue = internal
                 , optionExternalValue = T.pack $ show external
                 }
      opts' = foldr (\x acc -> snd x : acc) [] opts
  return $ mkOptionList (zipWith mkOption opts' opts)

-- This map is used in the Home Route to produce the drop-down
-- form that the user selects their country from.
supportedCountriesMap :: [(T.Text, T.Text)]
supportedCountriesMap =
  [ ("United States", "United States")
  , ("Canada", "Canada")
  , ("Great Britain", "Great Britain")
  , ("Finland", "Finland")
  , ("Germany", "Germany")
  , ("Sweden", "Sweden")
  , ("Switzerland", "Switzerland")
  , ("New Zealand", "New Zealand")
  , ("Australia", "Australia")
  ]

-- 'isLocationFormValid' is used to determine if the user
-- has inputted a a valid form for their location. Users from
-- the United States and Canada are required to input a state code.
type Country   = T.Text
type StateCode = Maybe T.Text

isLocationFormValid :: Country
                    -> StateCode
                    -> Bool
isLocationFormValid "United States" (Just _) = True
isLocationFormValid "united states" (Just _) = True
isLocationFormValid "United States" Nothing  = False
isLocationFormValid "united states" Nothing  = False
isLocationFormValid "Canada" (Just _)        = True
isLocationFormValid "canada" (Just _)        = True
isLocationFormValid "Canada" Nothing         = False
isLocationFormValid "canada" Nothing         = False
isLocationFormValid _ _                      = True
