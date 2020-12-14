{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExtendedDefaultRules #-} --this is new

module NonHtml where

import Yesod

data App = App 

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App
  
getHomeR = return $ object ["msg" .= "Hello, World"]

main :: IO ()
main = warp 3000 App
