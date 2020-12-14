{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module HelloWorld where

import Yesod

main :: IO ()
main = warp 3000 HelloWorld

data HelloWorld = HelloWorld

instance Yesod HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello, World!|]


