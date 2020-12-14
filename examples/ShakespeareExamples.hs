{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ShakespeareExamples where

import Text.Hamlet (HtmlUrl, shamlet, hamlet)
import Text.Lucius 
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Text (Text, append, pack)
import Data.Char (toLower)
import Data.List (sort)
import Control.Arrow (second)
import Network.HTTP.Types (renderQueryText)
import Data.Text.Encoding (decodeUtf8)
import Blaze.ByteString.Builder (toByteString)
import qualified Data.Text.Lazy.IO as TLIO

--------------------EXAMPLE 1-----------------------
data Person = Person
    { name :: String
    , age  :: Int
    }

main1 :: IO ()
main1 = putStrLn $ renderHtml [shamlet|
<p>Hello, my name is #{name person} and I am #{show $ age person}.
<p>
   Let's mess with my name: #
   <b>#{sort $ map toLower (name person)}
<p>Oh, and in 5 years I'll be {#show ((+) 5 (age person))} years old.
|]
  where
    person = Person "Nick" 21


--------------------EXAMPLE 2-----------------------
data MyRoute = Home

render :: MyRoute -> [(Text, Text)] -> Text
render Home _ = "/home"

footer :: HtmlUrl MyRoute
footer = [hamlet|
<footer>
    ReturnTo #
    <a href=@{Home}>Homepage
    .
|]    

main2 :: IO ()
main2 = putStrLn $ renderHtml $ [hamlet|
<body>
    <p>This is my page.
    ^{footer}
|] render    

 
--------------------EXAMPLE 3 (query string interpolation)-----------------------
data MyRoute' = SomePage

render' :: MyRoute' -> [(Text, Text)] -> Text
render' SomePage params =
  "/home" `append`
  decodeUtf8 (toByteString $ renderQueryText True (map (second Just) params))

main3 :: IO ()
main3 = do
      let currPage = 2 :: Int
      putStrLn $ renderHtml $ [hamlet|
  <p>
      You are currently on page #{currPage}.
      <a href=@?{(SomePage, [("page", pack $ show $ currPage - 1)])}>Previous
      <a href=@?{(SomePage, [("page", pack $ show $ currPage + 1)])}>Next
  |] render'


---------------------EXAMPLE 4 (CSS Lucius)-----------------------
--dummy render functions
render'' = undefined

-- Our mixing which provides a number of vendor prefixes
-- for transitions
transition val =
    [luciusMixin|
          -webkit-transition: #{val};
          -moz-transition: #{val};
          -ms-transition: #{val};
          -o-transition: #{val};
          transition: #{val};
    |]

--the actual template which uses the mixin
--myCSS =
--  [lucius|
--      .some-class {
--          ^{transition "all 4s ease"}
--      }
--  |]

--main4 = TLIO.putStrLn $ renderCss $ myCss render


      
-------------------------------EXAMPLE 5------------------------------
data MyRoute'' = Home'' | Time'' | Stylesheet''

render''' :: MyRoute'' -> [(Text, Text)] -> Text
render''' Home'' _ = "/home"
render''' Time'' _ = "/time"
render''' Stylesheet'' _ = "/style.css"

template :: Text -> HtmlUrl MyRoute''
template title = [hamlet|
$doctype 5
<html>
    <head>
        <title>#{title}
        <link rel=stylesheet href=@{Stylesheet''}>
    <body>
        <h1>#{title}
|]        

main5 :: IO ()
main5 = putStrLn $ renderHtml $ template "My Title" render'''



-------------------------------EXAMPLE 6------------------------------
data MyRoute''' = Home''' | Time''' | Stylesheet'''

render'''' :: MyRoute''' -> [(Text, Text)] -> Text
render'''' Home''' _ = "/home"
render'''' Time''' _ = "/time"
render'''' Stylesheet''' _ = "/style.css"

--template :: CssUrl MyRoute
-- #if PRODUCTION
--template = $(luciusFile "template.lucius")
-- #else
--template = $(luciusFileDebug "template.lucius")
-- #endif

--main6 :: IO ()
--main6 = TLIO.putStrLn $ renderCss $ template render
