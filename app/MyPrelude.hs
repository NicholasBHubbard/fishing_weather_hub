{-# LANGUAGE OverloadedStrings #-}

module MyPrelude where

import           Data.Char (toUpper)
import qualified Data.Text as T 
import           Data.Time.Clock
import           Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import           Text.Trifecta 
import           Control.Applicative ((<*>), (<|>))
import           Data.List 
import           Data.List.Split (splitOn)
import           Data.Foldable
import           Control.Lens.Combinators
import           Control.Monad
import qualified System.Random as R

upperWords :: String -> String
upperWords str = unwords $
  map (\xs -> toUpper (head xs) : tail xs) (words str)

allUpper :: String -> String
allUpper str = filter (/= ' ') (go str)
  where
    go []     = []
    go (x:xs) = toUpper x : go xs

integerToPaddedString :: (Integral n, Show n) => n -> String
integerToPaddedString n
  | n < 10 && n >= 0 = "0" ++ show n
  | otherwise        = show n

numsAndCharsOnly :: String -> String
numsAndCharsOnly = filter (flip elem (['a'..'z']
                                   ++ ['A'..'Z']
                                   ++ ['0'..'9']))
                 
last_n_elems :: Int -> [a] -> [a]
last_n_elems _ [] = []
last_n_elems n (_:tail)
  | length tail == n = tail
  | otherwise =
      last_n_elems n tail
  
drop_last_n_elems :: Int -> [a] -> [a]
drop_last_n_elems _ [] = []
drop_last_n_elems n (x:xs)
  | length xs == (n-1) = []
  | otherwise =
      x : drop_last_n_elems n xs
  
randomString :: Int -> IO String
randomString n = replicateM n (
  do r <- R.randomRIO (0,m)
     pure (chars !! r)
  )
  where
    chars = ['A'..'Z'] ++ ['0'..'9']
    m = length chars - 1

isAscending :: Ord a => [a] -> Bool
isAscending []  = True 
isAscending [_] = True 
isAscending (x:xs) 
  | x < head xs = isAscending xs
  | otherwise   = False

idDescending :: Ord a => [a] -> Bool
idDescending = not . isAscending

convertCompass :: (Integral a, Show a) => a -> String
convertCompass deg
  | deg >  360 || deg < 0   = throwErr
  | deg >= 315              = "North West"
  | deg >= 270              = "West"
  | deg >= 225              = "South West"
  | deg >= 180              = "South"
  | deg >= 135              = "South East"
  | deg >= 90               = "East"
  | deg >= 45               = "North East"
  | deg >= 0                = "North"
   where
     throwErr =
       error $ "Cannot deduce compass cardinal direction of " ++ show deg 

------------------------------------------------------------
--------------------- UNIT CONVERSIONS ---------------------
------------------------------------------------------------

mpsToKph :: (Num a, Fractional a) => a -> a
mpsToKph = (* 3.6)

mpsToMph :: (Num a, Fractional a) => a -> a
mpsToMph = (* 2.2369363)

kphToMph :: (Num a, Fractional a) => a -> a
kphToMph = (* 0.621371)

mphToKph :: (Num a, Fractional a) => a -> a
mphToKph = (* 1.609344)

celsiusToFahr :: (Num a, Fractional a) => a -> a
celsiusToFahr = (+32) . (* 1.8)

fahrToCelsius :: (Num a, Fractional a) => a -> a
fahrToCelsius = (/ 1.8) . (subtract 32)

metersToFeet :: (Num a, Fractional a) => a -> a
metersToFeet = (* 3.2808398)

feetToMeters :: (Num a, Fractional a) => a -> a
feetToMeters = (/ 3.2808398)

cmsPerSecToInchesPerSec :: (Num a, Fractional a) => a -> a
cmsPerSecToInchesPerSec = (* 0.393701)

------------------------------------------------------------
---------------------- TIME REASONING ----------------------
------------------------------------------------------------
  
epochToUTC :: Integral a => a -> UTCTime
epochToUTC = posixSecondsToUTCTime . fromIntegral

shiftTime :: Integral a => a -> UTCTime -> UTCTime
shiftTime sec time =
  addUTCTime toAdd time
    where
      toAdd = fromIntegral sec :: NominalDiffTime
  
makeLocalTime :: Integral a => a -> a -> UTCTime
makeLocalTime dt offset = shiftTime offset (epochToUTC dt)

--2020-08-10
mkZeroLeadNum :: (Integral a, Show a) => a -> String
mkZeroLeadNum x 
  | x >= 10   = show x
  | otherwise = ( "0" ++ show x)

subtractUtcDay :: Int -> UTCTime -> String
subtractUtcDay n date = 
  let justDate = takeWhile (/= ' ') (show date)
      spl  = (splitAt (length justDate - 2) justDate)
      spl' = (fst spl, (mkZeroLeadNum . subtract n . read . snd) spl)
  in foldOf each spl' -- foldOf each ("foo","bar") = "foobar"

dropLast :: [a] -> [a]
dropLast []     = []
dropLast (_:[]) = []
dropLast (x:xs) = x : dropLast xs

------------------------------------------------------------
------------------------- PARSING --------------------------
------------------------------------------------------------

------------------ "2020-07-31 09:00:00" -------------------

skipWhitespace :: Parser ()
skipWhitespace = 
  skipMany (char ' ' <|> char '\n')

--  timeAsIntegers = [year,month,day,hour,min]
sameHourAndDay :: String -> String -> Bool
sameHourAndDay t1 t2 = 
  dropLast p1 == dropLast p2 
  where
    p1 = asum $ parseString timeAsIntegers mempty t1
    p2 = asum $ parseString timeAsIntegers mempty t2   

sameDay :: String -> String -> Bool
sameDay t1 t2 = take 3 p1 == take 3 p2
  where
    p1 = asum $ parseString timeAsIntegers mempty t1
    p2 = asum $ parseString timeAsIntegers mempty t2   

withinNHours :: Integer -> String -> String -> Bool
withinNHours n t1 t2 =
  sameDay t1 t2 && (h1 - h2 == n || h2 - h1 == n) 
  where
    p1 = asum $ parseString timeAsIntegers mempty t1
    h1 = p1 !! 3
    p2 = asum $ parseString timeAsIntegers mempty t2   
    h2 = p2 !! 3

utcTimeParser :: Parser String
utcTimeParser = do
  year <- integer
  char '-' 
  month <- integer 
  char '-' 
  day <- integer
  skipWhitespace
  hour <- integer 
  char ':' 
  minute <- integer 
  pure $ show year
      ++ (" ")
      ++ (if month < 10 then "0" else "")
      ++ show month
      ++ (" ")
      ++ (if day < 10 then "0" else "")
      ++ show day
      ++ (" ")
      ++ show hour
      ++ (" ")
      ++ show minute

parseUtcTime :: UTCTime -> String                    
parseUtcTime t =
  case parse of
    Success a -> a
    Failure err -> error $ show err
  where
    parse = parseString utcTimeParser mempty (show t)

timeAsIntegers :: Parser [Integer]
timeAsIntegers = do
  year <- integer
  char '-' 
  month <- integer 
  char '-' 
  day <- integer
  skipWhitespace
  hour <- integer 
  char ':'
  min <- integer
  pure $ [year,month,day,hour,min]

hourMinParser :: Parser String
hourMinParser = do
  _ <- integer
  char '-' 
  _ <- integer 
  char '-' 
  _ <- integer
  skipWhitespace
  hour <- integer 
  char ':'
  min <- integer
  pure $
    (integerToPaddedString hour)
    ++ ":"
    ++ (integerToPaddedString min)

parseHourMin :: String -> String
parseHourMin t =
  case parse of
    Success a -> a
    Failure err -> error $ show err
  where
    parse = parseString hourMinParser mempty t

isTimeStrEarlier :: String -> String -> Bool                    
isTimeStrEarlier t1 t2 = lexOrder p1 p2
  where
    p1 = asum $ parseString timeAsIntegers mempty t1
    p2 = asum $ parseString timeAsIntegers mempty t2
    lexOrder [] [] = False
    lexOrder (x:xs) (y:ys)
      | x == y    = lexOrder xs ys
      | otherwise = x < y
    
orderTime :: String -> String -> Ordering
orderTime t1 t2
  | isTimeStrEarlier t1 t2 = LT
  | isTimeStrEarlier t2 t1 = GT
  | otherwise = EQ

------------------------------------------------------------
------------------------ LOCATIONS -------------------------
------------------------------------------------------------
  
countryCode "United States" = "US"
countryCode "Canada" = "CA"
countryCode "Great Britain" = "GB"
countryCode "Finland" = "FI"
countryCode "Netherlands" = "NE"
countryCode "Germany" = "DE"
countryCode "Sweden" = "SE"
countryCode "Switzerland" = "CH"
countryCode "New Zealand" = "NZ"
countryCode "Australia" = "AU"
countryCode str = error $ "Country " ++ str ++ " not supported"

codeCountry "US" = "United States"
codeCountry "CA" = "Canada"
codeCountry "GB" = "Great Britain"
codeCountry "FI" = "Finland"
codeCountry "NE" = "Netherlands"
codeCountry "DE" = "Germany"
codeCountry "SE" = "Sweden"
codeCountry "CH" = "Switzerland"
codeCountry "NZ" = "New Zealand"
codeCountry "AU" = "Australia"
codeCountry str = error $ "Country Code " ++ str ++ " not supported"

