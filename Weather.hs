{-# LANGUAGE OverloadedStrings #-}

module Weather where

import Numeric (showFFloat)
import Text.JSON (JSValue)
import Data.Maybe (mapMaybe)
import Data.DateTime (fromSeconds, formatDateTime, DateTime)
import Data.Text (pack, unpack, intercalate)

import JsonParsing (httpGetJson, getByKey,
                    fromJSRational, fromJSString, fromJSArray)

newtype Latitude = Latitude Double deriving Show
newtype Longitude = Longitude Double deriving Show

data Location = Location (Latitude, Longitude)
                deriving Show

london :: Location
london = Location (Latitude 51.5265, Longitude 0.0825)

-- doesn't use scientific notation
showDouble :: Double -> String
showDouble d = showFFloat Nothing d ""

data Fahrenheit = Fahrenheit Double deriving (Show, Eq, Ord)
data Celsius = Celsius Double deriving (Show, Eq, Ord)

toCelsius :: Fahrenheit -> Celsius
toCelsius (Fahrenheit f) = Celsius $ (f - 32) * (5 / 9)

toFahrenheit :: Celsius -> Fahrenheit
toFahrenheit (Celsius c) = Fahrenheit $ c * 9 / 5 + 32

describeCelsius :: Celsius -> String
describeCelsius (Celsius c) =
  show (round c) ++ " C (" ++ show (round f) ++ " F)"
  where
    (Fahrenheit f) = toFahrenheit (Celsius c)

data DayForecast = DayForecast { summary :: String,
                                 time :: DateTime,
                                 minTemp :: Celsius,
                                 maxTemp :: Celsius
                               } deriving (Show)

describeForecast :: DayForecast -> String
describeForecast f =
  day ++ ": " ++ summary' ++ " Min: " ++ minTemp' ++ " Max: " ++ maxTemp'
  where
    day = formatDateTime "%A" $ time f
    summary' = summary f
    minTemp' = describeCelsius $ minTemp f
    maxTemp' = describeCelsius $ maxTemp f

describeForecasts :: [DayForecast] -> String
describeForecasts days =
  unpack $ intercalate (pack "\n") $ map (pack . describeForecast) $ take 3 days

getForecasts :: JSValue -> Maybe [DayForecast]
getForecasts json = do
  dailyObject <- getByKey "daily" json
  dailyData <- getByKey "data" dailyObject
  dailyData' <- fromJSArray dailyData
  return $ mapMaybe getForecast dailyData'

getForecast :: JSValue -> Maybe DayForecast
getForecast json = do
  summary <- getByKey "summary" json
  summary' <- fromJSString summary
  min <- getByKey "temperatureMin" json
  min' <- fromJSRational min
  max <- getByKey "temperatureMax" json
  max' <- fromJSRational max
  let minTemp = toCelsius $ Fahrenheit min'
  let maxTemp = toCelsius $ Fahrenheit max'
  time <- getByKey "time" json
  time' <- fromJSRational time
  -- Timestamps from forecast.io are in unix time, so UTC.
  -- In the summer, midnight is 11pm yesterday. Since we're only
  -- interested in days anyway, we add an hour to make sure we
  -- have the right day. TODO: Use Dates instead.
  let time'' = fromSeconds $ round time' + 60 * 60
  return DayForecast {summary=summary', minTemp=minTemp, maxTemp=maxTemp, time=time''}

forecastIoUrl :: String -> Location -> String
forecastIoUrl apiKey location =
  "https://api.forecast.io/forecast/" ++ apiKey ++ "/" ++ showDouble lat ++ "," ++ showDouble long
  where Location (Latitude lat, Longitude long) = location
