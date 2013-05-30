{-# LANGUAGE OverloadedStrings #-}
import Numeric
import Network.Curl
import Network.Mail.SMTP hiding (Ok, sendMail)
import Text.JSON
import Data.Maybe
import Data.Text.Lazy (pack)
import Control.Monad
import System.Environment

newtype Latitude = Latitude Double deriving Show
newtype Longitude = Longitude Double deriving Show

data Location = Location (Latitude, Longitude)
                deriving Show

london :: Location
london = Location (Latitude 51.5265, Longitude 0.0825)

httpGet :: URLString -> IO (CurlCode, String)
httpGet url = curlGetString url []

-- get JSON, returning Nothing for any failures
-- based on http://www.amateurtopologist.com/blog/2010/11/05/a-haskell-newbies-guide-to-text-json/
httpGetJson :: JSON a => URLString -> IO (Maybe a)
httpGetJson url = do
  (code, result) <- httpGet url
  case (decode result) of
    Ok object -> return (Just object)
    Error s -> return Nothing

-- get the value in a JSON object that has this key
getByKey :: String -> JSValue -> Maybe JSValue
getByKey key (JSObject obj) = lookup key (fromJSObject obj)
getByKey _ _ = Nothing

-- TODO: why can't I write `fromJust . getByKey`?
getByKey' key obj = fromJust $ getByKey key obj

-- get the value in a JSON array at this index
getByIndex :: Int -> JSValue -> Maybe JSValue
getByIndex index (JSArray arr) = return (arr !! index)
getByIndex _ _ = Nothing

-- map over this JS Array, if it's an array
jsArrayMap :: (JSValue -> Maybe JSValue) -> JSValue -> JSValue
jsArrayMap f (JSArray arr) = JSArray (mapMaybe f arr)
jsArrayMap _ jsValue = jsValue

fromJSArray :: JSValue -> Maybe [JSValue]
fromJSArray (JSArray arr) = Just arr
fromJSArray _ = Nothing

-- fromJSString :: JSValue -> Maybe 
fromJSString' (JSString s) = Just $ fromJSString s
fromJSString' _ = Nothing

fromJSRational :: JSValue -> Maybe Double
fromJSRational (JSRational _ ratio) = Just $ fromRational ratio
fromJSRational _ = Nothing

-- doesn't use scientific notation
showDouble :: Double -> String
showDouble d = showFFloat Nothing d ""

data Fahrenheit = Fahrenheit Double deriving (Show, Eq, Ord)
data Celsius = Celsius Double deriving (Show, Eq, Ord)

toCelsius :: Fahrenheit -> Celsius
toCelsius (Fahrenheit f) = Celsius $ (f - 32) * (5 / 9)

toFahrenheit :: Celsius -> Fahrenheit
toFahrenheit (Celsius c) = Fahrenheit $ c * 9 / 5 + 32

data DayForecast = DayForecast { summary :: String,
                                 minTemp :: Celsius,
                                 maxTemp :: Celsius
                               } deriving (Show)

getForecasts :: JSValue -> Maybe [DayForecast]
getForecasts json = do
  dailyObject <- getByKey "daily" json
  dailyData <- getByKey "data" dailyObject
  dailyData' <- fromJSArray dailyData
  return $ mapMaybe getForecast dailyData'

getForecast :: JSValue -> Maybe DayForecast
getForecast json = do
  summary <- getByKey "summary" json
  summary' <- fromJSString' summary
  min <- getByKey "temperatureMin" json
  min' <- fromJSRational min
  max <- getByKey "temperatureMax" json
  max' <- fromJSRational max
  let minTemp = toCelsius $ Fahrenheit min'
  let maxTemp = toCelsius $ Fahrenheit max'
  return $ DayForecast {summary=summary', minTemp=minTemp, maxTemp=maxTemp}

forecastIoUrl :: String -> Location -> String
forecastIoUrl apiKey location =
  "https://api.forecast.io/forecast/" ++ apiKey ++ "/" ++ showDouble lat ++ "," ++ showDouble long
  where Location (Latitude lat, Longitude long) = location

toEmail from to subject body =
  let
    from' = Address Nothing from
    to' = [Address Nothing to]
    cc = []
    bcc = []
    body' = plainTextPart body
  in simpleMail from' to' cc bcc subject [body']

sendMail from to subject body =
  renderSendMail $ toEmail from to subject body

main = do
  args <- getArgs
  case args of
    [apiKey] -> do
      let url = forecastIoUrl apiKey london
      json <- httpGetJson url
      -- todo: handle Nothings here
      let forecasts = do
            json' <- json
            getForecasts json'
      let forecastsText = case forecasts of
            Just forecasts' -> show forecasts'
            Nothing -> "Network or parse error getting forecasts."
      putStrLn forecastsText
      sendMail "test@example.com" "root@localhost" "hello world!" $ pack forecastsText
      putStrLn "done!"
    _ -> putStrLn "Need an API key for forecast.io"
