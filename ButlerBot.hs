{-# LANGUAGE OverloadedStrings #-}
import Numeric
import Network.Curl
import Network.Mail.SMTP hiding (Ok, sendMail)
import Network.Mail.Mime (Mail)
import Text.JSON
import Data.Maybe
import Data.Text (pack, unpack, intercalate)
import Data.DateTime (fromSeconds, formatDateTime, DateTime)
import qualified Data.Text.Lazy
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
  summary' ++ " Min: " ++ minTemp' ++ " Max: " ++ maxTemp'
  where
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
  summary' <- fromJSString' summary
  min <- getByKey "temperatureMin" json
  min' <- fromJSRational min
  max <- getByKey "temperatureMax" json
  max' <- fromJSRational max
  let minTemp = toCelsius $ Fahrenheit min'
  let maxTemp = toCelsius $ Fahrenheit max'
  time <- getByKey "time" json
  time' <- fromJSRational time
  let time'' = fromSeconds $ round time'
  return $ DayForecast {summary=summary', minTemp=minTemp, maxTemp=maxTemp, time=time''}

forecastIoUrl :: String -> Location -> String
forecastIoUrl apiKey location =
  "https://api.forecast.io/forecast/" ++ apiKey ++ "/" ++ showDouble lat ++ "," ++ showDouble long
  where Location (Latitude lat, Longitude long) = location

toEmail :: String -> String -> String -> String -> Mail
toEmail from to subject body =
  let
    from' = Address Nothing $ pack from
    to' = [Address Nothing $ pack to]
    cc = []
    bcc = []
    body' = plainTextPart $ Data.Text.Lazy.pack body
    subject' = pack subject
  in simpleMail from' to' cc bcc subject' [body']

sendMail :: String -> String -> String -> String -> IO ()
sendMail from to subject body =
  renderSendMail $ toEmail from to subject body

main = do
  args <- getArgs
  case args of
    [apiKey, recipient] -> do
      let url = forecastIoUrl apiKey london
      json <- httpGetJson url
      -- todo: handle Nothings here
      let forecasts = do
            json' <- json
            getForecasts json'
      let forecastsText = case forecasts of
            Just forecasts' -> describeForecasts forecasts'
            Nothing -> "Network or parse error getting forecasts."
      putStrLn forecastsText
      sendMail "butlerbot@wilfred.me.uk" recipient "Weather summary" forecastsText
      putStrLn "done!"
    _ -> putStrLn "Usage: <api key> <recipient email>"
