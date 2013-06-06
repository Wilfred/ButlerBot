{-# LANGUAGE OverloadedStrings #-}
import Numeric
import Network.HTTP.Wget (wget, WgetException)
import Network.Mail.SMTP hiding (Ok, sendMail)
import Network.Mail.Mime (Mail)
import Text.JSON
import Data.Maybe
import Data.Text (pack, unpack, intercalate)
import qualified Data.Text.Lazy
import Control.Monad
import Control.Exception.Base (try)
import System.Environment

newtype Latitude = Latitude Double deriving Show
newtype Longitude = Longitude Double deriving Show

data Location = Location (Latitude, Longitude)
                deriving Show

london :: Location
london = Location (Latitude 51.5265, Longitude 0.0825)

httpGet :: String -> IO (Maybe String)
httpGet url = do
  response <- try (wget url [] []) :: IO (Either WgetException String)
  case response of
    Left _ -> return Nothing
    Right s -> return $ Just s

-- get JSON, returning Nothing for any failures
-- based on http://www.amateurtopologist.com/blog/2010/11/05/a-haskell-newbies-guide-to-text-json/
httpGetJson :: JSON a => String -> IO (Maybe a)
httpGetJson url = do
  httpResult <- httpGet url
  case httpResult of
    Just result ->
      case (decode result) of
        Ok object -> return (Just object)
        Error s -> return Nothing
    Nothing -> return Nothing

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
describeCelsius (Celsius c) = show (round c) ++ " C"

data DayForecast = DayForecast { summary :: String,
                                 minTemp :: Celsius,
                                 maxTemp :: Celsius
                               } deriving (Show)

describeForecast :: DayForecast -> String
describeForecast day =
  summary day ++ " Min: " ++ (describeCelsius $ minTemp day) ++ " Max: " ++ (describeCelsius $ maxTemp day)

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
  return $ DayForecast {summary=summary', minTemp=minTemp, maxTemp=maxTemp}

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
      sendMail "test@example.com" recipient "hello world!" forecastsText
      putStrLn "done!"
    _ -> putStrLn "Usage: <api key> <recipient email>"
