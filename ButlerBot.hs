import Network.Curl
import Text.JSON

newtype Latitude = Latitude Double deriving Show
newtype Longitude = Longitude Double deriving Show

data Location = Location (Latitude, Longitude)
                deriving Show

london :: Location
london = Location (Latitude 51.5265, Longitude 0.0825)

httpGet :: URLString -> IO (CurlCode, String)
httpGet url = curlGetString url []

-- get JSON, returning Right for any failures
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

-- get the value in a JSON array at this index
getByIndex :: Int -> JSValue -> Maybe JSValue
getByIndex index (JSArray arr) = return (arr !! index)
getByIndex _ _ = Nothing

-- todaysWeather :: JSObject JSValue -> String
todaysWeather json =
  getFirst keyValues dailyEntries
  where keyValues = fromJSObject json
        dailyEntries (key, value) =
          case key of
            "daily" -> True
            _ -> False

main = putStrLn "Hello, World!"
