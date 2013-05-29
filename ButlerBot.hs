import Network.Curl
import Text.JSON
import Data.Maybe

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

weatherSummariesByDay :: JSValue -> Maybe [String]
weatherSummariesByDay json = do
  dailyObject <- getByKey "daily" json
  dailyData <- getByKey "data" dailyObject
  let jsSummaries = jsArrayMap (getByKey "summary") dailyData
  summaries <- fromJSArray $ jsSummaries
  let summaries' = map fromJSString' $ summaries
  return $ catMaybes summaries'

main = putStrLn "Hello, World!"
