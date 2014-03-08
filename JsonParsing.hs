module JsonParsing where

import Data.Maybe (fromJust, mapMaybe)
import Network.Curl (curlGetString, CurlCode, URLString)
import Text.JSON (JSON, decode, Result(Ok, Error),
                  JSValue(JSArray, JSObject, JSString, JSRational),
                  fromJSObject, fromJSString)
import qualified Text.JSON as JSON


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

fromJSString :: JSValue -> Maybe String
fromJSString (JSString s) = Just $ JSON.fromJSString s
fromJSString _ = Nothing

fromJSRational :: JSValue -> Maybe Double
fromJSRational (JSRational _ ratio) = Just $ fromRational ratio
fromJSRational _ = Nothing
