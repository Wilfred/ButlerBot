import qualified Data.Text.Lazy
import System.Environment (getArgs)
import System.Exit(exitFailure)

import JsonParsing (httpGetJson)
import SendEmail (sendEmail)
import Weather (forecastIoUrl, getForecasts, describeForecasts, london)

main = do
  args <- getArgs
  case args of
    [forecastIoKey, mailgunKey, recipient] -> do
      let url = forecastIoUrl forecastIoKey london
      json <- httpGetJson url
      -- todo: handle Nothings here
      let forecasts = do
            json' <- json
            getForecasts json'
      let forecastsText = case forecasts of
            Just forecasts' -> describeForecasts forecasts'
            Nothing -> "Network or parse error getting forecasts."
      putStrLn forecastsText
      sendEmail mailgunKey recipient "Weather summary" forecastsText
      putStrLn "done!"
    _ -> do
      putStrLn "Usage: <forecast.io api key> <mailgun credentials e.g. api:key-12345> <recipient email>"
      exitFailure
