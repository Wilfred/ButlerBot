module SendEmail where

import Network.Curl (withCurlDo, URLString, initialize, setopt, perform,
                     CurlOption(..))

mailgunPost :: String -> URLString -> [String] -> IO ()
mailgunPost apiKey url params = initialize >>= \ h -> do
  setopt h (CurlVerbose True)
  setopt h (CurlPostFields params)
  setopt h (CurlCookieJar "cookies")
  setopt h (CurlURL url)
  setopt h (CurlUserPwd apiKey)
  perform h
  return ()

mailgunUrl = "https://api.mailgun.net/v2/samples.mailgun.org/messages"

sendEmail :: String -> String -> String -> String -> IO ()
sendEmail mailgunKey to subject body = do
  let from = "from=ButlerBot <butler@bots.wilfred.me.uk>"
      to' = "to=" ++ to
      subject' = "subject=" ++ subject
      body' = "text=" ++ body
  withCurlDo $ mailgunPost mailgunKey mailgunUrl [from, to', subject', body']
