import Network.Curl (curlPost, withCurlDo)



sendEmail :: String -> String -> IO ()
sendEmail subject body = do
  withCurlDo $ curlPost "http://localhost:8000" []
