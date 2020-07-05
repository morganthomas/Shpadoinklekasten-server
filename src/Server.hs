{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}


module Main where


import           Control.Monad.IO.Class
import           Data.Proxy
import           Data.Text
import           Database.MongoDB
import           Network.Socket (PortNumber)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant.API
import           Servant.Server
import           Shpadoinkle.Router.Server
import           Shpadoinkle (JSM)

import           Types
import           View
import           Persist
import           SHA256


api :: Pipe -> Server API
api conn = hoistServer (Proxy @API) (liftIO . access conn UnconfirmedWrites "s11kasten")
           (saveChange :<|> getDatabase :<|> login)


app :: Text -> Server SPA
app js = serveUI @SPA "./assets" (fmap (template js) . viewRouter @App) routes


server :: Text -> Pipe -> Application
server js conn = serve (Proxy @(SPA :<|> API)) $ app js :<|> api conn


main :: IO ()
main = do
  js <- readFile "./assets/all.js"
  conn <- connect (Host "localhost" (PortNumber 27017))
  putStrLn "listening on port 8080"
  run 8080 (server (sha256_js <> pack js) conn)
