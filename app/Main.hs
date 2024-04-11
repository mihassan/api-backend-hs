module Main where

import Api
import Network.Wai.Middleware.RequestLogger
import Web.Scotty

main :: IO ()
main = do
  scotty 8080 $ do
    middleware logStdoutDev

    setupRoutes
