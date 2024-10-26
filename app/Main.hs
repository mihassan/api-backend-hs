{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hello.Api
import Network.Wai.Handler.Warp
import Bananagram.Api
import Servant
import System.Environment
import Wordle.Api

type API =
  "api" :> "hello" :> ReqBody '[JSON] HelloRequest :> Post '[JSON] HelloResponse
    :<|> "api" :> "wordle" :> ReqBody '[JSON] WordleRequest :> Post '[JSON] WordleResponse
    :<|> "api" :> "bananagram" :> ReqBody '[JSON] BananagramRequest :> Post '[JSON] BananagramResponse
    :<|> Raw

server :: Server API
server = helloHandler :<|> wordleHandler :<|> bananagramHandler :<|> serveDirectoryWebApp "static"

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
  port <- maybe 8080 read <$> lookupEnv "PORT"
  putStrLn $ "Starting Server...\nListening on port " <> show port
  run port app
