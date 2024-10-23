{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hello.Api
import Network.Wai.Handler.Warp
import Scrabble.Api
import Servant
import System.Environment
import Wordle.Api

type API =
  "api" :> "hello" :> ReqBody '[JSON] HelloRequest :> Post '[JSON] HelloResponse
    :<|> "api" :> "wordle" :> ReqBody '[JSON] WordleRequest :> Post '[JSON] WordleResponse
    :<|> "api" :> "scrabble" :> ReqBody '[JSON] ScrabbleRequest :> Post '[JSON] ScrabbleResponse
    :<|> Raw

server :: Server API
server = helloHandler :<|> wordleHandler :<|> scrabbleHandler :<|> serveDirectoryWebApp "static"

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
  port <- maybe 8080 read <$> lookupEnv "PORT"
  putStrLn $ "Starting Server...\nListening on port " <> show port
  run port app
