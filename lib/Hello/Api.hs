{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hello.Api (HelloRequest, HelloResponse, helloHandler) where

import Data.Aeson
import Data.Text
import GHC.Generics
import Servant
import Prelude hiding (id)

data HelloRequest = HelloRequest {id :: Int} deriving (Show, Eq, Generic)

instance FromJSON HelloRequest

data HelloResponse = HelloResponse {message :: Text} deriving (Show, Eq, Generic)

instance ToJSON HelloResponse

helloHandler :: HelloRequest -> Handler HelloResponse
helloHandler HelloRequest {..} = return $ HelloResponse {message = "Hello, World! " <> (pack . show $ id)}
