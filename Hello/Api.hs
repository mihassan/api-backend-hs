{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hello.Api (service) where

import Common.Service
import Data.Aeson
import Data.Text
import GHC.Generics
import Prelude hiding (id)

data Request = Request {id :: Int} deriving (Show, Eq, Generic)

instance FromJSON Request

data Response = Response {message :: Text} deriving (Show, Eq, Generic)

instance ToJSON Response

rpc :: Rpc
rpc = Rpc "hello" $ \Request {..} -> Response {message = "Hello, World! " <> (pack . show $ id)}

service :: Service
service = Service [rpc]
