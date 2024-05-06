{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wordle.Api (service) where

import Common.Service
import Data.Aeson
import GHC.Generics
import Wordle.Solver
import Wordle.Types
import Prelude hiding (Word)

data Request = Request
  { solver :: Solver,
    attempts :: Attempts
  }
  deriving (Show, Eq, Generic)

instance FromJSON Request

data Response = Response {word :: Word} deriving (Show, Eq, Generic)

instance ToJSON Response

rpc :: Rpc
rpc = Rpc "wordle" $ \Request {..} -> Response {word = solve solver attempts}

service :: Service
service = Service [rpc]
