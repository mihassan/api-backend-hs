{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wordle.Api (service) where

import Common.Service
import Data.Aeson
import GHC.Generics
import Wordle.Matcher
import Wordle.Solver
import Wordle.Types
import Wordle.WordBank
import Prelude hiding (Word)

data Request = Request
  { solver :: Solver,
    attempts :: Attempts
  }
  deriving (Show, Eq, Generic)

instance FromJSON Request

data Response = Response {word :: Word, words :: WordBank} deriving (Show, Eq, Generic)

instance ToJSON Response

rpc :: Rpc
rpc = Rpc "wordle" $ \Request {..} ->
  let words = filterWords wordBank attempts
      word = solve solver words
   in Response {..}

service :: Service
service = Service [rpc]
