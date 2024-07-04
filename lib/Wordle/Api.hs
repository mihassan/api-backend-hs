{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wordle.Api (WordleRequest, WordleResponse, wordleHandler) where

import Data.Aeson
import GHC.Generics
import Servant
import Wordle.Matcher
import Wordle.Solver
import Wordle.Types
import Wordle.WordBank
import Prelude hiding (Word, words)

data WordleRequest = WordleRequest
  { solver :: Solver,
    attempts :: Attempts
  }
  deriving (Show, Eq, Generic)

instance FromJSON WordleRequest

data WordleResponse = WordleResponse {word :: Word, words :: WordBank} deriving (Show, Eq, Generic)

instance ToJSON WordleResponse

wordleHandler :: WordleRequest -> Handler WordleResponse
wordleHandler WordleRequest {..} = do
  let words = filterWords wordBank attempts
      word = solve solver words
  pure $ WordleResponse {..}
