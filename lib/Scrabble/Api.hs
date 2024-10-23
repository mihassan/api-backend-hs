{-# LANGUAGE RecordWildCards #-}

module Scrabble.Api (ScrabbleRequest, ScrabbleResponse, scrabbleHandler) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Char
import GHC.Generics
import Scrabble.Dictionary
import Scrabble.Grid
import Scrabble.Solver
import Servant

data ScrabbleRequest = ScrabbleRequest
  { dictionary :: Size,
    letters :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON ScrabbleRequest

newtype ScrabbleResponse = ScrabbleResponse {grid :: RenderedGrid} deriving (Show, Eq, Generic)

instance ToJSON ScrabbleResponse

handlerIO :: ScrabbleRequest -> IO ScrabbleResponse
handlerIO ScrabbleRequest {..} = do
  dict <- load dictionary
  let letters' = filter isAlpha $ toUpper <$> letters
  let grid = case solve dict letters' of
        Just chain -> render $ chainToGrid chain
        Nothing -> []
  pure $ ScrabbleResponse {..}

scrabbleHandler :: ScrabbleRequest -> Handler ScrabbleResponse
scrabbleHandler = liftIO . handlerIO
