{-# LANGUAGE RecordWildCards #-}

module Bananagram.Api (BananagramRequest, BananagramResponse, bananagramHandler) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Char
import GHC.Generics
import Bananagram.Dictionary
import Bananagram.Grid
import Bananagram.Solver
import Servant

data BananagramRequest = BananagramRequest
  { dictionary :: Size,
    letters :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON BananagramRequest

newtype BananagramResponse = BananagramResponse {grid :: RenderedGrid} deriving (Show, Eq, Generic)

instance ToJSON BananagramResponse

handlerIO :: BananagramRequest -> IO BananagramResponse
handlerIO BananagramRequest {..} = do
  let dict = load dictionary
  let letters' = filter isAlpha $ toUpper <$> letters
  let grid = case solve dict letters' of
        Just chain -> render $ chainToGrid chain
        Nothing -> []
  pure $ BananagramResponse {..}

bananagramHandler :: BananagramRequest -> Handler BananagramResponse
bananagramHandler = liftIO . handlerIO
