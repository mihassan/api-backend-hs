{-# LANGUAGE RecordWildCards #-}

module Common.Service (Service (..), Rpc (..), findRpc) where

import Data.Aeson
import Data.List
import Data.Text (Text)

data Service = Service
  { rpcs :: [Rpc]
  }

instance Semigroup Service where
  Service rpcs1 <> Service rpcs2 = Service (rpcs1 <> rpcs2)

instance Monoid Service where
  mempty = Service []

data Rpc = forall req res. (FromJSON req, ToJSON res) => Rpc
  { name :: Text,
    handler :: req -> res
  }

findRpc :: Service -> Text -> Maybe Rpc
findRpc Service {..} rpc = find (\Rpc {..} -> name == rpc) rpcs
