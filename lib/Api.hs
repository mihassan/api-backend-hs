{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Api (setupRoutes) where

import Common.Service
import Hello.Api qualified as Hello
import Network.HTTP.Types.Status
import Web.Scotty
import Wordle.Api qualified as Wordle

service :: Service
service = Hello.service <> Wordle.service

setupRoutes :: ScottyM ()
setupRoutes = do
  matchAny "/api/:rpc" $ do
    rpc <- pathParam "rpc"
    case findRpc service rpc of
      Just Rpc {..} -> jsonData >>= json . handler
      _ -> status status404
