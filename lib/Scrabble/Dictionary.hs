module Scrabble.Dictionary
  ( Dictionary,
    Size (..),
    load,
    prune,
    rank,
  )
where

import Control.Monad
import Data.Aeson
import Data.List.Extra
import GHC.Generics
import Paths_ApiBackend
import Scrabble.Point
import Scrabble.Util

type Dictionary = [String]

data Size = Tiny | Small | Medium | Large | Huge | Full deriving (Eq, Show, Read, Generic)

instance FromJSON Size

load :: Size -> IO Dictionary
load = toPath >=> load'

load' :: FilePath -> IO Dictionary
load' = readFile >=> pure . lines

toPath :: Size -> IO FilePath
toPath size = getDataFileName $ "dictionary/" <> show size <> ".txt"

prune :: String -> Dictionary -> Dictionary
prune s = filter (flip isSubMapOf (histogram s) . histogram)

rank :: Dictionary -> Dictionary
rank = sortOn (negate . wordPoint)
