{-# LANGUAGE TemplateHaskell #-}

module Bananagram.Dictionary
  ( Dictionary,
    Size (..),
    load,
    prune,
    rank,
  )
where

import Data.Aeson
import Data.ByteString.Char8 qualified as C
import Data.FileEmbed
import Data.List.Extra
import Data.Maybe
import GHC.Generics
import Bananagram.Point
import Bananagram.Util

dictionaryFiles :: [(FilePath, C.ByteString)]
dictionaryFiles = $(embedDir "data/dictionary")

type Dictionary = [String]

data Size = Tiny | Small | Medium | Large | Huge | Full deriving (Eq, Show, Read, Generic)

instance FromJSON Size

load :: Size -> Dictionary
load size =
  let fp = show size <> ".txt"
      content = C.unpack . fromJust $ lookup fp dictionaryFiles
   in lines content

prune :: String -> Dictionary -> Dictionary
prune s = filter (flip isSubMapOf (histogram s) . histogram)

rank :: Dictionary -> Dictionary
rank = sortOn (negate . wordPoint)
