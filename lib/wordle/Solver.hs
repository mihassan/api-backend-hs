module Wordle.Solver (Solver (..), solve) where

import Common.Util
import Data.List.Extra
import Data.Map.Strict qualified as Map
import Wordle.Matcher
import Wordle.Types
import Prelude hiding (Word)

solve :: Solver -> WordBank -> Word
solve _ [] = "SALET"
solve LetterCountSolver wb = maximumOn (letterCount wb) wb
solve MinimizeMaxPartitionSolver wb = maximumOn (minMaxPartitionSize wb) wb
solve EvenPartitionSolver wb = maximumOn (partitionEntropy wb) wb

letterCount :: WordBank -> Word -> Double
letterCount wb w = nub w |> map f |> sum |> fromIntegral
  where
    f l = concat wb |> histogram |> Map.findWithDefault 0 l

minMaxPartitionSize :: WordBank -> Word -> Double
minMaxPartitionSize wb w =
  partitionWords w wb
    |> map (length . snd)
    |> maximum
    |> negate
    |> fromIntegral

partitionEntropy :: WordBank -> Word -> Double
partitionEntropy wb w =
  partitionWords w wb
    |> map (snd .> length)
    |> entropyFromFreq
