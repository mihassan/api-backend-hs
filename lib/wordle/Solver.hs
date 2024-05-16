module Wordle.Solver (Solver (..), solve) where

import Common.Util
import Data.List.Extra
import Data.Map.Strict qualified as Map
import Wordle.Matcher
import Wordle.Types
import Wordle.WordBank
import Prelude hiding (Word)

solve :: Solver -> WordBank -> Word
solve _ [] = "SALET"
solve _ [w] = w
solve _ [w, _] = w
solve LetterCountSolver wb = maximumOn rank wb
  where
    rank w = letterCount wb w
solve MinimizeMaxPartitionSolver wb = maximumOn rank wordBank
  where
    rank w =
      minMaxPartitionSize wb w
        |> boostGuessIsPossible wb w
solve EvenPartitionSolver wb = maximumOn rank wordBank
  where
    rank w =
      partitionEntropy wb w
        |> boostGuessIsPossible wb w

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

boostGuessIsPossible :: WordBank -> Word -> Double -> Double
boostGuessIsPossible wb w | w `elem` wb = (+ 0.0000001)
boostGuessIsPossible _ _ = id
