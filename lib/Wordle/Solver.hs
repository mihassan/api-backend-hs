module Wordle.Solver (Solver (..), solve) where

import Common.Util
import Data.List.Extra
import Data.Map.Strict qualified as Map
import Wordle.Matcher
import Wordle.Types
import Wordle.WordBank
import Prelude hiding (Word)

solve :: Solver -> WordBank -> Word
solve _ [] = error "Empty word bank"
solve _ [w] = w
solve _ [w, _] = w
solve LetterCountHard wb = maximumOn rank wb
  where
    rank w = letterCount wb w
solve LetterCountByPositionHard wb = maximumOn rank wb
  where
    rank w = letterCountByPosition wb w
solve LetterEntropyHard wb = maximumOn rank wb
  where
    rank w = letterEntropy wb w
solve MinMaxPartitionHard wb = maximumOn rank wb
  where
    rank w = minMaxPartitionSize wb w
solve EvenPartitionHard wb = maximumOn rank wb
  where
    rank w = partitionEntropy wb w
solve MinMaxPartitionSlow wb = maximumOn rank wordBank
  where
    rank w =
      minMaxPartitionSize wb w
        |> boostGuessIsPossible wb w
solve EvenPartitionSlow wb = maximumOn rank wordBank
  where
    rank w =
      partitionEntropy wb w
        |> boostGuessIsPossible wb w

letterCount :: WordBank -> Word -> Double
letterCount wb w =
  nub w
    |> map f
    |> sum
    |> fromIntegral
  where
    h = concat wb |> histogram
    f x = Map.findWithDefault 0 x h

letterCountByPosition :: WordBank -> Word -> Double
letterCountByPosition wb w =
  zip [0 :: Int ..] w
    |> map f
    |> sum
    |> fromIntegral
  where
    h = concat wb |> zip [0 :: Int ..] |> histogram
    f x = Map.findWithDefault 0 x h

letterEntropy :: WordBank -> Word -> Double
letterEntropy wb w =
  nub w
    |> map f
    |> sum
  where
    h = map g ['A' .. 'Z'] |> map entropyFromFreq |> zip ['A' .. 'Z'] |> Map.fromList
    g x = map (elem x) wb |> histogram |> Map.elems
    f x = Map.findWithDefault 0 x h

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
