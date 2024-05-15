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
solve FastSolver1 wb = maximumOn (letterCount wb) wb
solve SlowSolver1 wb = solveHelper minMaxPartitionSize wb
solve SlowSolver2 wb = solveHelper partitionEntropy wb

solveHelper :: (WordBank -> Word -> Double) -> WordBank -> Word
solveHelper rs wb = candidates |> maximumOn (rs wb)
  where
    candidates
      | length wb < 3 = wb -- If there are only 2 words left, just return one of them.
      | otherwise = computeCandidates wb

computeCandidates :: WordBank -> WordBank
computeCandidates wb = wb ++ wb' |> nub
  where
    -- Try few more words which may not be the guess word, but could be helpful to partiton possible words into evenly distributed groups.
    wb' = take 500 . reverse $ sortOn rank wordBank
    d = concat wb |> distribution |> Map.map surprise
    rank = nub .> map (\l -> Map.findWithDefault 0 l d) .> sum

letterCount :: WordBank -> Word -> Double
letterCount wb w = nub w |> map f |> sum
  where
    f l = concat wb |> distribution |> Map.findWithDefault 0 l

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
