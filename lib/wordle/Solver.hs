module Wordle.Solver (Solver (..), solve) where

import Common.Util
import Data.List.Extra
import Data.Map.Strict qualified as Map
import Wordle.Matcher
import Wordle.Types
import Wordle.WordBank
import Prelude hiding (Word)

solve :: Solver -> Attempts -> Word
solve RandomSolver as = filterWords wordBank as |> head
solve NaiveSolver as = solveHelper1 (countUnseenLetters as) as
solve FastSolver1 as = solveHelper1 totalLetterDistribution as
solve FastSolver2 as = solveHelper1 totalLetterEntropy as
solve SlowSolver1 as = solveHelper2 minMaxPartitionSize as
solve SlowSolver2 as = solveHelper2 partitionEntropy as
solve MixedSolver1 as
  | n == 0 = "TRACE"
  | n == 1 = solve NaiveSolver as
  | n == 2 = solve SlowSolver2 as
  | otherwise = solve SlowSolver1 as
  where
    n = length as
solve MixedSolver2 as
  | n == 0 = "TRACE"
  | n == 1 = solve SlowSolver2 as
  | otherwise = solve SlowSolver1 as
  where
    n = length as

solveHelper1 :: RankingStrategy -> Attempts -> Word
solveHelper1 rs as = maximumOn (rs wb) wb
  where
    wb = filterWords wordBank as

solveHelper2 :: RankingStrategy -> Attempts -> Word
solveHelper2 rs as = candidates |> maximumOn (rs wb)
  where
    wb = filterWords wordBank as
    candidates
      | length wb < 3 = wb -- If there are only 2 words left, just return one of them.
      | otherwise = computeCandidates as

computeCandidates :: Attempts -> WordBank
computeCandidates as = wb1 ++ wb2 |> nub
  where
    -- Try all possible guess words.
    wb1 = filterWords wordBank as
    -- Try few more words which may not be the guess word, but could be helpful to partiton possible words into evenly distributed groups.
    wb2 = take 500 . reverse $ sortOn rank wordBank
    d = concat wb1 |> distribution |> Map.map letterSurprise
    letterSurprise = logBase 2 .> negate
    rank = nub .> map (\l -> Map.findWithDefault 0 l d) .> sum

countUnseenLetters :: Attempts -> RankingStrategy
countUnseenLetters as _ w =
  concatMap word as
    |> (nub w \\)
    |> length
    |> fromIntegral

letterDistribution :: Distribution Letter
letterDistribution = distribution $ concat wordBank

totalLetterDistribution :: RankingStrategy
totalLetterDistribution _ w =
  nub w
    |> map toLetterDistribution
    |> sum
  where
    toLetterDistribution l = letterDistribution |> Map.findWithDefault 0 l

letterEntropy :: Distribution Letter
letterEntropy = entropy $ concat wordBank

totalLetterEntropy :: RankingStrategy
totalLetterEntropy _ w =
  nub w
    |> map toLetterEntropy
    |> sum
  where
    toLetterEntropy l = letterEntropy |> Map.findWithDefault 0 l

minMaxPartitionSize :: RankingStrategy
minMaxPartitionSize wb w =
  partitionWords w wb
    |> map (length . snd)
    |> maximum
    |> negate
    |> fromIntegral

partitionEntropy :: RankingStrategy
partitionEntropy wb w =
  partitionWords w wb
    |> map (\(x, y) -> (x, length y))
    |> Map.fromList
    |> entropyFromHistogram
    |> Map.elems
    |> sum
