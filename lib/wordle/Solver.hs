module Wordle.Solver (Solver (..), solve) where

import Common.Util
import Data.List.Extra
import Data.Map.Strict qualified as Map
import Wordle.Matcher
import Wordle.Types
import Wordle.WordBank
import Prelude hiding (Word)

solve :: Solver -> Attempts -> Word
solve RandomSolver as = solveHelper (const 0.0) as
solve NaiveSolver as = solveHelper (countUnseenLetters as) as
solve FastestSolver as = solveHelper totalLetterDistribution as
solve FastSolver as = solveHelper totalLetterEntropy as
solve SlowerSolver as = solveHelper minMaxPartitionSize as
solve SlowestSolver as = solveHelper partitionEntropy as
solve ModerateSolver [] = "SALET"
solve ModerateSolver [a] = solve FastestSolver [a]
solve ModerateSolver as = solve SlowestSolver as

solveHelper :: RankingStrategy -> Attempts -> Word
solveHelper rs as =
  filterWords wordBank as
    |> filter (not . guessed)
    |> maximumOn rs
  where
    gs = fst <$> as
    guessed = (`elem` gs)

countUnseenLetters :: Attempts -> RankingStrategy
countUnseenLetters as w =
  concatMap fst as
    |> (nub w \\)
    |> length
    |> fromIntegral

letterDistribution :: Distribution Letter
letterDistribution = distribution $ concat wordBank

totalLetterDistribution :: RankingStrategy
totalLetterDistribution w =
  nub w
    |> map toLetterDistribution
    |> sum
  where
    toLetterDistribution l = letterDistribution |> Map.findWithDefault 0 l

letterEntropy :: Distribution Letter
letterEntropy = entropy $ concat wordBank

totalLetterEntropy :: RankingStrategy
totalLetterEntropy w =
  nub w
    |> map toLetterEntropy
    |> sum
  where
    toLetterEntropy l = letterEntropy |> Map.findWithDefault 0 l

minMaxPartitionSize :: RankingStrategy
minMaxPartitionSize w =
  partitionWords w wordBank
    |> map (length . snd)
    |> maximum
    |> negate
    |> fromIntegral

partitionEntropy :: RankingStrategy
partitionEntropy w =
  partitionWords w wordBank
    |> map (length . snd)
    |> entropy
    |> Map.elems
    |> sum
