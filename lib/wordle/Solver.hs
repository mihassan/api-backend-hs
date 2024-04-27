module Wordle.Solver (Solver (..), solve) where

import Common.Util
import Data.List.Extra
import Data.Map.Strict qualified as Map
import Wordle.Matcher
import Wordle.Types
import Wordle.WordBank
import Prelude hiding (Word)

solve :: Solver -> Attempts -> Word
solve RandomSolver as = solveHelper (\_ _ -> 0.0) as
solve NaiveSolver as = solveHelper (countUnseenLetters as) as
solve FastestSolver as = solveHelper totalLetterDistribution as
solve FastSolver as = solveHelper totalLetterEntropy as
solve SlowerSolver as = solveHelper minMaxPartitionSize as
solve SlowestSolver as = solveHelper partitionEntropy as
solve ModerateSolver [] = "SALET"
solve ModerateSolver [a] = solve FastestSolver [a]
solve ModerateSolver as = solve SlowestSolver as

solveHelper :: RankingStrategy -> Attempts -> Word
solveHelper rs as = maximumOn (rs wb) wb
  where
    wb = filterWords wordBank as

countUnseenLetters :: Attempts -> RankingStrategy
countUnseenLetters as _ w =
  concatMap fst as
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
