module Wordle.Analyzer (analyze, simulate) where

import Common.Util
import Wordle.Matcher
import Wordle.Solver
import Wordle.Types
import Wordle.WordBank
import Prelude hiding (Word)

analyze :: Solver -> AnalysisReport
analyze s =
  map (simulate s) (take 10000 wordBank)
    |> map length
    |> histogram
    |> mkAnalysisReport

mkAnalysisReport :: Histogram Int -> AnalysisReport
mkAnalysisReport h =
  AnalysisReport
    { histogramOfAttempts = h,
      avgAttempts = histAverage h,
      maxAttempts = histMax h,
      minAttempts = histMin h
    }

simulate :: Solver -> Word -> Attempts
simulate s w = go [("TRACE", checkGuess w "TRACE")] |> reverse
  where
    go :: Attempts -> Attempts
    go as
      | isSolved as = as
      | otherwise = go $ (g, checkGuess w g) : as
      where
        g = solve s as
        isSolved [] = False
        isSolved ((_, f) : _) = all (== Correct) f
