module Wordle.Analyzer
  ( analyze,
    difficultWords,
    simulate,
  )
where

import Common.Util
import Wordle.Matcher
import Wordle.Solver
import Wordle.Types
import Prelude hiding (Word)

analyze :: Solver -> WordBank -> AnalysisReport
analyze s wb =
  map (simulate s) wb
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

difficultWords :: Int -> Solver -> WordBank -> [(Word, Int)]
difficultWords n s wb =
  map (simulate s) wb
    |> map length
    |> zip wb
    |> filter ((> n) . snd)

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
