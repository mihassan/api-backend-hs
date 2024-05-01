{-# LANGUAGE RecordWildCards #-}

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
simulate s w = go [initialAttempt] |> reverse
  where
    initialAttempt = Attempt {word = "TRACE", feedback = checkGuess w "TRACE"}
    mkAttempt :: Word -> Attempt
    mkAttempt g = Attempt {word = g, feedback = checkGuess w g}
    go :: Attempts -> Attempts
    go as
      | isSolved as = as
      | otherwise =
          solve s as
            |> mkAttempt
            |> (: as)
            |> go

isSolved :: Attempts -> Bool
isSolved [] = False
isSolved (Attempt {..} : _) = all (== Correct) feedback
