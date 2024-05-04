{-# LANGUAGE RecordWildCards #-}

module Wordle.Analyzer
  ( analyze,
    difficultWords,
    totalCostForSolver,
    simulate,
  )
where

import Common.Util
import Data.List
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

totalCostForSolver :: Solver -> WordBank -> Attempts -> Int
totalCostForSolver _ [] _ = 0
totalCostForSolver _ [_] _ = 1
totalCostForSolver _ [_, _] _ = 3
totalCostForSolver s wb as = guessCost + sum costs
  where
    guess = solve s as
    guessCost = if guess `elem` wb then 1 else 0
    costs = (wb \\ [guess]) |> partitionWords guess |> map go
    go (fb, wb') =
      let as' = Attempt guess fb : as
       in length wb' + totalCostForSolver s wb' as'

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
