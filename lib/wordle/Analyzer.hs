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

analyze :: Solver -> Word -> WordBank -> AnalysisReport
analyze s g wb =
  map (simulate s g) wb
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

difficultWords :: Int -> Solver -> Word -> WordBank -> [(Word, Int)]
difficultWords n s g wb =
  map (simulate s g) wb
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

simulate :: Solver -> Word -> Word -> Attempts
simulate s g w = go [initialAttempt] |> reverse
  where
    initialAttempt = Attempt {word = g, feedback = checkGuess w g}
    mkAttempt :: Word -> Attempt
    mkAttempt x = Attempt {word = x, feedback = checkGuess w x}
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
