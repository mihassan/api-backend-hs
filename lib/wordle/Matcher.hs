{-# LANGUAGE ParallelListComp #-}

module Wordle.Matcher (checkGuess, filterWords) where

import Data.List ((\\))
import Wordle.Types
import Prelude hiding (Word)

checkGuess :: Word -> Word -> WordFeedback
checkGuess target guess = go target guess remainingLetters
  where
    pairs = zip target guess
    correctLetters = fst <$> filter (uncurry (==)) pairs
    remainingLetters = target \\ correctLetters
    go :: Word -> Word -> Word -> WordFeedback
    go (t : ts) (g : gs) remaining
      | t == g = Correct : go ts gs remaining
      | g `elem` remaining = Misplaced : go ts gs (remaining \\ [g])
      | otherwise = Absent : go ts gs remaining
    go _ _ _ = []

filterWords :: WordBank -> [(Word, WordFeedback)] -> WordBank
filterWords = foldr (uncurry filterWordsForSingleGuess)

filterWordsForSingleGuess :: Word -> WordFeedback -> WordBank -> WordBank
filterWordsForSingleGuess guess feedback = filter (\word -> checkGuess word guess == feedback)
