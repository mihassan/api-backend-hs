{-# LANGUAGE RecordWildCards #-}

module Wordle.Matcher (checkGuess, filterWords, partitionWords) where

import Common.Util
import Data.List ((\\))
import Data.List.Extra (groupSort)
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

filterWords :: WordBank -> Attempts -> WordBank
filterWords = foldr filterWordsForAttempt

filterWordsForAttempt :: Attempt -> WordBank -> WordBank
filterWordsForAttempt Attempt {..} = filter (\target -> checkGuess target word == feedback)

partitionWords :: Word -> WordBank -> [(WordFeedback, WordBank)]
partitionWords w wb =
  map (`checkGuess` w) wb
    |> flip zip wb
    |> groupSort
