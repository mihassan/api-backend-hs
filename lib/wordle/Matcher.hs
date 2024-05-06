{-# LANGUAGE RecordWildCards #-}

module Wordle.Matcher
  ( checkGuess,
    filterWords,
    partitionWords,
  )
where

import Common.Util ((.>), (|>))
import Data.List ((\\))
import Data.List.Extra (groupSort)
import Wordle.Types (Attempt (..), Attempts, Feedback (..), Word, WordBank, WordFeedback)
import Prelude hiding (Word)

-- | Given a target word and a guess, return the feedback on the guess.
checkGuess ::
  -- | The target hidden word that the player is trying to guess
  Word ->
  -- | The player's guess
  Word ->
  -- | The feedback on the player's guess
  WordFeedback
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

-- | Given a word bank and a list of attempts, filter out words that are not possible given the feedback.
filterWords :: WordBank -> Attempts -> WordBank
filterWords = foldr (match .> filter)
  where
    match :: Attempt -> Word -> Bool
    match Attempt {..} w = checkGuess w word == feedback

-- | Given a word and a word bank, partition the word bank into groups based on the feedback.
partitionWords :: Word -> WordBank -> [(WordFeedback, WordBank)]
partitionWords w wb =
  map (`checkGuess` w) wb
    |> flip zip wb
    |> groupSort
