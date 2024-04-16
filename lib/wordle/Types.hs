module Wordle.Types (Letter, Word, WordBank, Feedback (..), WordFeedback, RankingStrategy) where

import Prelude hiding (Word)

type Letter = Char

type Word = [Letter]

type WordBank = [Word]

data Feedback = Correct | Misplaced | Absent deriving (Show, Eq)

type WordFeedback = [Feedback]

type RankingStrategy = Word -> Double
