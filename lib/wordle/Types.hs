module Wordle.Types (Letter, Word, WordBank, Feedback (..), WordFeedback, Attempt, Attempts, Solver (..), RankingStrategy) where

import Prelude hiding (Word)

type Letter = Char

type Word = [Letter]

type WordBank = [Word]

data Feedback = Correct | Misplaced | Absent deriving (Show, Eq, Ord)

type WordFeedback = [Feedback]

type Attempt = (Word, WordFeedback)

type Attempts = [Attempt]

data Solver
  = RandomSolver
  | NaiveSolver
  | FastestSolver
  | FastSolver
  | BetterSolver
  | BestSolver
  | SmartSolver
  deriving (Show, Eq)

type RankingStrategy = Word -> Double
