module Wordle.Solver (Solver (..), solve) where

import Common.Util
import Data.Function (on)
import Data.List (nub, sortBy)
import Data.Map.Strict qualified as Map
import Wordle.Matcher
import Wordle.Types
import Wordle.WordBank
import Prelude hiding (Word)

data Solver = RandomSolver | SimpleSolver | FastSolver | BestSolver | SmartSolver deriving (Show, Eq)

data CandidateWords = AllWords | FilteredWords deriving (Show, Eq)

solve :: Solver -> [(Word, WordFeedback)] -> Word
solve RandomSolver = solveHelper FilteredWords $ const 0
solve SimpleSolver = solveHelper FilteredWords rankByLetterFrequency
solve FastSolver = undefined
solve BestSolver = undefined
solve SmartSolver = undefined

solveHelper :: CandidateWords -> RankingStrategy -> [(Word, WordFeedback)] -> Word
solveHelper candidateWords rankingStrategy guesses =
  head . filter (notYetGuessed guesses) . reverse . sortBy (compare `on` rankingStrategy) $
    case candidateWords of
      AllWords -> wordBank
      FilteredWords -> filterWords wordBank guesses

notYetGuessed :: [(Word, WordFeedback)] -> Word -> Bool
notYetGuessed guesses word = word `notElem` (fst <$> guesses)

rankByLetterFrequency :: RankingStrategy
rankByLetterFrequency word = sum $ distForLetter <$> nub word
  where
    dist = distribution $ concat wordBank
    distForLetter letter = Map.findWithDefault 0 letter dist
