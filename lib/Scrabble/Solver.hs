module Scrabble.Solver (solve) where

import Data.List
import Data.List.Extra
import Scrabble.Dictionary
import Scrabble.Util

-- Available letters to construct scrabble words
type Letters = String

-- A chain of words that can be constructed from the available letters
-- Each word relates to the next word by sharing its last character
type Chain = [String]

-- solve constructs a chain of words from the available letters
solve :: Dictionary -> Letters -> Maybe Chain
solve dict letters = update dict letters []

-- update recursively constructs a chain of words from the available letters given a partial chain
update :: Dictionary -> Letters -> Chain -> Maybe Chain
update dict letters chain = reverse <$> firstJust go [0 .. length chain]
  where
    dict' = rank dict
    go = (\(ls, ch) -> loop dict' ls (reverse ch)) . dropChainEnd letters chain

-- dropChainEnd removes some words from the end of the chain and adds the shared letters to the available letters
dropChainEnd :: Letters -> Chain -> Int -> (Letters, Chain)
dropChainEnd letters [] _ = (letters, [])
dropChainEnd letters chain@((start : _) : _) n = (letters', chain')
  where
    (chain', xs) = splitAt (length chain - n) chain
    ys = concatMap (drop 1) xs -- remove the first letter of each word as it is shared with the previous word
    letters' =
      if null chain'
        then start : ys ++ letters -- edge case when the entire chain is removed, we need to put the first letter of the first word back
        else ys ++ letters
dropChainEnd _ chain _ = error $ "dropChainEnd: invalid chain containing an empty word" ++ show chain

-- Loop recursively constructs a chain of words from the available letters
-- dict: the dictionary of words that can be constructed from the available letters
-- letters: the available letters besides the ones in the chain
-- chain: the chain of words that can be constructed from the available letters in reverse order
loop :: Dictionary -> Letters -> Chain -> Maybe Chain
-- Found a solution when there are no more available letters
loop _ [] chain = Just chain
-- First step is straightforward, try all words in the dictionary
loop dict letters [] = firstJust go dict'
  where
    dict' = prune letters dict
    go w = loop dict (letters \\ w) [w]
loop dict letters chain@(lastWord : _) = firstJust go candidates
  where
    sharedLetter = last lastWord -- The shared letter between the last word in the chain and the next word
    letters' = sharedLetter : letters -- Add the shared letter to the available letters
    dict' = prune letters' dict
    candidates = take 4 $ filter (startsWith sharedLetter) dict' -- Only consider words that start with the shared letter
    go w = loop dict' (letters' \\ w) (w : chain)
