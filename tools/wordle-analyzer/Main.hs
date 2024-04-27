module Main where

import Control.Monad
import System.TimeIt
import Wordle.Analyzer
import Wordle.Solver
import Wordle.Types
import Wordle.WordBank

runAnalysis :: Solver -> WordBank -> IO ()
runAnalysis s wb = do
  let r = analyze s wb
  putStrLn $ "\nAnalysis: " ++ show r
  putStrLn $ "\nDifficult words: " ++ show (difficultWords 5 s wb)

main :: IO ()
main = do
  forM_ [minBound .. maxBound] $ \s -> do
    putStrLn $ "\n\nRunning analysis for solver: " ++ show s
    timeIt $ runAnalysis s wordBank
