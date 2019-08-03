module Main where

import           Control.Brew.Usage (readFormulasWithUsages)

import           Data.Brew
import           Data.Either        (lefts, rights)
import           Data.Time

main :: IO ()
main = do
  start <- getCurrentTime

  readFormulasWithUsages >>= printUsed

  stop <- getCurrentTime

  print $ diffUTCTime stop start

  where
    printUsed = putStrLn . concatMap show . filter (null . dependents) . rights
             -- putStrLn . concatMap show . lefts
