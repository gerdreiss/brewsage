module Main where

import           Brewsage2
import           Data.Either (lefts, rights)

main :: IO ()
main = readFormulasWithUsages >>= printUsed
  where
    printUsed = putStrLn . concatMap show . filter (not . null . dependents) . rights
             -- putStrLn . concatMap show . lefts
