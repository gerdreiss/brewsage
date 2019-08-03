module Main where

import           Control.Brew.Maintenance (askDeleteFormulas)
import           Control.Brew.Usage       (readFormulasWithUsages)

import           Data.Brew
import           Data.Either              (lefts, rights)
import           Data.Time

main :: IO ()
main = do
  start <- getCurrentTime
  formulas <- readFormulasWithUsages
  stop <- getCurrentTime
  askDeleteFormulas . rights $ formulas
  putStrLn . concatMap show . lefts $ formulas
  print $ diffUTCTime stop start
