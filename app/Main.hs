module Main where

import           Control.Brew.Usage             ( listFormulasWithDependants )
import           Data.Either                    ( lefts
                                                , rights
                                                )
import           Data.Time                      ( diffUTCTime
                                                , getCurrentTime
                                                )
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           Tui.Main                       ( tui )

main :: IO ()
main = do
  putStr "Reading formula information... "
  hFlush stdout
  start    <- getCurrentTime
  formulas <- listFormulasWithDependants
  stop     <- getCurrentTime
  putStrLn $ "Done. Time: " ++ show (diffUTCTime stop start)
  tui . rights $ formulas
  putStrLn . concatMap show . lefts $ formulas
