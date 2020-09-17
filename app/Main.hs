module Main where

import           Control.Brew.Maintenance       ( procFormulas )
import           Control.Brew.Usage             ( listFormulasWithDependants )
import           Data.Either                    ( lefts
                                                , rights
                                                )
import           Data.Time                      ( diffUTCTime
                                                , getCurrentTime
                                                )
import           System.Environment             ( getArgs )
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
  putStrLn "Done."
  putStrLn ("Time elapsed: " ++ show (diffUTCTime stop start))
  args <- getArgs
  case args of
    ("--tui" : _) -> tui . rights $ formulas
    _             -> procFormulas . rights $ formulas
  putStrLn . concatMap show . lefts $ formulas
