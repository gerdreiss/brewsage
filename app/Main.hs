module Main where

import           Control.Brew.Maintenance       ( procFormulas )
import           Control.Brew.Usage             ( listFormulas
                                                , listFormulasComplete
                                                )
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
  args <- getArgs
  case args of
    ("--tui" : _) -> do
      formulas <- listFormulas
      tui . rights $ formulas
    _             -> do
      putStr "Reading formula information... "
      hFlush stdout
      start    <- getCurrentTime
      formulas <- listFormulasComplete
      stop     <- getCurrentTime
      putStrLn "Done."
      putStrLn ("Time elapsed: " ++ show (diffUTCTime stop start))
      procFormulas . rights $ formulas
      putStrLn . concatMap show . lefts $ formulas
