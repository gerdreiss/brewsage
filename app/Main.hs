module Main where

import           Control.Brew.Maintenance       ( procFormulas )
import           Control.Brew.Usage             ( listFormulasWithDependants )
import           Data.Brew
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
main = tui
-- main = do
--   putStr "Reading formula information... "
--   hFlush stdout
--   start    <- getCurrentTime
--   formulas <- listFormulasWithDependants
--   stop     <- getCurrentTime
--   putStrLn $ "Done. Time: " ++ show (diffUTCTime stop start)
--   procFormulas . rights $ formulas
--   putStrLn . concatMap show . lefts $ formulas
