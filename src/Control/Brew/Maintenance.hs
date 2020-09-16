module Control.Brew.Maintenance
  ( procFormulas
  , procFormula
  )
where

import           Data.Brew                      ( Answer(..)
                                                , BrewFormula(..)
                                                )
import           Data.ByteString.Lazy.Char8     ( unpack )
import           System.Exit                    ( ExitCode(..)
                                                , exitSuccess
                                                )
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           System.Process.Typed           ( proc
                                                , readProcess
                                                )

--
--
-- process the list of formulas
procFormulas :: [BrewFormula] -> IO ()
procFormulas formulas = do
  putStrLn "Processing..."
  mapM_ procFormula formulas

--
procFormula :: BrewFormula -> IO ()
procFormula formula
  | not . null . dependants $ formula = print formula
  | otherwise = do
    answer <- askDeleteFormula formula
    case answer of
      No   -> return ()
      Yes  -> deleteFormula formula
      Quit -> putStrLn "Exit..." >> exitSuccess
      Que  -> putStrLn "Did not get that. Try again." >> procFormula formula

-- ask the user whether to delete the unused formula
askDeleteFormula :: BrewFormula -> IO Answer
askDeleteFormula formula = do
  putStr (show formula ++ "Delete? (q/y/N) ")
  hFlush stdout
  read <$> getLine

-- delete the given formula
deleteFormula :: BrewFormula -> IO ()
deleteFormula formula = do
  input <- readProcess $ proc "brew" ["uninstall", unpack . name $ formula]
  case input of
    (ExitSuccess  , out, _  ) -> print out
    (ExitFailure _, _  , err) -> print err
