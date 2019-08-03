module Control.Brew.Maintenance
  ( procFormulas
  , procFormula
  ) where

import           Data.Brew
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.List                  (intercalate)
import           System.Exit                (ExitCode (ExitFailure, ExitSuccess),
                                             exitSuccess)
import           System.IO                  (hFlush, stdout)
import           System.Process.Typed       (proc, readProcess)

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
  | not . null . dependents $ formula = print formula
  | otherwise = do
    answer <- askDeleteFormula formula
    case answer of
      No   -> return ()
      Yes  -> deleteFormula formula
      Quit -> putStrLn "Exit..." >> exitSuccess
      Que  -> putStrLn "Did not understand your answer. Try again." >> procFormula formula

-- ask the user whether to delete the unused formula
askDeleteFormula :: BrewFormula -> IO Answer
askDeleteFormula formula = do
  putStr $ (C8.unpack . name $ formula) ++ " is not used by any other formula. Delete? (y/N) "
  hFlush stdout
  read <$> getLine

-- delete the given formula
deleteFormula :: BrewFormula -> IO ()
deleteFormula formula = do
  input <- readProcess $ proc "brew" ["uninstall", C8.unpack . name $ formula]
  case input of
    (ExitSuccess  , out,   _) -> print out
    (ExitFailure _,   _, err) -> print err
