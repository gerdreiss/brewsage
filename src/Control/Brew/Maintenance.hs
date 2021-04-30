module Control.Brew.Maintenance
  ( procFormulas
  ) where

import           Control.Brew.Commands          ( uninstallFormula )
import           Data.Brew                      ( Answer(..)
                                                , BrewFormula(..)
                                                )
import           Data.ByteString.Lazy.Char8     ( unpack )
import           System.Exit                    ( exitSuccess )
import           System.IO                      ( hFlush
                                                , stdout
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
  | not . null . formulaDependants $ formula = print formula
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
  input <- uninstallFormula formula
  case input of
    Right f -> print $ (unpack . formulaName $ f) ++ " uninstalled"
    Left  e -> print e
