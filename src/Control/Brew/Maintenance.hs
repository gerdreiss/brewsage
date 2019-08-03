module Control.Brew.Maintenance where

import           Data.Brew
import qualified Data.ByteString.Lazy.Char8 as C8
import           System.IO                  (hFlush, stdout)

--
--
-- ask the user to delete the unused formula
askDeleteFormula :: BrewFormula -> IO Answer
askDeleteFormula formula = do
  putStr $ (C8.unpack . name $ formula) ++ " is not used by any other formula. Delete? (y/N) "
  hFlush stdout
  read <$> getLine
