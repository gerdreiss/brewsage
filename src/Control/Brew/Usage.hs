module Control.Brew.Usage
  ( listFormulasWithDependants
  )
where

import           Control.Brew.Commands
import           Control.Concurrent.ParallelIO  ( parallel )
import           Data.Brew
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C8
import           System.Exit
import           System.Process.Typed           ( proc
                                                , readProcess
                                                )

--
--
-- list all formulas with respective usages
listFormulasWithDependants :: IO [Either BrewError BrewFormula]
listFormulasWithDependants = listFormulas >>= procErrorOrFormulas

-- process error or retrieve usage for the given formulas
procErrorOrFormulas :: Either BrewError [BrewFormula] -> IO [Either BrewError BrewFormula]
procErrorOrFormulas (Right formulas) = parallel $ map readFormulaUsage formulas
procErrorOrFormulas (Left  error   ) = return [Left error]

--
--
-- get dependants of and assign them to the given formula
readFormulaUsage :: BrewFormula -> IO (Either BrewError BrewFormula)
readFormulaUsage formula = do
  usage <- listDependants formula
  return $ procErrorOrFormulaUsage formula usage

-- process error or assign retrieved dependent formulas to the given formula
procErrorOrFormulaUsage
  :: BrewFormula -> Either BrewError [BrewFormula] -> Either BrewError BrewFormula
procErrorOrFormulaUsage formula (Right formulas) = Right $ formula { dependants = formulas }
procErrorOrFormulaUsage _       (Left  error   ) = Left error
