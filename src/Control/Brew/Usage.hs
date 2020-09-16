module Control.Brew.Usage
  ( listFormulasWithDependants
  )
where

import           Control.Brew.Commands          ( listFormulas
                                                , listDependants
                                                , listDependencies
                                                )
import           Control.Concurrent.ParallelIO  ( parallel )
import           Data.Brew                      ( BrewError
                                                , BrewFormula(..)
                                                )

--
--
-- list all formulas with respective usages
listFormulasWithDependants :: IO [Either BrewError BrewFormula]
listFormulasWithDependants = listFormulas >>= procErrorOrFormulas

-- process error or retrieve usage for the given formulas
procErrorOrFormulas :: Either BrewError [BrewFormula] -> IO [Either BrewError BrewFormula]
procErrorOrFormulas (Right formulas) = parallel $ map readFormulaUsage formulas
procErrorOrFormulas (Left  err     ) = return [Left err]

--
--
-- get dependants of and assign them to the given formula
readFormulaUsage :: BrewFormula -> IO (Either BrewError BrewFormula)
readFormulaUsage formula = do
  usage <- listDependants formula
  deps  <- listDependencies formula
  return $ procErrorOrFormulaUsage formula usage deps

-- process error or assign retrieved dependent formulas to the given formula
procErrorOrFormulaUsage
  :: BrewFormula                      -- the formula
  -> Either BrewError [BrewFormula]   -- the dependants
  -> Either BrewError [BrewFormula]   -- the dependencies
  -> Either BrewError BrewFormula     -- the formula including the dependants and the dependencies
procErrorOrFormulaUsage formula (Right dpns) (Right deps) =
  Right $ formula { dependants = dpns, dependencies = deps }
procErrorOrFormulaUsage _ (Left err) _          = Left err
procErrorOrFormulaUsage _ _          (Left err) = Left err
