module Control.Brew.Usage
  ( listFormulasWithDependants
  )
where

import           Control.Brew.Commands          ( listFormulas
                                                , listDependants
                                                , listDependencies
                                                )
import           Control.Concurrent.ParallelIO  ( parallel )
import           Data.Brew                      ( ErrorOrFormulas
                                                , BrewError
                                                , BrewFormula(..)
                                                , ErrorOrFormula
                                                )

--
--
-- list all formulas with respective usages
listFormulasWithDependants :: IO [ErrorOrFormula]
listFormulasWithDependants = listFormulas >>= procErrorOrFormulas

-- process error or retrieve usage for the given formulas
procErrorOrFormulas :: Either BrewError [BrewFormula] -> IO [ErrorOrFormula]
procErrorOrFormulas (Right formulas) = parallel $ map readFormulaUsage formulas
procErrorOrFormulas (Left  err     ) = return [Left err]

--
--
-- get dependants of and assign them to the given formula
readFormulaUsage :: BrewFormula -> IO ErrorOrFormula
readFormulaUsage formula = do
  usage <- listDependants formula
  deps  <- listDependencies formula
  return $ procErrorOrFormulaUsage formula usage deps

-- process error or assign retrieved dependent formulas to the given formula
procErrorOrFormulaUsage
  :: BrewFormula       -- the formula
  -> ErrorOrFormulas   -- the dependants
  -> ErrorOrFormulas   -- the dependencies
  -> ErrorOrFormula    -- the formula including the dependants and the dependencies
procErrorOrFormulaUsage formula (Right dpns) (Right deps) =
  Right $ formula { formulaDependants = dpns, formulaDependencies = deps }
procErrorOrFormulaUsage _ (Left err) _          = Left err
procErrorOrFormulaUsage _ _          (Left err) = Left err
