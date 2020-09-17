module Control.Brew.Usage
  ( listFormulas
  , listFormulasWithDependants
  , readFormulaUsage
  )
where

import qualified Control.Brew.Commands         as C

import           Control.Concurrent.ParallelIO  ( parallel )
import           Data.Brew                      ( ErrorOrFormulas
                                                , BrewFormula(..)
                                                , ErrorOrFormula
                                                )

--
--
-- list all formulas
listFormulas :: IO [ErrorOrFormula]
listFormulas = C.listFormulas >>= procErrorOrFormulas False

--
--
-- list all formulas with respective usages
listFormulasWithDependants :: IO [ErrorOrFormula]
listFormulasWithDependants = C.listFormulas >>= procErrorOrFormulas True

-- process error or retrieve usage for the given formulas
procErrorOrFormulas :: Bool -> ErrorOrFormulas -> IO [ErrorOrFormula]
procErrorOrFormulas b (Right formulas) =
  if b then parallel $ map readFormulaUsage formulas else return $ map Right formulas
procErrorOrFormulas _ (Left err) = return [Left err]

--
--
-- get dependants of and assign them to the given formula
readFormulaUsage :: BrewFormula -> IO ErrorOrFormula
readFormulaUsage formula = do
  usage <- C.listDependants formula
  deps  <- C.listDependencies formula
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
