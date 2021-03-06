module Control.Brew.Usage
  ( listFormulas
  , listFormulasComplete
  , getCompleteFormulaInfo
  ) where

import qualified Control.Brew.Commands         as C

import           Control.Concurrent.ParallelIO  ( parallel )
import           Data.Brew                      ( BrewFormula(..)
                                                , ErrorOrFormula
                                                , ErrorOrFormulas
                                                )

--
--
-- list all formulas
listFormulas :: IO [ErrorOrFormula]
listFormulas = C.listFormulas >>= procErrorOrFormulas False

--
--
-- list all formulas with respective usages
listFormulasComplete :: IO [ErrorOrFormula]
listFormulasComplete = C.listFormulas >>= procErrorOrFormulas True

--
--
-- get full info for the given formula
getCompleteFormulaInfo :: BrewFormula -> IO ErrorOrFormula
getCompleteFormulaInfo formula = do
  [i, u, d] <- parallel
    $ fmap ($ formula) [C.getFormulaInfo False, C.getFormulaUsage, C.getFormulaDeps]
  return $ procErrorOrFormulaCompleteWithUsageAndDeps i u d

--
--
-- process error or retrieve usage for the given formulas
procErrorOrFormulas :: Bool -> ErrorOrFormulas -> IO [ErrorOrFormula]
procErrorOrFormulas complete (Right formulas) = if complete
  then parallel $ fmap getCompleteFormulaInfo formulas
  else return $ fmap Right formulas
procErrorOrFormulas _        (Left  err     ) = return [Left err]

--
--
-- process error or assign retrieved dependent formulas to the given formula
procErrorOrFormulaCompleteWithUsageAndDeps
  :: ErrorOrFormula    -- the formula with info
  -> ErrorOrFormula    -- the formula with dependants
  -> ErrorOrFormula    -- the formula with dependencies
  -> ErrorOrFormula    -- the formula including the dependants and the dependencies
procErrorOrFormulaCompleteWithUsageAndDeps (Right formula) (Right dpns) (Right deps) =
  Right $ formula { formulaDependants   = formulaDependants dpns
                  , formulaDependencies = formulaDependencies deps
                  }
procErrorOrFormulaCompleteWithUsageAndDeps formula dpns deps =
  -- this looks more dangerous than it is.
  -- head will never be called because sequence necessarily returns a Left,
  -- since the case where all elements of the list are Right is covered above.
  -- it's just a 
  head <$> sequence [formula, dpns, deps]
