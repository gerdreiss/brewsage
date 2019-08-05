{-# LANGUAGE OverloadedStrings #-}

module Control.Brew.Usage
  ( readFormulas
  , readFormulasWithUsages
  ) where

import           Control.Concurrent.ParallelIO (parallel)
import           Data.Brew
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C8
import           System.Exit
import           System.Process.Typed          (proc, readProcess)

--
--
type ReadProcessResult = (ExitCode, B.ByteString, B.ByteString)

--
--
-- list all formulas with respective usages
readFormulasWithUsages :: IO [Either BrewError BrewFormula]
readFormulasWithUsages = readFormulas >>= procErrorOrFormulas

-- list all formulas
readFormulas :: IO (Either BrewError [BrewFormula])
readFormulas = procBrewResult <$> execBrewList

-- process error or retrieve usage for the given formulas
procErrorOrFormulas :: Either BrewError [BrewFormula] -> IO [Either BrewError BrewFormula]
procErrorOrFormulas (Right formulas) = parallel $ map readFormulaUsage formulas
procErrorOrFormulas (Left error    ) = return [Left error]

--
--
-- get dependents of and assign them to the given formula
readFormulaUsage :: BrewFormula -> IO (Either BrewError BrewFormula)
readFormulaUsage formula = do
  usage <- readFormulaUsageByName . name $ formula
  return $ procErrorOrFormulaUsage formula usage

-- list dependents for the given formula name
readFormulaUsageByName :: B.ByteString -> IO (Either BrewError [BrewFormula])
readFormulaUsageByName formula = procBrewResult <$> execBrewDeps formula

-- process error or assign retrieved dependent formulas to the given formula
procErrorOrFormulaUsage :: BrewFormula -> Either BrewError [BrewFormula] -> Either BrewError BrewFormula
procErrorOrFormulaUsage formula (Right formulas) = Right $ formula {dependents = formulas}
procErrorOrFormulaUsage _       (Left error    ) = Left error

--
--
-- execute "brew list"
execBrewList :: IO ReadProcessResult
execBrewList = readProcess "brew list"

-- execute "brew uses --installed" for the given formula
execBrewDeps :: B.ByteString -> IO ReadProcessResult
execBrewDeps formula = readProcess $ proc "brew" ["uses", "--installed", C8.unpack formula]

-- process results of a brew command that returns a list of formulas
procBrewResult :: ReadProcessResult -> Either BrewError [BrewFormula]
procBrewResult (ExitSuccess     , out,   _) = Right $ map (\s -> BrewFormula s []) (C8.words out)
procBrewResult (ExitFailure code,   _, err) = Left $ BrewError code err
