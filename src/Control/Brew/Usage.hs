{-# LANGUAGE OverloadedStrings #-}

module Control.Brew.Usage
  ( readFormulasWithUsages
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
readFormulasWithUsages = do
  errorOrFormulas <- readFormulas
  case errorOrFormulas of
    Right formulas -> parallel $ map readFormulaUsage formulas
    Left error     -> return [Left error]

-- list all formulas
readFormulas :: IO (Either BrewError [BrewFormula])
readFormulas = procBrewResult <$> execBrewList

--
--
-- list dependents for the given formula
readFormulaUsage :: BrewFormula -> IO (Either BrewError BrewFormula)
readFormulaUsage formula = do
  usage <- readFormulaUsageByName . name $ formula
  return $
    case usage of
      Right formulas -> Right $ formula {dependents = formulas}
      Left error     -> Left error

-- list dependents for the given formula name
readFormulaUsageByName :: B.ByteString -> IO (Either BrewError [BrewFormula])
readFormulaUsageByName formula = procBrewResult <$> execBrewDeps formula

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
procBrewResult input =
  case input of
    (ExitSuccess     , out, _  ) -> Right $ map (\s -> BrewFormula s []) (C8.words out)
    (ExitFailure code, _  , err) -> Left $ BrewError code err