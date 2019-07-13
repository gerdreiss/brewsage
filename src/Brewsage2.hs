{-# LANGUAGE OverloadedStrings #-}

module Brewsage2 where

import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C8

import           Data.Char                  (isLetter, toLower)
import           Data.List                  (intercalate)
import           System.Exit
import           System.IO                  (hFlush, stdout)
import           System.Process.Typed       (proc, readProcess)

type ReadProcessResult = (ExitCode, B.ByteString, B.ByteString)

data BrewFormula =
  BrewFormula
    { name       :: B.ByteString
    , dependents :: [BrewFormula]
    }

instance Show BrewFormula where
  show formula =
    (C8.unpack . name $ formula) ++
    case dependents formula of
      []       -> " is not used by any other formula.\n"
      formulas -> " is used by " ++ formulaNames formulas ++ ".\n"
        where formulaNames formulas = intercalate ", " (map (C8.unpack . name) formulas)

data BrewError =
  BrewError
    { code    :: Int
    , message :: B.ByteString
    }

instance Show BrewError where
  show error = "Error occurred: code " ++ (show . code $ error) ++ " message " ++ (show . message $ error)

data Answer
  = Yes
  | No
  | Quit
  | Que
  deriving (Show)

instance Read Answer where
  readsPrec _ input =
    case map toLower . filter isLetter $ input of
      "quit" -> [(Quit, [])]
      "q"    -> [(Quit, [])]
      "yes"  -> [(Yes, [])]
      "y"    -> [(Yes, [])]
      "nope" -> [(No, [])]
      "no"   -> [(No, [])]
      "n"    -> [(No, [])]
      ""     -> [(No, [])]
      _      -> [(Que, [])]

--
--
-- list all formulas with respective usages
readFormulasWithUsages :: IO [Either BrewError BrewFormula]
readFormulasWithUsages = do
  errorOrFormulas <- readFormulas
  case errorOrFormulas of
    Right formulas -> traverse readFormulaUsage formulas
    Left error     -> return [Left error]

--
--
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
    (ExitSuccess     , out,   _) -> Right $ map (\s -> BrewFormula s []) (C8.words out)
    (ExitFailure code,   _, err) -> Left $ BrewError code err

--
--
-- ask the user to delete the unused formula
askDeleteFormula :: BrewFormula -> IO Answer
askDeleteFormula formula = do
  putStr $ (C8.unpack . name $ formula) ++ " is not used by any other formula. Delete? (y/N) "
  hFlush stdout
  read <$> getLine
