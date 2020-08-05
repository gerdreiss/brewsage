{-# LANGUAGE OverloadedStrings #-}

module Control.Brew.Commands
  ( listFormulas
  , listDependants
  , listDependencies
  )
where

import           Data.Brew
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C8
import           System.Exit
import           System.Process.Typed           ( proc
                                                , readProcess
                                                )
import           Data.List.Safe

type ReadProcessResult = (ExitCode, B.ByteString, B.ByteString)

type ErrorOrFormulas = Either BrewError [BrewFormula]

-- list installed homebrew formulas
listFormulas :: IO ErrorOrFormulas
listFormulas = processListResult <$> execBrewList

-- list dependants of the given formula
listDependants :: BrewFormula -> IO ErrorOrFormulas
listDependants formula = processListResult <$> (execBrewUses . name $ formula)

-- list dependencies of the given formula
listDependencies :: BrewFormula -> IO ErrorOrFormulas
listDependencies formula = processInfoResult <$> (execBrewInfo . name $ formula)

-- execute "brew list"
execBrewList :: IO ReadProcessResult
execBrewList = readProcess "brew list"

-- execute "brew uses --installed" for the given formula
execBrewUses :: B.ByteString -> IO ReadProcessResult
execBrewUses formula = readProcess $ proc "brew" ["uses", "--installed", C8.unpack formula]

-- execute "brew info" for the given formula
execBrewInfo :: B.ByteString -> IO ReadProcessResult
execBrewInfo formula = readProcess $ proc "brew" ["info", C8.unpack formula]

-- process results of a brew command that returns a list of formulas
processListResult :: ReadProcessResult -> ErrorOrFormulas
processListResult (ExitFailure code, _, err) = Left $ BrewError code err
processListResult (ExitSuccess, out, _) = Right $ map (\s -> BrewFormula s [] []) (C8.words out)

-- process results of a brew info command
processInfoResult :: ReadProcessResult -> ErrorOrFormulas
processInfoResult (ExitFailure code, _  , err) = Left $ BrewError code err
processInfoResult (ExitSuccess     , out, _  ) = Right formulas where
  formulas     = map (\dependency -> BrewFormula dependency [] []) dependencies
  dependencies = case safeHead . safeTail . dropWhile (/= "==> Dependencies") . C8.lines $ out of
    Nothing   -> []
    Just line -> map (C8.takeWhile (/= ',')) . C8.words . C8.dropWhile (/= ' ') $ line
