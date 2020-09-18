{-# LANGUAGE OverloadedStrings #-}

module Control.Brew.Commands
  ( getFormulaInfo
  , getFormulaUsage
  , listFormulas
  , listDependants
  , listDependencies
  )
where

import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C8

import           Data.Brew                      ( BrewError(..)
                                                , BrewFormula(..)
                                                , ErrorOrFormula
                                                , ErrorOrFormulas
                                                )
import           System.Exit                    ( ExitCode(..) )
import           System.Process.Typed           ( proc
                                                , readProcess
                                                )
import           Data.List.Safe                 ( safeHead )

type ReadProcessResult = (ExitCode, B.ByteString, B.ByteString)

-- list installed homebrew formulas
listFormulas :: IO ErrorOrFormulas
listFormulas = processListResult <$> execBrewList

-- load formula info for the given formula
getFormulaInfo :: BrewFormula -> IO ErrorOrFormula
getFormulaInfo formula = processInfoResult <$> (execBrewInfo . formulaName $ formula)

-- load usage info for the given formula
getFormulaUsage :: BrewFormula -> IO ErrorOrFormula
getFormulaUsage formula =
  fmap (\deps -> formula { formulaDependants = deps }) <$> listDependants formula

-- list dependants of the given formula
listDependants :: BrewFormula -> IO ErrorOrFormulas
listDependants formula = processListResult <$> (execBrewUses . formulaName $ formula)

-- list dependencies of the given formula
listDependencies :: BrewFormula -> IO ErrorOrFormulas
listDependencies formula =
  fmap formulaDependencies
    <$> (processInfoResult <$> (execBrewInfo . formulaName $ formula))

-- execute "brew list"
execBrewList :: IO ReadProcessResult
execBrewList = readProcess "brew list"

-- execute "brew uses --installed" for the given formula
execBrewUses :: B.ByteString -> IO ReadProcessResult
execBrewUses formula =
  readProcess $ proc "brew" ["uses", "--installed", C8.unpack formula]

-- execute "brew info" for the given formula
execBrewInfo :: B.ByteString -> IO ReadProcessResult
execBrewInfo formula = readProcess $ proc "brew" ["info", C8.unpack formula]

-- process results of a brew command that returns a list of formulas
processListResult :: ReadProcessResult -> ErrorOrFormulas
processListResult (ExitFailure cd, _, err) = Left $ BrewError cd err
processListResult (ExitSuccess, out, _) =
  Right $ map (\name -> BrewFormula name Nothing [] []) (C8.words out)

-- process results of a brew info command
processInfoResult :: ReadProcessResult -> ErrorOrFormula
processInfoResult (ExitFailure code, _  , err) = Left (BrewError code err)
processInfoResult (ExitSuccess     , out, _  ) = Right formula where
  formula      = BrewFormula name info dependencies []
  name         = C8.takeWhile (/= ':') out
  info         = Just . C8.intercalate (C8.pack "\n") . filter (not . C8.null) $ infoLines
  dependencies = map
    (\dependency -> BrewFormula dependency Nothing [] [])
    (case safeHead . dropWhile (/= "Required: ") $ rest of
      Nothing   -> []
      Just line -> map (C8.takeWhile (/= ',')) . C8.words . C8.dropWhile (/= ' ') $ line
    )
  (infoLines, rest) =
    span (\line -> "==>" /= (take 3 . C8.unpack $ line)) . C8.lines $ out
