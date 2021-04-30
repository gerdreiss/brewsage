{-# LANGUAGE OverloadedStrings #-}

module Control.Brew.Commands
  ( getFormulaInfo
  , getFormulaUsage
  , getFormulaDeps
  , listFormulas
  , listDependants
  , listDependencies
  , installFormula
  , uninstallFormula
  , upgradeAllFormulas
  ) where

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

type ReadProcessResult = (ExitCode, B.ByteString, B.ByteString)

-- upgrade and return upgraded all formulas
upgradeAllFormulas :: IO ErrorOrFormulas
upgradeAllFormulas =
  putStrLn "executing brew upgrade..." >> execBrewUpgrade >> listFormulas

-- list installed homebrew formulas
listFormulas :: IO ErrorOrFormulas
listFormulas = processListResult <$> execBrewList

-- load formula info for the given formula
getFormulaInfo :: Bool -> BrewFormula -> IO ErrorOrFormula
getFormulaInfo full formula =
  processInfoResult full <$> (execBrewInfo . formulaName $ formula)

-- load usage info for the given formula
getFormulaUsage :: BrewFormula -> IO ErrorOrFormula
getFormulaUsage formula =
  fmap (\deps -> formula { formulaDependants = deps }) <$> listDependants formula

getFormulaDeps :: BrewFormula -> IO ErrorOrFormula
getFormulaDeps formula =
  fmap (\deps -> formula { formulaDependencies = deps }) <$> listDependencies formula

-- list dependants of the given formula
listDependants :: BrewFormula -> IO ErrorOrFormulas
listDependants formula = processListResult <$> (execBrewUses . formulaName $ formula)

-- list dependencies of the given formula
listDependencies :: BrewFormula -> IO ErrorOrFormulas
listDependencies formula = processListResult <$> (execBrewDeps . formulaName $ formula)

-- install formula
installFormula :: BrewFormula -> IO ErrorOrFormula
installFormula formula = do
  putStrLn . concat $ ["Installing formula '", C8.unpack . formulaName $ formula, "'..."]
  result <- execBrewInstall . formulaName $ formula
  case result of
    (ExitFailure code, _, err) -> return $ Left (BrewError code err)
    (ExitSuccess     , _, _  ) -> return $ Right formula

-- uninstall formula
uninstallFormula :: BrewFormula -> IO ErrorOrFormula
uninstallFormula formula = do
  putStrLn
    . concat
    $ ["Uninstalling formula '", C8.unpack . formulaName $ formula, "'..."]
  result <- execBrewUninstall . formulaName $ formula
  case result of
    (ExitFailure code, _, err) -> return $ Left (BrewError code err)
    (ExitSuccess     , _, _  ) -> return $ Right formula

-- execute "brew list"
execBrewList :: IO ReadProcessResult
execBrewList = readProcess $ proc "brew" ["list", "--formula", "--version"]

-- execute "brew upgrade"
execBrewUpgrade :: IO ReadProcessResult
execBrewUpgrade = readProcess $ proc "brew" ["upgrade"]

-- execute "brew uses --installed" for the given formula
execBrewUses :: B.ByteString -> IO ReadProcessResult
execBrewUses formula =
  readProcess $ proc "brew" ["uses", "--installed", C8.unpack formula]

-- execute "brew deps" for the given formula
execBrewDeps :: B.ByteString -> IO ReadProcessResult
execBrewDeps formula = readProcess $ proc "brew" ["deps", C8.unpack formula]

-- execute "brew info" for the given formula
execBrewInfo :: B.ByteString -> IO ReadProcessResult
execBrewInfo formula = readProcess $ proc "brew" ["info", C8.unpack formula]

-- execute "brew install 'formula-name'"
execBrewInstall :: B.ByteString -> IO ReadProcessResult
execBrewInstall formula = readProcess $ proc "brew" ["install", C8.unpack formula]

-- execute "brew uninstall 'formula-name'"
execBrewUninstall :: B.ByteString -> IO ReadProcessResult
execBrewUninstall formula = readProcess $ proc "brew" ["uninstall", C8.unpack formula]

-- process results of a brew command that returns a list of formulas
processListResult :: ReadProcessResult -> ErrorOrFormulas
processListResult (ExitFailure cd, _  , err) = Left $ BrewError cd err
processListResult (ExitSuccess   , out, _  ) = Right
  $ fmap (\line -> BrewFormula (name line) (version line) Nothing [] []) (C8.lines out)
 where
  name line = head $ C8.words line
  version line = Just . last $ C8.words line

-- process results of a brew info command
processInfoResult :: Bool -> ReadProcessResult -> ErrorOrFormula
processInfoResult _    (ExitFailure code, _  , err) = Left (BrewError code err)
processInfoResult full (ExitSuccess     , out, _  ) = Right formula where
  formula = BrewFormula name version info [] []
  name    = C8.takeWhile (/= ':') out
  version = Just $ C8.words out !! 2
  info    = Just . C8.intercalate (C8.pack "\n") . filter (not . C8.null) $ infoLines
  infoLines =
    if full then C8.lines out else takeWhile (not . B.isPrefixOf "==>") $ C8.lines out
