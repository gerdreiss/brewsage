{-# LANGUAGE OverloadedStrings #-}

module Brewsage
  ( brewsage
  ) where

import           Data.ByteString.Lazy       (ByteString)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.List.Split            (splitOn)
import           System.Exit                (ExitCode (ExitFailure, ExitSuccess), exitSuccess)
import           System.IO                  (hFlush, stdout)
import           System.Process.Typed       (proc, readProcess, runProcess_)

-- main function of the module
-- executes 'brew list', extracts list of formulas, and processes each of them
brewsage :: IO ()
brewsage = readProcess "brew list" >>= listFormulas >>= mapM_ processFormula >>= cleanup

-- extracts list of formulas or errors out of a byte string
listFormulas :: (ExitCode, ByteString, ByteString) -> IO [String]
listFormulas input =
  return $
  case input of
    (ExitSuccess, out, _)   -> toFormulaList out
    (ExitFailure _, _, err) -> error $ unpack err

-- processes the given formula
processFormula :: String -> IO ()
processFormula formula = brewdeps formula >>= listDependents >>= processDependents formula

-- check for dependents of the given formula
brewdeps :: String -> IO (ExitCode, ByteString, ByteString)
brewdeps formula = readProcess $ proc "brew" ["uses", "--installed", formula]

-- extracts list of dependents of the given formula out of byte string
listDependents :: (ExitCode, ByteString, ByteString) -> IO [String]
listDependents input =
  return $
  case input of
    (ExitSuccess, out, _)   -> toFormulaList out
    (ExitFailure _, _, err) -> error $ unpack err

-- processes each of the dependents of the given formula
processDependents :: String -> [String] -> IO ()
processDependents formula dependents =
  case dependents of
    [""] -> checkDeleteFormula formula
    _    -> mapM_ putStr [formula, " is used by ", unwords dependents, ". skipping...\n"]

-- checks whether to delete the given formula, and deletes it if so desired
checkDeleteFormula :: String -> IO ()
checkDeleteFormula formula = do
  putStr $ formula ++ " is not used by any other formula. Delete? (y/N) "
  hFlush stdout
  line <- getLine
  case line of
    "y" -> deleteFormula formula >>= print
    "q" -> exitSuccess
    _   -> putStr ""

-- delete the given formula
deleteFormula :: String -> IO ()
deleteFormula formula = do
  input <- readProcess $ proc "brew" ["uninstall", formula]
  case input of
    (ExitSuccess, out, _)   -> print out
    (ExitFailure _, _, err) -> print err

-- extracts a list of strings out of the given byte string
toFormulaList :: ByteString -> [String]
toFormulaList = splitOn "\n" . unpack

cleanup :: () -> IO ()
cleanup = return $ runProcess_ "brew cleanup"
