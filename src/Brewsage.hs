{-# LANGUAGE OverloadedStrings #-}

module Brewsage
  ( brewsage
  ) where

import           Data.ByteString.Lazy       (ByteString)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.List.Split            (splitOn)
import           System.Exit                (ExitCode (ExitFailure, ExitSuccess))
import           System.Process.Typed       (proc, readProcess)

brewsage :: IO ()
brewsage = readProcess "brew list" >>= listFormulas >>= processFormulas

listFormulas :: (ExitCode, ByteString, ByteString) -> IO [String]
listFormulas input =
  return $
  case input of
    (ExitSuccess, out, _) -> toFormulaList out
    (ExitFailure code, _, err) -> toErrorList code err

processFormulas :: [String] -> IO ()
processFormulas = mapM_ processFormula

processFormula :: String -> IO ()
processFormula formula =
  execBrewdeps >>= listDependents >>= processDependents formula
  where
    execBrewdeps = readProcess $ proc "brew" ["uses", "--installed", formula]

listDependents :: (ExitCode, ByteString, ByteString) -> IO [String]
listDependents input =
  return $
  case input of
    (ExitSuccess, out, _) -> toFormulaList out
    (ExitFailure code, _, err) -> toErrorList code err

processDependents :: String -> [String] -> IO ()
processDependents formula dependents =
  mapM_ putStr [formula, ": ", unwords dependents, "\n"]

toFormulaList :: ByteString -> [String]
toFormulaList = splitOn "\n" . unpack

toErrorList :: Int -> ByteString -> [String]
toErrorList code err =
  ["Error occurred while invoking homebrew", codeMessage code, errorMessage err]

codeMessage :: Int -> String
codeMessage code = "Code   -> " ++ show code

errorMessage :: ByteString -> String
errorMessage err = "Error  -> " ++ unpack err
