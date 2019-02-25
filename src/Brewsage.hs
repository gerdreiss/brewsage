{-# LANGUAGE OverloadedStrings #-}

module Brewsage
  ( brewsage
  ) where

import           Data.ByteString.Lazy       (ByteString)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.List.Split            (splitOn)
import           System.Exit                (ExitCode (ExitFailure, ExitSuccess))
import           System.IO                  (hFlush, stdout)
import           System.Process.Typed       (proc, readProcess)

-- main function of the module
-- executes 'brew list', extracts list of formulas, and processes each of them
brewsage :: IO ()
brewsage = readProcess "brew list" >>= listFormulas >>= processFormulas

-- extracts list of formulas or errors out of a byte string
listFormulas :: (ExitCode, ByteString, ByteString) -> IO [String]
listFormulas input =
  return $
    case input of
      (ExitSuccess, out, _)      -> toFormulaList out
      (ExitFailure code, _, err) -> toErrorList code err

-- processes a list of formulas
processFormulas :: [String] -> IO ()
processFormulas = mapM_ processFormula

-- processes the given formula
processFormula :: String -> IO ()
processFormula formula =
  execBrewdeps >>= listDependents >>= processDependents formula
  where
    execBrewdeps = readProcess $ proc "brew" ["uses", "--installed", formula]

-- extracts list of dependents of the given formula out of byte string
listDependents :: (ExitCode, ByteString, ByteString) -> IO [String]
listDependents input =
  return $
    case input of
      (ExitSuccess, out, _)      -> toFormulaList out
      (ExitFailure code, _, err) -> toErrorList code err

-- processes each of the dependents of the given formula
processDependents :: String -> [String] -> IO ()
processDependents formula dependents =
  case dependents of
    [""] -> deleteFormula formula
    _    -> mapM_ putStr [formula, ": ", unwords dependents, "\n"]

-- deletes the given formula, if so desired
deleteFormula :: String -> IO ()
deleteFormula formula = do
  putStr $ "Delete " ++ formula ++ "? (y/N) "
  hFlush stdout
  line <- getLine
  putStrLn $ "You said '" ++ valueOrDefault line ++ "'"
  where
    valueOrDefault v =
      case v of
        [] -> "N"
        xs -> xs

-- extracts a list of strings out of the given byte string
toFormulaList :: ByteString -> [String]
toFormulaList = splitOn "\n" . unpack

-- extracts a list of errors out of the given byte string
toErrorList :: Int -> ByteString -> [String]
toErrorList code err =
  ["Error occurred while invoking homebrew", codeMessage code, errorMessage err]

-- creates a message for the given error code
codeMessage :: Int -> String
codeMessage code = "Code   -> " ++ show code

-- creates a message for the given error message
errorMessage :: ByteString -> String
errorMessage err = "Error  -> " ++ unpack err
