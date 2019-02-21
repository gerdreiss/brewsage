{-# LANGUAGE OverloadedStrings #-}

module Brewsage
  ( brewsage
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List
import Data.List.Split
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process.Typed

brewsage :: IO ()
brewsage = readProcess "brew list" >>= listFormulas >>= processFormulas

listFormulas :: (ExitCode, ByteString, ByteString) -> IO [String]
listFormulas input =
  return $
  case input of
    (ExitSuccess, out, _) -> mkStringList out
    (ExitFailure code, _, err) -> errors code err

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
    (ExitSuccess, out, _) -> mkStringList out
    (ExitFailure code, _, err) -> errors code err

processDependents :: String -> [String] -> IO ()
processDependents formula dependents =
  mapM_ putStr [formula, ": ", unwords dependents, "\n"]

mkStringList :: ByteString -> [String]
mkStringList = splitOn "\n" . unpack

errors :: Int -> ByteString -> [String]
errors code err = ["Error occured:", codeMessage code, errorMessage err]

codeMessage :: Int -> String
codeMessage code = "Code    -> " ++ show code

errorMessage :: ByteString -> String
errorMessage err = "Message -> " ++ unpack err
