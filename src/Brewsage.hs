{-# LANGUAGE OverloadedStrings #-}

module Brewsage
  ( brewsage
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List.Split
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process.Typed

brewsage :: IO ()
brewsage = brewList >>= mapM_ putStrLn

brewList :: IO [String]
brewList = readProcess "brew list" >>= processResult

processResult :: (ExitCode, ByteString, ByteString) -> IO [String]
processResult input =
  return $
  case input of
    (ExitSuccess, out, _) -> formulas out
    (ExitFailure code, _, err) -> errors code err

formulas :: ByteString -> [String]
formulas = splitOn "\n" . unpack

errors :: Int -> ByteString -> [String]
errors code err = ["Error occured:", codeMessage code, errorMessage err]

codeMessage :: Int -> String
codeMessage code = "Code    -> " ++ show code

errorMessage :: ByteString -> String
errorMessage err = "Message -> " ++ unpack err
