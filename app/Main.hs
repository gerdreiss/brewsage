{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brewsage
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List.Split
import System.Exit (ExitCode)
import System.Process.Typed

main :: IO ()
main = do
  (exitCode, out, err) <- readProcess "brew list"
  putStrLn $ "ExitCode = " ++ show exitCode
  putStrLn $ "Err      = " ++ show err
  putStrLn "Out      = "
  mapM_ putStrLn $ splitOn "\n" $ unpack out
