{-# LANGUAGE OverloadedStrings #-}

module Brewsage
  ( brewsage
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List.Split
import System.Exit (ExitCode)
import System.Process.Typed


brewsage :: IO ()
brewsage = do
  (exitCode, out, err) <- readProcess "brew list"
  putStrLn $ "ExitCode = " ++ show exitCode
  putStrLn $ "Err      = " ++ show err
  putStrLn "Out      = "
  mapM_ putStrLn $ splitOn "\n" $ unpack out
