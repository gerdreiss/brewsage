{-# LANGUAGE OverloadedStrings #-}

module Control.Brew.Commands
  ( listFormulas
  , listDependants
  )
where

import           Data.Brew
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C8
import           System.Exit
import           System.Process.Typed           ( proc
                                                , readProcess
                                                )

type ReadProcessResult = (ExitCode, B.ByteString, B.ByteString)
type ErrorOrFormulas = Either BrewError [BrewFormula]

-- list installed homebrew formulas
listFormulas :: IO ErrorOrFormulas
listFormulas = procBrewResult <$> execBrewList

-- list dependants of the given formula
listDependants :: BrewFormula -> IO ErrorOrFormulas
listDependants formula = procBrewResult <$> (execBrewUses . name $ formula)

-- execute "brew list"
execBrewList :: IO ReadProcessResult
execBrewList = readProcess "brew list"

-- execute "brew uses --installed" for the given formula
execBrewUses :: B.ByteString -> IO ReadProcessResult
execBrewUses formula = readProcess $ proc "brew" ["uses", "--installed", C8.unpack formula]

-- process results of a brew command that returns a list of formulas
procBrewResult :: ReadProcessResult -> ErrorOrFormulas
procBrewResult (ExitSuccess     , out, _  ) = Right $ map (`BrewFormula` []) (C8.words out)
procBrewResult (ExitFailure code, _  , err) = Left $ BrewError code err
