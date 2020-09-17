{-# LANGUAGE OverloadedStrings #-}

module Tui.State where

import qualified Data.List.NonEmpty            as NE

import           Data.Brew                      ( BrewError
                                                , BrewFormula(..)
                                                )
import           Cursor.Simple.List.NonEmpty    ( makeNonEmptyCursor
                                                , NonEmptyCursor
                                                )


data TuiState = TuiState
  { stateTitle :: String
  , stateFormulas :: NonEmptyCursor BrewFormula
  , stateNumberFormulas :: Int
  , stateSelected :: Maybe BrewFormula
  , stateStatus :: String
  , stateError :: Maybe BrewError
  }
  deriving (Show, Eq)

buildInitialState :: [BrewFormula] -> IO TuiState
buildInitialState fs = do
  let maybeFormulas = NE.nonEmpty fs
  case maybeFormulas of
    Nothing -> pure emptyState
    Just ne -> pure TuiState
      { stateTitle          = "Brewsage"
      , stateFormulas       = makeNonEmptyCursor ne
      , stateNumberFormulas = length ne
      , stateSelected       = Nothing
      , stateStatus         =
        "Ready | q - Exit | ENTER - Show selected | x - Delete selected | i - Install new"
      , stateError          = Nothing
      }

emptyState :: TuiState
emptyState = TuiState
  { stateTitle          = "Brewsage"
  , stateFormulas       = makeNonEmptyCursor
                          . NE.fromList
                          $ [ BrewFormula { formulaName         = ""
                                          , formulaDependencies = []
                                          , formulaDependants   = []
                                          }
                            ]
  , stateNumberFormulas = 0
  , stateSelected       = Nothing
  , stateStatus         = "No formulas found | q - Exit | i - Install new"
  , stateError          = Nothing
  }
