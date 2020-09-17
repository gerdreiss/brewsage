{-# LANGUAGE OverloadedStrings #-}

module Tui.State where

import qualified Data.List.NonEmpty            as NE

import           Data.Brew                      ( BrewError
                                                , BrewFormula(..)
                                                , emptyFormula
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
    Just ne -> pure TuiState { stateTitle          = "Brewsage"
                             , stateFormulas       = makeNonEmptyCursor ne
                             , stateNumberFormulas = length ne
                             , stateSelected       = Nothing
                             , stateStatus         = "Ready"
                             , stateError          = Nothing
                             }

emptyState :: TuiState
emptyState = TuiState { stateTitle          = "Brewsage"
                      , stateFormulas = makeNonEmptyCursor $ NE.fromList [emptyFormula]
                      , stateNumberFormulas = 0
                      , stateSelected       = Nothing
                      , stateStatus         = "No installed formulas found"
                      , stateError          = Nothing
                      }
