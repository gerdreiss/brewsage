{-# LANGUAGE OverloadedStrings #-}

module Tui.State where

import qualified Data.List.NonEmpty            as NE

import           Data.Brew                      ( BrewFormula(..) )
import           Cursor.Simple.List.NonEmpty    ( makeNonEmptyCursor
                                                , NonEmptyCursor
                                                )


data TuiState = TuiState
  { title :: String
  , status :: String
  , formulas :: NonEmptyCursor BrewFormula
  , selected :: Maybe BrewFormula
  }
  deriving (Show, Eq)

buildInitialState :: [BrewFormula] -> IO TuiState
buildInitialState fs = do
  let maybeFormulas = NE.nonEmpty fs
  case maybeFormulas of
    Nothing -> pure emptyState
    Just ne -> pure TuiState
      { title    = "Brewsage"
      , status   = "Ready | q - Exit | ENTER - Show selected | x - Delete selected"
      , formulas = makeNonEmptyCursor ne
      , selected = Nothing
      }

emptyState :: TuiState
emptyState = TuiState
  { title    = "Brewsage"
  , status   = "No formulas found | q - Exit"
  , formulas = makeNonEmptyCursor
               . NE.fromList
               $ [BrewFormula { name = "", dependencies = [], dependants = [] }]
  , selected = Nothing
  }
