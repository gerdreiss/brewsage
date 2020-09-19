module Tui.State where

import qualified Brick                         as B
import qualified Data.List.NonEmpty            as NE

import           Data.Brew                      ( BrewError
                                                , BrewFormula(..)
                                                , emptyFormula
                                                )
import           Cursor.Simple.List.NonEmpty    ( makeNonEmptyCursor
                                                , NonEmptyCursor
                                                )
import           Tui.Popup
import           Tui.Types


type NewState = B.EventM RName (B.Next TuiState)

data TuiState = TuiState
  { stateTitle :: String
  , stateFormulas :: NonEmptyCursor BrewFormula
  , stateNumberFormulas :: Int
  , stateSelectedFormula :: Maybe BrewFormula
  , stateStatus :: String
  , stateError :: Maybe BrewError
  , statePopup :: Maybe (Popup RName (TuiState -> NewState))
  }

buildInitialState :: [BrewFormula] -> IO TuiState
buildInitialState fs = do
  let maybeFormulas = NE.nonEmpty fs
  case maybeFormulas of
    Nothing -> pure emptyState
    Just ne -> pure TuiState { stateTitle           = "Brewsage"
                             , stateFormulas        = makeNonEmptyCursor ne
                             , stateNumberFormulas  = length ne
                             , stateSelectedFormula = Nothing
                             , stateStatus          = "Ready"
                             , stateError           = Nothing
                             , statePopup           = Nothing
                             }

emptyState :: TuiState
emptyState = TuiState { stateTitle           = "Brewsage"
                      , stateFormulas = makeNonEmptyCursor $ NE.fromList [emptyFormula]
                      , stateNumberFormulas  = 0
                      , stateSelectedFormula = Nothing
                      , stateStatus          = "No installed formulas found"
                      , stateError           = Nothing
                      , statePopup           = Nothing
                      }
