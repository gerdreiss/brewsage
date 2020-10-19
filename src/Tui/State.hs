module Tui.State where

import qualified Brick                         as B
import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T

import           Cursor.Simple.List.NonEmpty    ( NonEmptyCursor
                                                , makeNonEmptyCursor
                                                )
import           Data.Brew                      ( BrewError
                                                , BrewFormula(..)
                                                , emptyFormulaList
                                                )
import           Tui.Popup                      ( Popup )
import           Tui.Types                      ( RName )

type NewState = B.EventM RName (B.Next TuiState)

data TuiState =
  TuiState
    { stateTitle           :: String
    , stateFormulas        :: NonEmptyCursor BrewFormula
    , stateNumberFormulas  :: Int
    , stateSelectedFormula :: Maybe BrewFormula
    , stateStatus          :: String
    , stateError           :: Maybe BrewError
    , statePopup           :: Maybe (Popup RName (TuiState -> NewState))
    }

data FormulaInfoState =
  FormState
    { _formulaInfoName   :: !T.Text
    , _formulaInfoAction :: !FormulaAction
    } deriving (Show)

data FormulaAction
  = InstallFormula
  | SearchFormula
  deriving (Eq, Ord, Show)



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
                      , stateFormulas        = emptyFormulaList
                      , stateNumberFormulas  = 0
                      , stateSelectedFormula = Nothing
                      , stateStatus          = "No installed formulas found"
                      , stateError           = Nothing
                      , statePopup           = Nothing
                      }

emptyInstallFormulaInfoState :: FormulaInfoState
emptyInstallFormulaInfoState =
  FormState { _formulaInfoName = T.empty, _formulaInfoAction = InstallFormula }

emptySearchFormulaInfoState :: FormulaInfoState
emptySearchFormulaInfoState =
  FormState { _formulaInfoName = T.empty, _formulaInfoAction = SearchFormula }
