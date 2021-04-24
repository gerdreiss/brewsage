module Tui.State where

import qualified Brick                         as B
import qualified Brick.Widgets.Edit            as E
import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T

import           Cursor.Simple.List.NonEmpty    ( NonEmptyCursor
                                                , makeNonEmptyCursor
                                                )
import           Data.Brew                      ( BrewError
                                                , BrewFormula(..)
                                                , emptyFormulaList
                                                )
import           Lens.Micro.TH                  ( makeLenses )
import           Tui.Popup                      ( Popup )

data RName = Formulas | FormulaInfo | FormulaName deriving (Eq, Ord, Show)

data FormulaOp =  FormulaList | FormulaFilter | FormulaSearch | FormulaInstall

type NewState = B.EventM RName (B.Next TuiState)

data TuiState = TuiState
  { _stateTitle           :: String
  , _stateFormulas        :: NonEmptyCursor BrewFormula
  , _stateNumberFormulas  :: Int
  , _stateSelectedFormula :: Maybe BrewFormula
  , _stateStatus          :: String
  , _stateError           :: Maybe BrewError
  , _statePopup           :: Maybe (Popup RName (TuiState -> NewState))
  , _stateFormulaNameOp   :: FormulaOp
  , _stateFormulaName     :: E.Editor String RName
  }

data FormulaInfoState = FormState
  { _formulaInfoName   :: !T.Text
  , _formulaInfoAction :: !FormulaAction
  }
  deriving Show

data FormulaAction
  = InstallFormula
  | SearchFormula
  deriving (Eq, Ord, Show)

makeLenses ''TuiState
makeLenses ''FormulaInfoState


buildInitialState :: [BrewFormula] -> IO TuiState
buildInitialState fs = do
  let maybeFormulas = NE.nonEmpty fs
  case maybeFormulas of
    Nothing -> pure emptyState
    Just ne -> pure TuiState { _stateTitle           = "Brewsage"
                             , _stateFormulas        = makeNonEmptyCursor ne
                             , _stateNumberFormulas  = length ne
                             , _stateSelectedFormula = Nothing
                             , _stateStatus          = "Ready"
                             , _stateError           = Nothing
                             , _statePopup           = Nothing
                             , _stateFormulaNameOp   = FormulaList
                             , _stateFormulaName     = emptyEditor
                             }

emptyState :: TuiState
emptyState = TuiState { _stateTitle           = "Brewsage"
                      , _stateFormulas        = emptyFormulaList
                      , _stateNumberFormulas  = 0
                      , _stateSelectedFormula = Nothing
                      , _stateStatus          = "No installed formulas found"
                      , _stateError           = Nothing
                      , _statePopup           = Nothing
                      , _stateFormulaNameOp   = FormulaList
                      , _stateFormulaName     = emptyEditor
                      }

emptyEditor :: E.Editor String RName
emptyEditor = E.editor FormulaName Nothing ""

emptyInstallFormulaInfoState :: FormulaInfoState
emptyInstallFormulaInfoState =
  FormState { _formulaInfoName = T.empty, _formulaInfoAction = InstallFormula }

emptySearchFormulaInfoState :: FormulaInfoState
emptySearchFormulaInfoState =
  FormState { _formulaInfoName = T.empty, _formulaInfoAction = SearchFormula }
