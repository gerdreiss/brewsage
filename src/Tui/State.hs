module Tui.State where

import qualified Data.List.NonEmpty            as NE

import           Brick                          ( EventM
                                                , Next
                                                )
import           Brick.Widgets.Edit             ( Editor
                                                , editor
                                                , getEditContents
                                                )
import           Cursor.Simple.List.NonEmpty    ( NonEmptyCursor
                                                , makeNonEmptyCursor
                                                )
import           Data.Char                      ( toLower )
import           Lens.Micro                     ( Lens'
                                                , lens
                                                )
import           Lens.Micro.TH                  ( makeLenses )

import           Data.Brew
import           Tui.Popup                      ( Popup )

type NewState = EventM RName (Next TuiState)

data RName
  = Formulas
  | FormulaName
  deriving (Eq, Ord, Show)

data FormulaOp
  = FormulaList
  | FormulaJumpTo
  | FormulaSearch
  | FormulaInstall
  deriving (Eq)

data TuiState = TuiState
  { _stateTitle           :: String
  , _stateFormulas        :: NonEmptyCursor BrewFormula
  , _stateNumberFormulas  :: Int
  , _stateSelectedFormula :: Maybe BrewFormula
  , _stateStatus          :: String
  , _stateError           :: Maybe BrewError
  , _statePopup           :: Maybe (Popup RName (TuiState -> NewState))
  , _stateFormulaNameEdit :: Editor String RName
  , _stateFormulaNameOp   :: FormulaOp
  }

instance Show FormulaOp where
  show FormulaList    = ""
  show FormulaJumpTo  = "Jump to"
  show FormulaSearch  = "Search formula"
  show FormulaInstall = "Install formula"

makeLenses ''TuiState

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
                             , _stateFormulaNameEdit = emptyEditor
                             , _stateFormulaNameOp   = FormulaList
                             }

emptyState :: TuiState
emptyState = TuiState { _stateTitle           = "Brewsage"
                      , _stateFormulas        = emptyFormulaList
                      , _stateNumberFormulas  = 0
                      , _stateSelectedFormula = Nothing
                      , _stateStatus          = "No installed formulas found"
                      , _stateError           = Nothing
                      , _statePopup           = Nothing
                      , _stateFormulaNameEdit = emptyEditor
                      , _stateFormulaNameOp   = FormulaList
                      }

emptyEditor :: Editor String RName
emptyEditor = editor FormulaName Nothing ""

stateFormulaNameEditL :: Lens' TuiState (Editor String RName)
stateFormulaNameEditL =
  lens _stateFormulaNameEdit (\state edit -> state { _stateFormulaNameEdit = edit })

stateFormulaNameOpL :: Lens' TuiState FormulaOp
stateFormulaNameOpL =
  lens _stateFormulaNameOp (\state op -> state { _stateFormulaNameOp = op })

getEditedFormulaName :: TuiState -> String
getEditedFormulaName = fmap toLower . unwords . getEditContents . _stateFormulaNameEdit
