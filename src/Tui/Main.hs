module Tui.Main
  ( tui
  )
where

import qualified Data.ByteString.Lazy.Char8    as C8
import qualified Data.List.NonEmpty            as NE
import qualified Tui.Popup                     as P
import qualified Tui.Widgets                   as W

import           Brick.AttrMap                  ( attrMap )
import           Brick.Main                     ( ViewportScroll
                                                , viewportScroll
                                                , vScrollBy
                                                , continue
                                                , defaultMain
                                                , halt
                                                , showFirstCursor
                                                , App(..)
                                                )
import           Brick.Types                    ( Widget
                                                , BrickEvent(VtyEvent)
                                                , EventM
                                                , Next
                                                )
import           Brick.Widgets.Core             ( hBox
                                                , vBox
                                                )
import           Control.Brew.Commands          ( listFormulas )
import           Control.Brew.Usage             ( getCompleteFormulaInfo )
import           Control.Monad.IO.Class         ( liftIO )
import           Cursor.Simple.List.NonEmpty    ( NonEmptyCursor
                                                , nonEmptyCursorSelectNext
                                                , nonEmptyCursorSelectPrev
                                                , nonEmptyCursorCurrent
                                                , makeNonEmptyCursor
                                                )
import           Data.Brew                      ( BrewFormula(..)
                                                , emptyFormula
                                                )
import           Graphics.Vty.Input.Events      ( Event(EvKey)
                                                , Key(KEnter, KChar, KDown, KUp)
                                                )
import           Tui.State                      ( TuiState(..)
                                                , buildInitialState
                                                )
import           Tui.Types                      ( RName(..) )


type ScrollDir = Int
type ScrollF = NonEmptyCursor BrewFormula -> Maybe (NonEmptyCursor BrewFormula)

tui :: [BrewFormula] -> IO ()
tui fs = buildInitialState fs >>= defaultMain tuiApp >>= printExitStatus
  where printExitStatus = print . maybe "Exited with success" show . stateError

tuiApp :: App TuiState e RName
tuiApp = App { appDraw         = drawTui
             , appChooseCursor = showFirstCursor
             , appStartEvent   = return --startTuiEvent
             , appHandleEvent  = handleTuiEvent
             , appAttrMap      = const $ attrMap mempty mempty
             }

drawTui :: TuiState -> [Widget RName]
drawTui s = maybe [ui s] (\p -> [P.renderPopup p, ui s]) (statePopup s)

ui :: TuiState -> Widget RName
ui s =
  let
    t   = stateTitle s
    fs  = stateFormulas s
    nfs = stateNumberFormulas s
    sel = stateSelectedFormula s
    st  = stateStatus s
    err = stateError s
  in
    vBox
      [ W.title t
      , hBox [W.formulas nfs fs, W.selected sel]
      , hBox [W.help, W.status st err]
      ]


startTuiEvent :: TuiState -> EventM RName TuiState
startTuiEvent s = do
  formulas <- liftIO listFormulas
  case formulas of
    Left err -> return s { stateError = Just err }
    Right [] ->
      return s { stateFormulas = makeNonEmptyCursor $ NE.fromList [emptyFormula] }
    Right fs -> return s { stateFormulas = makeNonEmptyCursor $ NE.fromList fs }

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM RName (Next TuiState)
handleTuiEvent s (VtyEvent (EvKey KDown _)) = scroll down nonEmptyCursorSelectNext s
handleTuiEvent s (VtyEvent (EvKey KUp _)) = scroll up nonEmptyCursorSelectPrev s
handleTuiEvent s (VtyEvent (EvKey KEnter _)) = displayFormula s
handleTuiEvent s (VtyEvent (EvKey (KChar 'a') _)) = continue s
  { statePopup = Just
                   $ P.popup "About" "About Brewsage" [("OK", continue), ("Cancel", halt)]
  }
handleTuiEvent s (VtyEvent (EvKey (KChar 'u') _)) = halt s -- TODO implement brew uninstall
handleTuiEvent s (VtyEvent (EvKey (KChar 's') _)) = halt s -- TODO implement brew search
handleTuiEvent s (VtyEvent (EvKey (KChar 'i') _)) = halt s -- TODO implement brew install
handleTuiEvent s (VtyEvent (EvKey (KChar 'U') _)) = halt s -- TODO implement brew upgrade
handleTuiEvent s (VtyEvent (EvKey (KChar 'q') _)) = halt s
handleTuiEvent s _ = continue s

scroll
  :: ScrollDir                          -- scroll direction: 1 = down, -1 = up
  -> ScrollF                            -- function to select the next/previous formula
  -> TuiState                           -- the TUI state
  -> EventM RName (Next TuiState)
scroll direction scrollF s = case scrollF . stateFormulas $ s of
  Nothing       -> continue s
  Just formulas -> do
    vScrollBy uiFormulaScroll direction
    continue $ s { stateFormulas = formulas }


displayFormula :: TuiState -> EventM RName (Next TuiState)
displayFormula s = do
  selected <- liftIO . getCompleteFormulaInfo . nonEmptyCursorCurrent . stateFormulas $ s
  case selected of
    Left  err     -> halt s { stateStatus = "Error occured", stateError = Just err }
    Right formula -> continue s
      { stateStatus          = (C8.unpack . formulaName $ formula) ++ " displayed"
      , stateSelectedFormula = Just formula
      }

uiFormulaScroll :: ViewportScroll RName
uiFormulaScroll = viewportScroll Formulas

down :: Int
down = 1

up :: Int
up = -1
