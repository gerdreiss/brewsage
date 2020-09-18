{-# LANGUAGE OverloadedStrings #-}

module Tui.Main
  ( tui
  )
where

import qualified Tui.Widgets                   as W
import qualified Data.List.NonEmpty            as NE

import           Control.Brew.Commands          ( listFormulas )
import           Control.Brew.Usage             ( getCompleteFormulaInfo )
import           Control.Monad.IO.Class         ( liftIO )
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
import           Cursor.Simple.List.NonEmpty    ( NonEmptyCursor
                                                , nonEmptyCursorSelectNext
                                                , nonEmptyCursorSelectPrev
                                                , nonEmptyCursorCurrent
                                                , makeNonEmptyCursor
                                                )
import           Data.Brew                      ( BrewFormula
                                                , emptyFormula
                                                )
import           Graphics.Vty.Input.Events      ( Event(EvKey)
                                                , Key(KEnter, KChar, KDown, KUp)
                                                )
import           Tui.State                      ( TuiState(..)
                                                , buildInitialState
                                                )
import           Tui.Types                      ( UIFormulas(..) )


type ScrollDir = Int
type ScrollF = NonEmptyCursor BrewFormula -> Maybe (NonEmptyCursor BrewFormula)

tui :: [BrewFormula] -> IO ()
tui fs = buildInitialState fs >>= defaultMain tuiApp >>= printExitStatus
  where printExitStatus = print . maybe "Exited with success" show . stateError

tuiApp :: App TuiState e UIFormulas
tuiApp = App { appDraw         = drawTui
             , appChooseCursor = showFirstCursor
             , appStartEvent   = pure --startTuiEvent
             , appHandleEvent  = handleTuiEvent
             , appAttrMap      = const $ attrMap mempty mempty
             }

drawTui :: TuiState -> [Widget UIFormulas]
drawTui s =
  let
    t   = stateTitle s
    fs  = stateFormulas s
    nfs = stateNumberFormulas s
    sel = stateSelected s
    st  = stateStatus s
  in
    [ vBox
        [W.title t, hBox [W.formulas nfs fs, W.selected sel], hBox [W.help, W.status st]]
    ]

startTuiEvent :: TuiState -> EventM UIFormulas TuiState
startTuiEvent s = do
  formulas <- liftIO listFormulas
  case formulas of
    Left err -> return s { stateError = Just err }
    Right [] ->
      return s { stateFormulas = makeNonEmptyCursor $ NE.fromList [emptyFormula] }
    Right fs -> return s { stateFormulas = makeNonEmptyCursor $ NE.fromList fs }


handleTuiEvent :: TuiState -> BrickEvent n e -> EventM UIFormulas (Next TuiState)
handleTuiEvent s (VtyEvent (EvKey (KChar 'q') _)) = halt s
handleTuiEvent s (VtyEvent (EvKey KDown _)) = scroll down nonEmptyCursorSelectNext s
handleTuiEvent s (VtyEvent (EvKey KUp _)) = scroll up nonEmptyCursorSelectPrev s
handleTuiEvent s (VtyEvent (EvKey KEnter _)) = displayFormula s
handleTuiEvent s _ = continue s

scroll
  :: ScrollDir                          -- scroll direction: 1 = down, -1 = up
  -> ScrollF                            -- function to select the next/previous formula
  -> TuiState                           -- the TUI state
  -> EventM UIFormulas (Next TuiState)
scroll direction scrollF s = case scrollF . stateFormulas $ s of
  Nothing       -> continue s
  Just formulas -> do
    vScrollBy uiFormulaScroll direction
    continue $ s { stateFormulas = formulas }

uiFormulaScroll :: ViewportScroll UIFormulas
uiFormulaScroll = viewportScroll UIFormulas

down :: Int
down = 1

up :: Int
up = -1

displayFormula :: TuiState -> EventM UIFormulas (Next TuiState)
displayFormula s = do
  selected <- liftIO . getCompleteFormulaInfo . nonEmptyCursorCurrent . stateFormulas $ s
  case selected of
    Left  err     -> halt s { stateError = Just err }
    Right formula -> continue s { stateSelected = Just formula }
