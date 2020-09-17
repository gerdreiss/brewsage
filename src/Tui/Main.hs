module Tui.Main
  ( tui
  )
where

import qualified Tui.Widgets                   as W

import           Data.Brew                      ( BrewFormula )
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
tui fs = buildInitialState fs >>= defaultMain tuiApp >> return ()

tuiApp :: App TuiState e UIFormulas
tuiApp = App { appDraw         = drawTui
             , appChooseCursor = showFirstCursor
             , appHandleEvent  = handleTuiEvent
             , appStartEvent   = pure
             , appAttrMap      = const $ attrMap mempty mempty
             }

drawTui :: TuiState -> [Widget UIFormulas]
drawTui s =
  let t   = stateTitle s
      fs  = stateFormulas s
      nfs = stateNumberFormulas s
      sel = stateSelected s
      st  = stateStatus s
  in  [vBox [W.title t, hBox [W.formulas nfs fs, W.selected sel], W.status st]]

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM UIFormulas (Next TuiState)
handleTuiEvent s (VtyEvent (EvKey (KChar 'q') _)) = halt s
handleTuiEvent s (VtyEvent (EvKey KDown _)) = scroll down nonEmptyCursorSelectNext s
handleTuiEvent s (VtyEvent (EvKey KUp _)) = scroll up nonEmptyCursorSelectPrev s
handleTuiEvent s (VtyEvent (EvKey KEnter _)) = select s
handleTuiEvent s _ = continue s

scroll
  :: ScrollDir                          -- scroll direction: 1 = down, -1 = up
  -> ScrollF                            -- function to select the next/previous formula
  -> TuiState                           -- the state
  -> EventM UIFormulas (Next TuiState)
scroll d f s = case f (stateFormulas s) of
  Nothing       -> continue s
  Just formulas -> do
    vScrollBy uiFormulaScroll d
    continue $ s { stateFormulas = formulas }

down :: Int
down = 1

up :: Int
up = -1

uiFormulaScroll :: ViewportScroll UIFormulas
uiFormulaScroll = viewportScroll UIFormulas

select :: TuiState -> EventM UIFormulas (Next TuiState)
select s = continue s { stateSelected = Just (nonEmptyCursorCurrent $ stateFormulas s) }
