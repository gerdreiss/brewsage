module Tui.Main
  ( tui
  )
where

import qualified Tui.Widgets                   as W

import           Data.Brew                      ( BrewFormula )
import           Brick.AttrMap                  ( attrMap )
import           Brick.Main                     ( continue
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
import           Cursor.Simple.List.NonEmpty    ( nonEmptyCursorSelectNext
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
      sel = stateSelected s
      st  = stateStatus s
  in  [vBox [W.title t, hBox [W.formulas fs, W.selected sel], W.status st]]

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'q') [] -> halt s
    EvKey KDown       [] -> do
      let nec = stateFormulas s
      case nonEmptyCursorSelectNext nec of
        Nothing   -> continue s
        Just nec' -> continue $ s { stateFormulas = nec' }
    EvKey KUp         [] -> do
      let nec = stateFormulas s
      case nonEmptyCursorSelectPrev nec of
        Nothing   -> continue s
        Just nec' -> continue $ s { stateFormulas = nec' }
    EvKey KEnter      [] -> do
      let sel = nonEmptyCursorCurrent $ stateFormulas s
      continue s { stateSelected = Just sel }
    _                    -> continue s
  _             -> continue s
