{-# LANGUAGE OverloadedStrings #-}

module Tui.Main
  ( tui
  )
where

import qualified Brick.Widgets.Border.Style    as BS
import qualified Data.ByteString.Lazy.Char8    as C8
import qualified Tui.Widgets                   as W

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
import           Brick.Util                     ( fg )
import           Brick.Widgets.Core             ( hBox
                                                , vBox
                                                , withBorderStyle
                                                )
import           Cursor.Simple.List.NonEmpty    ( nonEmptyCursorSelectNext
                                                , nonEmptyCursorSelectPrev
                                                , nonEmptyCursorCurrent
                                                )
import           Data.Brew                      ( BrewFormula(name) )
import           Graphics.Vty.Attributes        ( red )
import           Graphics.Vty.Input.Events      ( Event(EvKey)
                                                , Key(KEnter, KChar, KDown, KUp)
                                                )
import           Tui.State                      ( TuiState(..)
                                                , buildInitialState
                                                )


data FormulaName = FormulaName
  deriving (Eq, Show, Ord)

tui :: [BrewFormula] -> IO ()
tui fs = do
  initialState <- buildInitialState fs
  endState     <- defaultMain tuiApp initialState
  print $ fmap (C8.unpack . name) (formulas endState)

tuiApp :: App TuiState e FormulaName
tuiApp = App { appDraw         = drawTui
             , appChooseCursor = showFirstCursor
             , appHandleEvent  = handleTuiEvent
             , appStartEvent   = pure
             , appAttrMap      = const $ attrMap mempty [("selected", fg red)]
             }

drawTui :: TuiState -> [Widget FormulaName]
drawTui s =
  let t   = title s
      fs  = formulas s
      sel = selected s
      st  = status s
  in  [ withBorderStyle BS.unicodeBold
          $ vBox [W.title t, hBox [W.formulas fs, W.selected sel], W.status st]
      ]

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'q') [] -> halt s
    EvKey KDown       [] -> do
      let nec = formulas s
      case nonEmptyCursorSelectNext nec of
        Nothing   -> continue s
        Just nec' -> continue $ s { formulas = nec' }
    EvKey KUp         [] -> do
      let nec = formulas s
      case nonEmptyCursorSelectPrev nec of
        Nothing   -> continue s
        Just nec' -> continue $ s { formulas = nec' }
    EvKey KEnter      [] -> do
      let sel = nonEmptyCursorCurrent $ formulas s
      continue s { selected = Just sel }
    _                    -> continue s
  _             -> continue s
