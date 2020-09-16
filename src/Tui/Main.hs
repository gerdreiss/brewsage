{-# LANGUAGE OverloadedStrings #-}

module Tui.Main
  ( tui
  )
where

import           Brick.AttrMap
import           Brick.Main
import           Brick.Types
import           Brick.Util
import qualified Brick.Widgets.Border.Style    as BS
import           Brick.Widgets.Core
import           Cursor.Simple.List.NonEmpty
import           Data.Brew
import qualified Data.ByteString.Lazy.Char8    as C8
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events
import qualified Tui.Widgets                   as W

data TuiState = TuiState
  { title :: String
  , status :: String
  , formulas :: NonEmptyCursor BrewFormula
  , selected :: Maybe BrewFormula
  }
  deriving (Show, Eq)

data FormulaName = FormulaName
  deriving (Eq, Show, Ord)

tui :: [BrewFormula] -> IO ()
tui fs = do
  initialState <- buildInitialState fs
  endState     <- defaultMain tuiApp initialState
  print $ fmap (C8.unpack . name) (formulas endState)

buildInitialState :: [BrewFormula] -> IO TuiState
buildInitialState fs = do
  let maybeFormulas = NE.nonEmpty fs
      df = NE.nonEmpty [BrewFormula { name = "", dependencies = [], dependants = [] }]
  case maybeFormulas of
    Nothing -> pure TuiState { title    = "Brewsage"
                             , status   = "No formulas found"
                             , formulas = makeNonEmptyCursor $ fromJust df
                             , selected = Nothing
                             }
    Just ne -> pure TuiState { title    = "Brewsage"
                             , status   = "Ready"
                             , formulas = makeNonEmptyCursor ne
                             , selected = Nothing
                             }

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
