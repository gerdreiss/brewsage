{-# LANGUAGE OverloadedStrings #-}

module Tui.Main where

import           Brick.AttrMap
import           Brick.Main
import           Brick.Types
import           Brick.Util
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import           Brick.Widgets.Core
import           Cursor.Simple.List.NonEmpty
import           Data.Brew
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C8
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events

data TuiState = TuiState
  { title :: String
  , status :: String
  , formulas :: NonEmptyCursor BrewFormula
  , selected :: Maybe BrewFormula
  }
  deriving (Show, Eq)

data FormulaName = FormulaName
  deriving (Eq, Show, Ord)

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState     <- defaultMain tuiApp initialState
  print endState

buildInitialState :: IO TuiState
buildInitialState = do
  let fs = NE.nonEmpty [BrewFormula { name = "the-formula", dependencies = [], dependants = [] }]
      df = NE.nonEmpty [BrewFormula { name = "", dependencies = [], dependants = [] }]
  case fs of
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
      st  = status s
      fs  = formulas s
      sel = selected s
  in  [ withBorderStyle BS.unicodeRounded $ C.vCenter $ C.hCenter $ vBox
          [ B.border $ vLimit 1 $ C.vCenter $ C.hCenter $ vBox [str t]
          , hBox
            [ withBorderStyle BS.unicodeRounded
            $ B.borderWithLabel (str " Formulas ")
            $ hLimit 20
            $ C.vCenter
            $ C.hCenter
            $ vBox
            $ concat
                [ map (drawFormula False) . reverse . nonEmptyCursorPrev $ fs
                , [drawFormula True $ nonEmptyCursorCurrent fs]
                , map (drawFormula False) $ nonEmptyCursorNext fs
                ]
            , withBorderStyle BS.unicodeRounded
            $ B.borderWithLabel (str " Selected Formula ")
            $ C.vCenter
            $ C.hCenter
            $ vBox [str " selected formula "]
            ]
          , B.border $ vLimit 1 $ C.vCenter $ C.hCenter $ vBox [str st]
          ]
      ]

drawFormula :: Bool -> BrewFormula -> Widget n
drawFormula b = (if b then withAttr "selected" else id) . str . C8.unpack . name

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
      let selected = nonEmptyCursorCurrent $ formulas s
      continue s { selected = Just selected }
    _                    -> continue s
  _             -> continue s
