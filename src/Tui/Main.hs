module Tui.Main
  ( tui
  ) where

import qualified Data.ByteString.Lazy.Char8    as C8
import qualified Data.List.NonEmpty            as NE
import qualified Tui.Popup                     as P
import qualified Tui.Widgets                   as W

import           Brick.AttrMap                  ( attrMap )
import           Brick.Main                     ( App(..)
                                                , continue
                                                , defaultMain
                                                , halt
                                                , showFirstCursor
                                                , suspendAndResume
                                                , vScrollBy
                                                , viewportScroll
                                                )
import           Brick.Types                    ( BrickEvent(VtyEvent)
                                                , Widget
                                                )
import           Brick.Widgets.Core             ( hBox
                                                , vBox
                                                )
import           Control.Brew.Commands          ( listFormulas
                                                , uninstallFormula
                                                , upgradeAllFormulas
                                                )
import           Control.Brew.Usage             ( getCompleteFormulaInfo )
import           Control.Monad.IO.Class         ( liftIO )
import           Cursor.Simple.List.NonEmpty    ( NonEmptyCursor
                                                , makeNonEmptyCursor
                                                , nonEmptyCursorCurrent
                                                , nonEmptyCursorSelectNext
                                                , nonEmptyCursorSelectPrev
                                                )
import           Data.Brew                      ( BrewFormula(..) )
import           Graphics.Vty.Input.Events      ( Event(EvKey)
                                                , Key(KChar, KDown, KEnter, KEsc, KUp)
                                                )
import           Lens.Micro
import           Tui.State


type ScrollDir = Int
type ScrollF = NonEmptyCursor BrewFormula -> Maybe (NonEmptyCursor BrewFormula)

--
--
-- | display the TUI
tui :: [BrewFormula] -> IO ()
tui fs = buildInitialState fs >>= defaultMain tuiApp >>= printExitStatus
  where printExitStatus = print . maybe "Exited with success" show . _stateError

-- | create the application state
tuiApp :: App TuiState e RName
tuiApp = App { appDraw         = drawTui
             , appChooseCursor = showFirstCursor
             , appStartEvent   = return --startTuiEvent
             , appHandleEvent  = handleTuiEvent
             , appAttrMap      = const $ attrMap mempty mempty
             }

-- | draw the TUI
drawTui :: TuiState -> [Widget RName]
drawTui s = maybe [ui s] (\p -> [P.renderPopup p, ui s]) (s ^. statePopup)

-- | create the TUI widget
ui :: TuiState -> Widget RName
ui s =
  let
    t   = _stateTitle s
    fs  = _stateFormulas s
    nfs = _stateNumberFormulas s
    sel = _stateSelectedFormula s
    st  = _stateStatus s
    err = _stateError s
  in
    vBox
      [ W.title t
      , hBox [W.formulas nfs fs, W.selected sel]
      , hBox [W.help, W.status st err]
      ]

-- | initial event - not used for now
-- startTuiEvent :: TuiState -> EventM RName TuiState
-- startTuiEvent s = do
--   formulas <- liftIO listFormulas
--   case formulas of
--     Left err -> return s { stateError = Just err }
--     Right [] ->
--       return s { stateFormulas = makeNonEmptyCursor $ NE.fromList [emptyFormula] }
--     Right fs -> return s { stateFormulas = makeNonEmptyCursor $ NE.fromList fs }

-- | handle TUI events
handleTuiEvent :: TuiState -> BrickEvent n e -> NewState
handleTuiEvent s (VtyEvent (EvKey KDown _)) = scroll down nonEmptyCursorSelectNext s
handleTuiEvent s (VtyEvent (EvKey KUp _)) = scroll up nonEmptyCursorSelectPrev s
handleTuiEvent s (VtyEvent (EvKey KEsc _)) = continue s { _statePopup = Nothing }
handleTuiEvent s (VtyEvent (EvKey KEnter _)) = handleCharacterEvent s displayFormula
handleTuiEvent s (VtyEvent (EvKey (KChar 'a') _)) = handleCharacterEvent s displayAbout
handleTuiEvent s (VtyEvent (EvKey (KChar 'U') _)) = handleCharacterEvent s upgradeAll
handleTuiEvent s (VtyEvent (EvKey (KChar 'h') _)) = handleCharacterEvent s displayHelp
handleTuiEvent s (VtyEvent (EvKey (KChar 'f') _)) = handleCharacterEvent s displayFilter
handleTuiEvent s (VtyEvent (EvKey (KChar 's') _)) = handleCharacterEvent s displaySearch
handleTuiEvent s (VtyEvent (EvKey (KChar 'i') _)) = handleCharacterEvent s displayInstall
handleTuiEvent s (VtyEvent (EvKey (KChar 'u') _)) = handleCharacterEvent s uninstall
handleTuiEvent s (VtyEvent (EvKey (KChar 'q') _)) = handleCharacterEvent s halt
handleTuiEvent s _ = continue s

-- | scroll viewport
scroll
  :: ScrollDir                          -- scroll direction: 1 = down, -1 = up
  -> ScrollF                            -- function to select the next/previous formula
  -> TuiState                           -- the TUI state
  -> NewState
scroll direction scrollF s = case scrollF . _stateFormulas $ s of
  Nothing       -> continue s
  Just formulas -> do
    vScrollBy (viewportScroll Formulas) direction
    continue $ s { _stateFormulas = formulas }

-- | scroll down constant
down :: Int
down = 1

-- | scroll up constant
up :: Int
up = -1

-- | handle character input depending on the popup being displayed or not
handleCharacterEvent :: TuiState -> (TuiState -> NewState) -> NewState
handleCharacterEvent s f = maybe (f s) (const $ continue s) (_statePopup s)

-- | display the selected formula
displayFormula :: TuiState -> NewState
displayFormula s = do
  selected <- liftIO . getCompleteFormulaInfo . nonEmptyCursorCurrent . _stateFormulas $ s
  case selected of
    Left  err     -> halt s { _stateStatus = "Error occurred", _stateError = Just err }
    Right formula -> continue s
      { _stateStatus          = (C8.unpack . formulaName $ formula) ++ " displayed"
      , _stateSelectedFormula = Just formula
      }

-- | display the 'About' dialog
displayAbout :: TuiState -> NewState
displayAbout s = continue s
  { _statePopup = Just $ P.popup
                    "About"
                    [ "Brewsage - a TUI for homebrew (https://brew.sh/)"
                    , "Powered by Brick (https://github.com/jtdaugherty/brick)"
                    , "Written in Haskell (https://www.haskell.org/)"
                    , "Hosted by GitHub (https://github.com/gerdreiss/brewsage)"
                    , "Copyright (c) 2020, Gerd Reiss"
                    , ""
                    , ""
                    , "                                          [ESC to close]"
                    ]
                    []
  }

-- | display the help text
displayHelp :: TuiState -> NewState
displayHelp s = continue s
  { _statePopup = Just $ P.popup
                    "Search formula"
                    [ "Displaying help is not implemented yet"
                    , ""
                    , "                                          [ESC to close]"
                    ]
                    []
  }

-- | display the filter field
displayFilter :: TuiState -> NewState
displayFilter s = continue s
  { _statePopup = Just $ P.popup
                    "Search formula"
                    [ "Displaying filter is not implemented yet"
                    , ""
                    , "                                          [ESC to close]"
                    ]
                    []
  }

-- | display the search dialog
displaySearch :: TuiState -> NewState
displaySearch s = continue s
  { _statePopup = Just $ P.popup
                    "Search formula"
                    [ "Searching formula information is not implemented yet"
                    , ""
                    , "                                          [ESC to close]"
                    ]
                    []
  }

-- | display the install dialog
displayInstall :: TuiState -> NewState
displayInstall s = continue s
  { _statePopup = Just $ P.popup
                    "Install formula"
                    [ "Installing new formula is not implemented yet"
                    , ""
                    , "                                          [ESC to close]"
                    ]
                    []
  }

-- | upgrade all formulas
upgradeAll :: TuiState -> NewState
upgradeAll s = suspendAndResume $ do
  upgraded <- upgradeAllFormulas
  case upgraded of
    Left  err      -> return s { _stateStatus = "Error occurred", _stateError = Just err }
    Right formulas -> return s
      { _stateFormulas        = makeNonEmptyCursor $ NE.fromList formulas
      , _stateSelectedFormula = Nothing
      , _stateStatus          = "Formulas upgraded:\n\t - "
                                  ++ ( C8.unpack
                                     . C8.intercalate "\n\t - "
                                     . map formulaName
                                     $ formulas
                                     )
      }

-- | uninstall the selected formula
uninstall :: TuiState -> NewState
uninstall s = suspendAndResume $ do
  let maybeFormula = _stateSelectedFormula s
  case maybeFormula of
    Nothing      -> return s { _stateStatus = "Select a formula before deleting" }
    Just formula -> do
      formulas <- uninstallFormula formula >> listFormulas
      case formulas of
        Left  err -> return s { _stateStatus = "Error occurred", _stateError = Just err }
        Right fs  -> return s
          { _stateFormulas        = makeNonEmptyCursor $ NE.fromList fs
          , _stateSelectedFormula = Nothing
          , _stateStatus          = (C8.unpack . formulaName $ formula) ++ " uninstalled"
          }
