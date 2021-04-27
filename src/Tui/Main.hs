module Tui.Main
  ( tui
  ) where

import qualified Brick.Widgets.Edit            as E
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
                                                , handleEventLensed
                                                )
import           Brick.Widgets.Core             ( hBox
                                                , vBox
                                                )
import           Control.Brew.Commands          ( getFormulaInfo
                                                , listFormulas
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
import           Data.Brew                      ( BrewFormula(..)
                                                , mkFormula
                                                )
import           Data.Char                      ( toLower )
import           Graphics.Vty.Input.Events      ( Event(EvKey)
                                                , Key
                                                  ( KChar
                                                  , KDel
                                                  , KDown
                                                  , KEnter
                                                  , KEsc
                                                  , KUp
                                                  )
                                                )
import           Lens.Micro                     ( (^.) )
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
ui s = vBox
  [ W.title (s ^. stateTitle)
  , hBox [W.formulas s, W.mainArea s]
  , hBox [W.help, W.status s]
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
handleTuiEvent s (VtyEvent (EvKey KEsc _)) = handleEscEvent s
handleTuiEvent s (VtyEvent (EvKey KEnter _)) = handleEnterEvent s displayFormula
handleTuiEvent s (VtyEvent ev@(EvKey KDel _)) = handleChrEvent ev s displayFormula
handleTuiEvent s (VtyEvent ev@(EvKey (KChar 'a') _)) = handleChrEvent ev s displayAbout
handleTuiEvent s (VtyEvent ev@(EvKey (KChar 'U') _)) = handleChrEvent ev s upgradeAll
handleTuiEvent s (VtyEvent ev@(EvKey (KChar 'h') _)) = handleChrEvent ev s displayHelp
handleTuiEvent s (VtyEvent ev@(EvKey (KChar 'f') _)) = handleChrEvent ev s displayFilter
handleTuiEvent s (VtyEvent ev@(EvKey (KChar 's') _)) = handleChrEvent ev s displaySearch
handleTuiEvent s (VtyEvent ev@(EvKey (KChar 'i') _)) = handleChrEvent ev s displayInstall
handleTuiEvent s (VtyEvent ev@(EvKey (KChar 'u') _)) = handleChrEvent ev s uninstall
handleTuiEvent s (VtyEvent ev@(EvKey (KChar 'q') _)) = handleChrEvent ev s halt
handleTuiEvent s (VtyEvent ev@(EvKey _ _)) = handleOtherKeys ev s
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

-- | handle escape event
handleEscEvent :: TuiState -> NewState
handleEscEvent s = continue s { _statePopup           = Nothing
                              , _stateFormulaNameOp   = FormulaList
                              , _stateFormulaNameEdit = emptyEditor
                              }

-- | handle the enter
handleEnterEvent :: TuiState -> (TuiState -> NewState) -> NewState
handleEnterEvent s f = maybe execFormulaOp (const $ continue s) (s ^. statePopup)
 where
  execFormulaOp = do
    let name = map toLower $ unwords $ E.getEditContents $ s ^. stateFormulaNameEdit
    case s ^. stateFormulaNameOp of
      FormulaList    -> f s
      FormulaSearch  -> searchDisplayFormula name s
      FormulaInstall -> continue s
      FormulaFilter  -> continue s

-- | handle character input depending on the popup being displayed or not
handleChrEvent :: Event -> TuiState -> (TuiState -> NewState) -> NewState
handleChrEvent ev s f = maybe
  (if s ^. stateFormulaNameOp == FormulaList then f s else handleEditor s ev)
  (const $ continue s)
  (s ^. statePopup)

handleEditor :: TuiState -> Event -> NewState
handleEditor state event =
  continue =<< handleEventLensed state stateFormulaNameEditL E.handleEditorEvent event

-- | handle other events
handleOtherKeys :: Event -> TuiState -> NewState
handleOtherKeys ev s = maybe
  (if s ^. stateFormulaNameOp == FormulaList then continue s else handleEditor s ev)
  (const $ continue s)
  (s ^. statePopup)

-- | display the selected formula
displayFormula :: TuiState -> NewState
displayFormula s = do
  selected <- liftIO . getCompleteFormulaInfo . nonEmptyCursorCurrent . _stateFormulas $ s
  case selected of
    Left err -> continue s { _stateStatus = "Error occurred", _stateError = Just err }
    Right formula -> continue s
      { _stateStatus          = (C8.unpack . formulaName $ formula) ++ " displayed"
      , _stateSelectedFormula = Just formula
      }

-- | search and display formula info
searchDisplayFormula :: String -> TuiState -> NewState
searchDisplayFormula name s = do
  info <- liftIO $ getFormulaInfo True $ mkFormula name
  case info of
    Left  err     -> continue s { _stateStatus          = "Error occurred"
                                , _stateError           = Just err
                                , _stateFormulaNameOp   = FormulaList
                                , _stateFormulaNameEdit = emptyEditor
                                }
    Right formula -> continue s
      { _stateStatus          = (++ " found") . C8.unpack . formulaName $ formula
      , _stateSelectedFormula = Just formula
      , _stateFormulaNameOp   = FormulaList
      , _stateFormulaNameEdit = emptyEditor
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
displayFilter s = continue s { _stateFormulaNameOp = FormulaFilter }

-- | display the search dialog
displaySearch :: TuiState -> NewState
displaySearch s =
  continue s { _stateFormulaNameOp = FormulaSearch, _stateSelectedFormula = Nothing }

-- | display the install dialog
displayInstall :: TuiState -> NewState
displayInstall s =
  continue s { _stateFormulaNameOp = FormulaInstall, _stateSelectedFormula = Nothing }

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
