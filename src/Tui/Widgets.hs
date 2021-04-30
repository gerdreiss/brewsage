module Tui.Widgets
  ( title
  , formulas
  , mainArea
  , help
  , status
  ) where

import qualified Data.ByteString.Lazy.Char8    as C8

import           Brick                          ( (<+>)
                                                , Padding(Max, Pad)
                                                , ViewportType(Vertical)
                                                , Widget
                                                , emptyWidget
                                                , hBox
                                                , hLimit
                                                , padBottom
                                                , padLeft
                                                , padRight
                                                , padTop
                                                , padTopBottom
                                                , str
                                                , strWrap
                                                , vBox
                                                , vLimit
                                                , viewport
                                                , withBorderStyle
                                                )
import           Brick.Widgets.Border           ( border )
import           Brick.Widgets.Border.Style     ( unicodeBold )
import           Brick.Widgets.Center           ( hCenter
                                                , vCenter
                                                )
import           Brick.Widgets.Edit             ( renderEditor )
import           Cursor.Simple.List.NonEmpty    ( NonEmptyCursor
                                                , nonEmptyCursorCurrent
                                                , nonEmptyCursorNext
                                                , nonEmptyCursorPrev
                                                , nonEmptyCursorSearch
                                                )
import           Data.List                      ( isPrefixOf )
import           Data.Maybe                     ( fromMaybe )
import           Lens.Micro                     ( (^.) )

import           Data.Brew
import           Tui.State

--
--
title :: String -> Widget RName
title t =
  withBorderStyle unicodeBold . border . vLimit 1 . vCenter . hCenter . vBox $ [str t]

--
--
formulas :: TuiState -> Widget RName
formulas s = do
  let
    fs =
      formulaNames (s ^. stateFormulas) (s ^. stateFormulaNameOp) (getEditedFormulaName s)
   in withBorderStyle unicodeBold
    . border
    . hLimit leftWidth
    . vBox
    $ [ drawFormulaJumpTo s
      , viewport Formulas Vertical
      . vBox
      $ [ padTopBottom 1
          . hLimit leftWidth
          . vLimit (s ^. stateNumberFormulas)
          . vBox
          . concat
          $ [ drawFormula False <$> (reverse . nonEmptyCursorPrev $ fs)
            , drawFormula True <$> [nonEmptyCursorCurrent fs]
            , drawFormula False <$> nonEmptyCursorNext fs
            ]
        ]
      ]

formulaNames
  :: NonEmptyCursor BrewFormula -> FormulaOp -> String -> NonEmptyCursor BrewFormula
formulaNames fs op name = if op /= FormulaJumpTo || null name then fs else selectFormula fs name

selectFormula :: NonEmptyCursor BrewFormula -> String -> NonEmptyCursor BrewFormula
selectFormula fs name = fromMaybe fs (nonEmptyCursorSearch prefix fs)
  where prefix formula = name `isPrefixOf` C8.unpack (formulaName formula)

drawFormula :: Bool -> BrewFormula -> Widget RName
drawFormula isSelected formula =
  padRight Max
    . str
    . (prefix ++)
    . C8.unpack
    . C8.concat
    $ [formulaName formula, fromMaybe C8.empty version]
 where
  prefix  = if isSelected then " * " else "   "
  version = versionPrefix <$> formulaVersion formula
  versionPrefix v = C8.concat [C8.pack " v", v]

--
--
mainArea :: TuiState -> Widget RName
mainArea s =
  vCenter
    . hCenter
    . vBox
    $ [maybe selectFormulaText displayFormulaInfo selectedFormula, drawFormulaInput s]
 where
  selectFormulaText =
    padLeft (Pad 3)
      . padTop (Pad 1)
      . padBottom Max
      . strWrap
      $ "select a formula by pushing ENTER to display it here with its dependants and dependencies"
  displayFormulaInfo formula =
    vBox [displayInfo formula, displayDependencies formula, displayDependants formula]
  selectedFormula = s ^. stateSelectedFormula

displayInfo :: BrewFormula -> Widget RName
displayInfo formula =
  border
    . vCenter
    . hCenter
    . hBox
    $ [ padLeft (Pad 3)
        . padTop (Pad 1)
        . padRight Max
        . padBottom Max
        $ case formulaInfo formula of
            Nothing   -> strWrap "No further information available"
            Just info -> strWrap . C8.unpack $ info
      ]

--
--
displayDependencies :: BrewFormula -> Widget RName
displayDependencies formula = case formulaDependencies formula of
  [] -> emptyWidget
  ds ->
    border
      . vLimit 3
      . vCenter
      . hCenter
      . hBox
      $ [ padLeft (Pad 3)
          . padTop (Pad 1)
          . padBottom Max
          $ strWrap
          . ("Depends on " ++)
          . C8.unpack
          . C8.intercalate (C8.pack ", ")
          . fmap formulaName
          $ ds
        ]

--
--
displayDependants :: BrewFormula -> Widget RName
displayDependants formula = case formulaDependants formula of
  [] -> emptyWidget
  ds ->
    border
      . vLimit 3
      . vCenter
      . hCenter
      . hBox
      $ [ padLeft (Pad 3)
          . padTop (Pad 1)
          . padBottom Max
          $ strWrap
          . ("Required by " ++)
          . C8.unpack
          . C8.intercalate (C8.pack ", ")
          . fmap formulaName
          $ ds
        ]

--
--
status :: TuiState -> Widget RName
status s =
  withBorderStyle unicodeBold
    . border
    . vLimit bottomHeight
    . vCenter
    . hCenter
    . hBox
    $ [ vBox
          [ padLeft (Pad 3) . padRight Max . padBottom Max . strWrap $ maybe
              (s ^. stateStatus)
              show
              (s ^. stateError)
          ]
      ]

--
--
help :: Widget RName
help =
  withBorderStyle unicodeBold
    . border
    . hLimit leftWidth
    . vLimit bottomHeight
    . vCenter
    . hCenter
    . hBox
    $ [ padRight Max
        . vBox
        $ [ str "   ENTER : Display selected"
          , str "   /     : Jump to formula"
          , str "   s     : Search formula"
          , str "   i     : Install new"
          , str "   u     : Uninstall selected"
          , str "   U     : Update all"
          , str "   a     : About"
          , str "   q     : Exit"
          ]
      ]

drawFormulaJumpTo :: TuiState -> Widget RName
drawFormulaJumpTo s =
  if _stateFormulaNameOp s == FormulaJumpTo then drawFormulaNameEdit s else emptyWidget

drawFormulaInput :: TuiState -> Widget RName
drawFormulaInput s = if s ^. stateFormulaNameOp `elem` [FormulaSearch, FormulaInstall]
  then drawFormulaNameEdit s
  else emptyWidget

drawFormulaNameEdit :: TuiState -> Widget RName
drawFormulaNameEdit state =
  withBorderStyle unicodeBold . border . vLimit 1 . padLeft (Pad 3) $ label <+> editor
 where
  editor = renderEditor (str . unlines) True (state ^. stateFormulaNameEditL)
  label  = str . (++ ": ") . show . _stateFormulaNameOp $ state

leftWidth :: Int
leftWidth = 30

bottomHeight :: Int
bottomHeight = 8
