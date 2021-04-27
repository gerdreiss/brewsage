module Tui.Widgets
  ( title
  , formulas
  , mainArea
  , help
  , status
  ) where

import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Edit            as E
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
import           Cursor.Simple.List.NonEmpty    ( nonEmptyCursorCurrent
                                                , nonEmptyCursorNext
                                                , nonEmptyCursorPrev
                                                )
import           Data.Brew                      ( BrewFormula
                                                  ( formulaDependants
                                                  , formulaDependencies
                                                  , formulaInfo
                                                  , formulaName
                                                  , formulaVersion
                                                  )
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Lens.Micro                     ( (^.) )
import           Tui.State

--
--
title :: String -> Widget RName
title t =
  withBorderStyle BS.unicodeBold
    . B.border
    . vLimit 1
    . C.vCenter
    . C.hCenter
    . vBox
    $ [str t]

--
--
formulas :: TuiState -> Widget RName
formulas s =
  withBorderStyle BS.unicodeBold
    . B.border
    . hLimit leftWidth
    . vBox
    $ [ drawFormulaFilter s
      , viewport Formulas Vertical
      . vBox
      $ [ padTopBottom 1
          . hLimit leftWidth
          . vLimit (s ^. stateNumberFormulas)
          . vBox
          . concat
          $ [ drawFormula False <$> (reverse . nonEmptyCursorPrev . _stateFormulas $ s)
            , drawFormula True <$> [nonEmptyCursorCurrent . _stateFormulas $ s]
            , drawFormula False <$> (nonEmptyCursorNext . _stateFormulas $ s)
            ]
        ]
      ]

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
  C.vCenter
    . C.hCenter
    . vBox
    $ [maybe selectFormulaText displaySelected selectedFormula, drawFormulaInput s]
 where
  selectFormulaText =
    padLeft (Pad 3)
      . padTop (Pad 1)
      . padBottom Max
      . strWrap
      $ "select a formula by pushing ENTER to display it here with its dependants and dependencies"
  selectedFormula = s ^. stateSelectedFormula

displaySelected :: BrewFormula -> Widget RName
displaySelected formula =
  vBox [displayInfo formula, displayDependencies formula, displayDependants formula]

displayInfo :: BrewFormula -> Widget RName
displayInfo formula =
  B.border
    . C.vCenter
    . C.hCenter
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
displayDependencies formula =
  B.border
    . vLimit 3
    . C.vCenter
    . C.hCenter
    . hBox
    $ [ padLeft (Pad 3)
        . padTop (Pad 1)
        . padBottom Max
        $ case formulaDependencies formula of
            [] -> strWrap "Does not depend on any other formula"
            ds ->
              strWrap
                . ("Depends on " ++)
                . C8.unpack
                . C8.intercalate (C8.pack ", ")
                . map formulaName
                $ ds
      ]

--
--
displayDependants :: BrewFormula -> Widget RName
displayDependants formula =
  B.border
    . vLimit 3
    . C.vCenter
    . C.hCenter
    . hBox
    $ [ padLeft (Pad 3)
        . padTop (Pad 1)
        . padBottom Max
        $ case formulaDependants formula of
            [] -> strWrap "Not required by any other formula"
            ds ->
              strWrap
                . ("Required by " ++)
                . C8.unpack
                . C8.intercalate (C8.pack ", ")
                . map formulaName
                $ ds
      ]

--
--
status :: TuiState -> Widget RName
status s =
  withBorderStyle BS.unicodeBold
    . B.border
    . vLimit bottomHeight
    . C.vCenter
    . C.hCenter
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
  withBorderStyle BS.unicodeBold
    . B.border
    . hLimit leftWidth
    . vLimit bottomHeight
    . C.vCenter
    . C.hCenter
    . hBox
    $ [ padRight Max
        . vBox
        $ [ str "   ENTER : Display selected"
          , str "   f     : Filter formula"
          , str "   s     : Search formula"
          , str "   i     : Install new"
          , str "   u     : Uninstall selected"
          , str "   U     : Update all"
          , str "   h     : Display usage"
          , str "   q     : Exit"
          ]
      ]

drawFormulaFilter :: TuiState -> Widget RName
drawFormulaFilter s =
  if _stateFormulaNameOp s == FormulaFilter then drawFormulaNameEdit s else emptyWidget

drawFormulaInput :: TuiState -> Widget RName
drawFormulaInput s = if s ^. stateFormulaNameOp `elem` [FormulaSearch, FormulaInstall]
  then drawFormulaNameEdit s
  else emptyWidget

drawFormulaNameEdit :: TuiState -> Widget RName
drawFormulaNameEdit state =
  withBorderStyle BS.unicodeBold
    .   B.border
    .   vLimit 1
    .   padLeft (Pad 3)
    $   label
    <+> editor
 where
  editor = E.renderEditor (str . unlines) True (state ^. stateFormulaNameEditL)
  label  = str . (++ ": ") . show . _stateFormulaNameOp $ state

leftWidth :: Int
leftWidth = 30

bottomHeight :: Int
bottomHeight = 8
