{-# LANGUAGE OverloadedStrings #-}

module Tui.Widgets
  ( title
  , formulas
  , selected
  , help
  , status
  )
where

import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import qualified Data.ByteString.Lazy.Char8    as C8

import           Brick                          ( Padding(Pad) )
import           Brick.Types                    ( ViewportType(Vertical)
                                                , Padding(Max)
                                                , Widget
                                                )
import           Brick.Widgets.Core             ( hBox
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
import           Cursor.Simple.List.NonEmpty    ( NonEmptyCursor
                                                , nonEmptyCursorCurrent
                                                , nonEmptyCursorNext
                                                , nonEmptyCursorPrev
                                                )
import           Data.Brew                      ( BrewError(..)
                                                , BrewFormula(..)
                                                )
import           Tui.Types                      ( RName(..) )

title :: String -> Widget RName
title t =
  withBorderStyle BS.unicodeBold
    . B.border
    . vLimit 1
    . C.vCenter
    . C.hCenter
    . vBox
    $ [str t]

formulas :: Int -> NonEmptyCursor BrewFormula -> Widget RName
formulas nfs fs =
  withBorderStyle BS.unicodeBold
    . B.border
    . hLimit leftWidth
    . vBox
    $ [ viewport Formulas Vertical
        . vBox
        $ [ padTopBottom 1
            . hLimit leftWidth
            . vLimit nfs
            . vBox
            . concat
            $ [ drawFormula False <$> (reverse . nonEmptyCursorPrev $ fs)
              , drawFormula True <$> [nonEmptyCursorCurrent fs]
              , drawFormula False <$> nonEmptyCursorNext fs
              ]
          ]
      ]

drawFormula :: Bool -> BrewFormula -> Widget RName
drawFormula isSelected = padRight Max . str . prefix . C8.unpack . formulaName
  where prefix = ((if isSelected then " * " else "   ") ++)

selected :: Maybe BrewFormula -> Widget RName
selected maybeSelected =
  withBorderStyle BS.unicodeBold
    . B.border
    . C.vCenter
    . C.hCenter
    . vBox
    $ [ maybe
          ( padLeft (Pad 3)
          . padTop (Pad 1)
          . padBottom Max
          . strWrap
          $ "select a formula by pushing ENTER to display it here with its dependants and dependencies"
          )
          displaySelected
          maybeSelected
      ]

displaySelected :: BrewFormula -> Widget RName
displaySelected formula =
  vBox [displayInfo formula, displayDependencies formula, displayDependants formula]

displayInfo :: BrewFormula -> Widget RName
displayInfo formula =
  B.border -- WithLabel (B.vBorder <+> str " Formula      " <+> B.vBorder)
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

displayDependencies :: BrewFormula -> Widget RName
displayDependencies formula =
  B.border -- WithLabel (B.vBorder <+> str " Dependencies " <+> B.vBorder)
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

displayDependants :: BrewFormula -> Widget RName
displayDependants formula =
  B.border -- WithLabel (B.vBorder <+> str " Usage        " <+> B.vBorder)
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

status :: String -> Maybe BrewError -> Widget RName
status st err =
  withBorderStyle BS.unicodeBold
    . B.border
    . vLimit bottomHeight
    . C.vCenter
    . C.hCenter
    . hBox
    $ [vBox [padLeft (Pad 3) . padRight Max . padBottom Max . str $ maybe st show err]]

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
          , str "   s     : Search formula"
          , str "   i     : Install new"
          , str "   u     : Uninstall selected"
          , str "   U     : Update all"
          , str "   q     : Exit"
          ]
      ]

leftWidth :: Int
leftWidth = 30

bottomHeight :: Int
bottomHeight = 6
