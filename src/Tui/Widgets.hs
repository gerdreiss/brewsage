{-# LANGUAGE OverloadedStrings #-}

module Tui.Widgets
  ( title
  , formulas
  , selected
  , status
  , help
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
                                                , padTopBottom
                                                , str
                                                , vBox
                                                , viewport
                                                , withBorderStyle
                                                , vLimit
                                                )
import           Cursor.Simple.List.NonEmpty    ( NonEmptyCursor
                                                , nonEmptyCursorCurrent
                                                , nonEmptyCursorNext
                                                , nonEmptyCursorPrev
                                                )
import           Data.Brew                      ( BrewFormula(..) )
import           Tui.Types                      ( UIFormulas(..)
                                                , IsSelected
                                                )

title :: String -> Widget UIFormulas
title t =
  withBorderStyle BS.unicodeBold
    . B.border
    . vLimit 1
    . C.vCenter
    . C.hCenter
    . vBox
    $ [str t]

formulas :: Int -> NonEmptyCursor BrewFormula -> Widget UIFormulas
formulas nfs fs =
  withBorderStyle BS.unicodeBold
    . B.border
    . hLimit leftWidth
    . vBox
    $ [ viewport UIFormulas Vertical
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

drawFormula :: IsSelected -> BrewFormula -> Widget UIFormulas
drawFormula isSelected = padRight Max . str . prefix . C8.unpack . formulaName
  where prefix = ((if isSelected then " * " else "   ") ++)

selected :: Maybe BrewFormula -> Widget UIFormulas
selected sel =
  withBorderStyle BS.unicodeBold
    . B.border
    . C.vCenter
    . C.hCenter
    . vBox
    $ [ str $ maybe
          "select a formula by pushing ENTER to display it here with its dependants and dependencies"
          show
          sel
      ]

status :: String -> Widget UIFormulas
status st =
  withBorderStyle BS.unicodeBold
    . B.border
    . vLimit bottomHeight
    . C.vCenter
    . C.hCenter
    . hBox
    $ [vBox [padLeft (Pad 3) . padRight Max . padBottom Max $ str st]]

help :: Widget UIFormulas
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
        $ [ str "   ENTER : Show selected"
          , str "   x     : Delete selected"
          , str "   i     : Install new"
          , str "   q     : Exit"
          ]
      ]

leftWidth :: Int
leftWidth = 30

bottomHeight :: Int
bottomHeight = 4
