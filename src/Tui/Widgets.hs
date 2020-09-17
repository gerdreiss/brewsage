{-# LANGUAGE OverloadedStrings #-}

module Tui.Widgets where

import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import qualified Data.ByteString.Lazy.Char8    as C8

import           Brick.Types                    ( Padding(Max)
                                                , Widget
                                                )
import           Brick.Widgets.Core             ( padRight
                                                , padBottom
                                                , hLimit
                                                , str
                                                , vBox
                                                , vLimit
                                                , withBorderStyle
                                                )
import           Cursor.Simple.List.NonEmpty    ( NonEmptyCursor
                                                , nonEmptyCursorCurrent
                                                , nonEmptyCursorNext
                                                , nonEmptyCursorPrev
                                                )
import           Data.Brew                      ( BrewFormula(..) )

type IsSelected = Bool

title :: String -> Widget n
title t =
  withBorderStyle BS.unicodeBold $ B.border $ vLimit 1 $ C.vCenter $ C.hCenter $ vBox
    [str t]

formulas :: NonEmptyCursor BrewFormula -> Widget n
formulas fs =
  withBorderStyle BS.unicodeBold
    . B.border
    . hLimit 30
    . vBox
    . concat
    $ [ map (drawFormula False) . reverse . nonEmptyCursorPrev $ fs
      , [drawFormula True $ nonEmptyCursorCurrent fs]
      , map (drawFormula False) $ nonEmptyCursorNext fs
      , [padBottom Max $ str "_"]
      ]

drawFormula :: IsSelected -> BrewFormula -> Widget n
drawFormula isSelected = padRight Max . str . prefix . C8.unpack . name
  where prefix = ((if isSelected then " * " else "   ") ++)

selected :: Maybe BrewFormula -> Widget n
selected sel =
  withBorderStyle BS.unicodeBold
    . B.border
    . C.vCenter
    . C.hCenter
    . vBox
    $ [str $ maybe " selected formula " show sel]

status :: String -> Widget n
status st =
  withBorderStyle BS.unicodeBold $ B.border $ vLimit 1 $ C.vCenter $ C.hCenter $ vBox
    [str st]
