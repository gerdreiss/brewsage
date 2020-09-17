{-# LANGUAGE OverloadedStrings #-}

module Tui.Widgets
  ( title
  , formulas
  , selected
  , status
  )
where

import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import qualified Data.ByteString.Lazy.Char8    as C8

import           Brick.Types                    ( ViewportType(Vertical)
                                                , Padding(Max)
                                                , Widget
                                                )
import           Brick.Widgets.Core             ( viewport
                                                , padRight
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

formulas :: NonEmptyCursor BrewFormula -> Widget UIFormulas
formulas fs =
  withBorderStyle BS.unicodeBold
    . B.border
    . hLimit 30
    . vBox
    . concat
    $ [ drawFormula False <$> (reverse . nonEmptyCursorPrev $ fs)
      , drawFormula True <$> [nonEmptyCursorCurrent fs]
      , drawFormula False <$> nonEmptyCursorNext fs
      , padBottom Max <$> [str "_"]
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
    $ [str $ maybe " selected formula " show sel]

status :: String -> Widget UIFormulas
status st =
  withBorderStyle BS.unicodeBold
    . B.border
    . vLimit 1
    . C.vCenter
    . C.hCenter
    . vBox
    $ [str st]
