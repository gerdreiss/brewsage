{-# LANGUAGE OverloadedStrings #-}
module Tui.Widgets where

import           Brick.Types
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import           Brick.Widgets.Core
import           Cursor.Simple.List.NonEmpty
import           Data.Brew
import qualified Data.ByteString.Lazy.Char8    as C8

title :: String -> Widget n
title t =
  withBorderStyle BS.unicodeBold $ B.border $ vLimit 1 $ C.vCenter $ C.hCenter $ vBox
    [str t]

formulas :: NonEmptyCursor BrewFormula -> Widget n
formulas fs =
  withBorderStyle BS.unicodeBold
    $ B.border
    $ hLimit 20
    $ C.vCenter
    $ C.hCenter
    $ vBox
    $ concat
        [ map (drawFormula False) . reverse . nonEmptyCursorPrev $ fs
        , [drawFormula True $ nonEmptyCursorCurrent fs]
        , map (drawFormula False) $ nonEmptyCursorNext fs
        ]

drawFormula :: Bool -> BrewFormula -> Widget n
drawFormula b = (if b then withAttr "selected" else id) . str . C8.unpack . name

selected :: Maybe BrewFormula -> Widget n
selected sel = withBorderStyle BS.unicodeBold $ B.border $ C.vCenter $ C.hCenter $ vBox
  [str $ maybe " selected formula " show sel]

status :: String -> Widget n
status st =
  withBorderStyle BS.unicodeBold $ B.border $ vLimit 1 $ C.vCenter $ C.hCenter $ vBox
    [str st]
