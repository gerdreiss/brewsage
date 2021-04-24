-- This module was shamelessly stolen from
--  https://github.com/Garmelon/task-machine/blob/master/src/TaskMachine/UI/Popup.hs
module Tui.Popup
  ( Popup
  , popup
  , popup'
  , renderPopup
  , handlePopupEvent
  , popupSelection
  , minPopupWidth
  ) where

import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Dialog          as D
import qualified Graphics.Vty                  as VTY

import           Brick                          ( padAll
                                                , str
                                                , vBox
                                                , vLimit
                                                )
import           Brick.Types                    ( EventM
                                                , Widget
                                                )
import           Data.List                      ( intercalate )

data Popup n r = Popup (D.Dialog r) (Widget n)

popup :: String -> [String] -> [(String, r)] -> Popup n r
popup title contentLines = popup'
  title
  ( B.border
  . vLimit (2 + length contentLines)
  . C.vCenter
  . C.hCenter
  . vBox
  $ [padAll 1 . str . intercalate "\n" $ contentLines]
  )

popup' :: String -> Widget n -> [(String, r)] -> Popup n r
popup' title widget results =
  let spacedTitle = " " ++ title ++ " "
      dialog      = D.dialog (Just spacedTitle) (Just (0, results)) minPopupWidth
  in  Popup dialog widget

renderPopup :: Popup n r -> Widget n
renderPopup (Popup dialog widget) = D.renderDialog dialog widget

handlePopupEvent :: VTY.Event -> Popup n r -> EventM n (Popup n r)
handlePopupEvent e (Popup dialog widget) =
  Popup <$> D.handleDialogEvent e dialog <*> pure widget

popupSelection :: Popup n r -> Maybe r
popupSelection (Popup dialog _) = D.dialogSelection dialog

minPopupWidth :: Int
minPopupWidth = 70
