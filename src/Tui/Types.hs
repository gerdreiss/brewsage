module Tui.Types
  ( RName(..)
  ) where

data RName = Formulas | FormulaInfo | FormulaName deriving (Eq, Ord, Show)
