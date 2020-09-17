module Tui.Types
  ( UIFormulas(..)
  , IsSelected
  )
where

type IsSelected = Bool

data UIFormulas = UIFormulas deriving (Eq, Ord, Show)
