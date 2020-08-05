module Data.List.Safe where

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> [a]
safeTail []       = []
safeTail (_ : xs) = xs
