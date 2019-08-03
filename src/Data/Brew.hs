module Data.Brew where

import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C8

import           Data.Char                  (isLetter, toLower)
import           Data.List                  (intercalate)

data BrewFormula =
  BrewFormula
    { name       :: B.ByteString
    , dependents :: [BrewFormula]
    }

instance Show BrewFormula where
  show formula =
    (C8.unpack . name $ formula) ++
    case dependents formula of
      []       -> " is not used by any other formula.\n"
      formulas -> " is used by " ++ formulaNames formulas ++ ".\n"
        where formulaNames formulas = intercalate ", " (map (C8.unpack . name) formulas)

data BrewError =
  BrewError
    { code    :: Int
    , message :: B.ByteString
    }

instance Show BrewError where
  show error = "Error occurred: code " ++ (show . code $ error) ++ " message " ++ (show . message $ error)

data Answer
  = Yes
  | No
  | Quit
  | Que
  deriving (Show)

instance Read Answer where
  readsPrec _ input =
    case map toLower . filter isLetter $ input of
      "quit" -> [(Quit, [])]
      "q"    -> [(Quit, [])]
      "yes"  -> [(Yes, [])]
      "y"    -> [(Yes, [])]
      "nope" -> [(No, [])]
      "no"   -> [(No, [])]
      "n"    -> [(No, [])]
      ""     -> [(No, [])]
      _      -> [(Que, [])]
