module Data.Brew where

import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C8
import           Data.Char                      ( isLetter
                                                , toLower
                                                )
import           Data.List                      ( intercalate )

data BrewFormula = BrewFormula
  { name :: B.ByteString,
    dependencies :: [BrewFormula],
    dependants :: [BrewFormula]
  }

instance Show BrewFormula where
  show formula = concat
    ["\n", C8.unpack . name $ formula, "\n", dependencyList, "\n", dependantList, "\n"]
   where
    dependencyList = case dependencies formula of
      []       -> " has no dependencies"
      formulas -> " depends on " ++ formulaNames formulas
    dependantList  = case dependants formula of
      []       -> " is not used by any other formula"
      formulas -> " is used by " ++ formulaNames formulas
    formulaNames formulas = intercalate ", " (map (C8.unpack . name) formulas)

instance Eq BrewFormula  where
  (==) f1 f2 = name f1 == name f2

data BrewError = BrewError
  { code :: Int,
    message :: B.ByteString
  }

instance Show BrewError where
  show error =
    concat ["Error occurred: code ", show . code $ error, " message ", show . message $ error]

data Answer
  = Yes
  | No
  | Quit
  | Que
  deriving (Show)

instance Read Answer where
  readsPrec _ input = case map toLower . filter isLetter $ input of
    "quit" -> [(Quit, [])]
    "q"    -> [(Quit, [])]
    "yes"  -> [(Yes, [])]
    "y"    -> [(Yes, [])]
    "no"   -> [(No, [])]
    "n"    -> [(No, [])]
    ""     -> [(No, [])]
    _      -> [(Que, [])]
