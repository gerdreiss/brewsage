{-# LANGUAGE OverloadedStrings #-}

module Data.Brew where

import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C8
import           Data.Char                      ( isLetter
                                                , toLower
                                                )
import           Data.List                      ( intercalate )

type ErrorOrFormulas = Either BrewError [BrewFormula]
type ErrorOrFormula = Either BrewError BrewFormula

data BrewFormula = BrewFormula
  { formulaName :: B.ByteString,
    formulaDependencies :: [BrewFormula],
    formulaDependants :: [BrewFormula]
  }

instance Show BrewFormula where
  show formula = concat
    [ "\n"
    , C8.unpack . formulaName $ formula
    , "\n"
    , dependantList
    , "\n"
    , dependencyList
    , "\n"
    ]
   where
    dependencyList = case formulaDependencies formula of
      []       -> "  has no dependencies"
      formulas -> "  depends on " ++ formulaNames formulas
    dependantList  = case formulaDependants formula of
      []       -> "  is not used by any other formula"
      formulas -> "  is used by " ++ formulaNames formulas
    formulaNames formulas = intercalate ", " (map (C8.unpack . formulaName) formulas)

instance Eq BrewFormula  where
  (==) f1 f2 = formulaName f1 == formulaName f2

data BrewError = BrewError
  { errorCode :: Int,
    errorMessage :: B.ByteString
  }

instance Show BrewError where
  show err = concat
    [ "Error occurred: code "
    , show . errorCode $ err
    , " message "
    , show . errorMessage $ err
    ]

instance Eq BrewError where
  (==) e1 e2 = errorCode e1 == errorCode e2 && errorMessage e1 == errorMessage e2

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

emptyFormula :: BrewFormula
emptyFormula =
  BrewFormula { formulaName = "", formulaDependencies = [], formulaDependants = [] }
