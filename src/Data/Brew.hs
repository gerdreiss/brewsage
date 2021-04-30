module Data.Brew where

import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.Char8    as C8
import qualified Data.List.NonEmpty            as NE

import           Cursor.Simple.List.NonEmpty    ( NonEmptyCursor
                                                , makeNonEmptyCursor
                                                )
import           Data.Char                      ( isLetter
                                                , toLower
                                                )
import           Data.List                      ( intercalate )

--
-- | brew formula
--

data BrewFormula = BrewFormula
  { formulaName         :: B.ByteString
  , formulaVersion      :: Maybe B.ByteString
  , formulaInfo         :: Maybe B.ByteString
  , formulaDependencies :: [BrewFormula]
  , formulaDependants   :: [BrewFormula]
  }

instance Eq BrewFormula where
  f1 == f2 = formulaName f1 == formulaName f2

instance Show BrewFormula where
  show formula = concat
    [ "\n"
    , name
    , " "
    , version
    , "\n"
    , "==> Info"
    , "\n"
    , info
    , "\n"
    , "==> Used by"
    , "\n"
    , dependantList
    , "\n"
    , "== Depends on"
    , "\n"
    , dependencyList
    , "\n"
    ]
   where
    name           = C8.unpack $ formulaName formula
    version        = maybe "" (("v" ++) . C8.unpack) . formulaVersion $ formula
    info           = maybe "N/A" C8.unpack . formulaInfo $ formula
    dependencyList = case formulaDependencies formula of
      []       -> "N/A"
      formulas -> formulaNames formulas
    dependantList  = case formulaDependants formula of
      []       -> "N/A"
      formulas -> formulaNames formulas
    formulaNames formulas = intercalate ", " (fmap (C8.unpack . formulaName) formulas)

--
-- | brew error
--

data BrewError = BrewError
  { errorCode    :: Int
  , errorMessage :: B.ByteString
  }
  deriving Eq

instance Show BrewError where
  show err =
    concat ["Error occurred: ", show (errorCode err), " - ", C8.unpack (errorMessage err)]

type ErrorOrFormulas = Either BrewError [BrewFormula]
type ErrorOrFormula = Either BrewError BrewFormula

--
-- | answers
--

data Answer
  = Yes
  | No
  | Quit
  | Que
  deriving (Show)

instance Read Answer where
  readsPrec _ input = case fmap toLower . filter isLetter $ input of
    "quit" -> [(Quit, [])]
    "q"    -> [(Quit, [])]
    "yes"  -> [(Yes, [])]
    "y"    -> [(Yes, [])]
    "no"   -> [(No, [])]
    "n"    -> [(No, [])]
    ""     -> [(No, [])]
    _      -> [(Que, [])]


--
-- | helper functions
--

emptyFormula :: BrewFormula
emptyFormula = BrewFormula { formulaName         = ""
                           , formulaVersion      = Nothing
                           , formulaInfo         = Nothing
                           , formulaDependencies = []
                           , formulaDependants   = []
                           }

emptyFormulaList :: NonEmptyCursor BrewFormula
emptyFormulaList = makeNonEmptyCursor $ NE.fromList [emptyFormula]

mkFormula :: String -> BrewFormula
mkFormula name = emptyFormula { formulaName = C8.pack name }
