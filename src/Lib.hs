module Lib
    (makeDict,
     Dictionary,
     Puzzle,
     Result,
     isPangram,
     isValid,
     makePuzzle
    ) where

import Data.Set (Set, fromList, size)
import qualified Data.Text as T
import qualified Data.Text.Normalize as N
import Data.Text.ICU.Char

defaultDictionary = "american-english-large"

data Dictionary = Dictionary
  { name :: String
  , words :: Set String
  } deriving (Show)

data Result =
   Valid T.Text Bool |
   Invalid T.Text 
  deriving (Ord, Eq, Show)

data Puzzle = Puzzle Char (Set Char)

makePuzzle :: Char -> String -> Puzzle
makePuzzle c cs =
  if valid then Puzzle c (fromList cs)
  else error "Puzzle input is invalid"
  where
    set = fromList cs
    valid = (length cs) == (size set) && c `notElem` cs

resultFor :: String -> Puzzle -> Result
resultFor cs  p = error "boom"

isValid :: Result -> Bool
isValid (Valid _ _)   = True
isValid (Invalid _) = False

isPangram :: Result -> Bool
isPangram (Valid _ True) = True
isPangram _ = False


makeDict :: Maybe String -> IO Dictionary
makeDict f = toDict <$> readFile fname
  where
    toDict contents = Dictionary fname (fromList $ norm $ lines contents)
    fname = selectDict f
    selectDict (Just f) = f
    selectDict Nothing = defaultDictionary

-- https://stackoverflow.com/questions/44290218/how-do-you-remove-accents-from-a-string-in-haskell
norm :: [String] -> [String]
norm ws = map normalized filtered
  where
    filtered = filter ltthree $ map T.pack ws
    normalized t = T.unpack $ T.filter (not . property Diacritic) $ N.normalize N.NFKD t
    ltthree s = T.length s > 3

