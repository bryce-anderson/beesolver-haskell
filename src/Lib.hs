module Lib
    (makeDict,
     Dictionary,
     Puzzle,
     Result (..),
     resultFor,
     isPangram,
     isValid,
     makePuzzle,
     solvePuzzle
    ) where

import Data.Set (Set, elems, fromList, member, size)
import qualified Data.Text as T
import qualified Data.Text.Normalize as N
import Data.Text.ICU.Char


data Dictionary = Dictionary String (Set String)
  deriving (Show)

data Result =
   Valid String Bool |
   Invalid String
  deriving (Ord, Eq, Show)

data Puzzle = Puzzle Char (Set Char)
  deriving (Show)

solvePuzzle :: Dictionary -> Puzzle -> [Result]
solvePuzzle (Dictionary _ words) puzzle = filter isValid $ (resultFor puzzle) `map` candidates
  where
    candidates = elems words
  

makePuzzle :: Char -> String -> Puzzle
makePuzzle c cs =
  if valid then Puzzle c (fromList cs)
  else error "Puzzle input is invalid"
  where
    set = fromList cs
    valid = (length cs) == (size set) && c `notElem` cs

resultFor :: Puzzle -> String -> Result
resultFor (Puzzle c req) cs =
  if hasRequired && charsMatch then Valid cs ispan
  else Invalid cs
  where
    hasRequired = elem c cs
    charsMatch = all (\i -> i == c || member i req) cs
    ispan = size (fromList cs) == 7

isValid :: Result -> Bool
isValid (Valid _ _)   = True
isValid (Invalid _) = False

isPangram :: Result -> Bool
isPangram (Valid _ True) = True
isPangram _ = False


makeDict :: String -> IO Dictionary
makeDict fname = toDict <$> readFile fname
  where
    toDict contents = Dictionary fname (fromList $ norm $ lines contents)

-- https://stackoverflow.com/questions/44290218/how-do-you-remove-accents-from-a-string-in-haskell
norm :: [String] -> [String]
norm ws = map normalized filtered
  where
    filtered = filter ltthree $ map T.pack ws
    normalized t = T.unpack $ T.toLower $ T.filter (not . property Diacritic) $ N.normalize N.NFKD t
    ltthree s = T.length s > 3

