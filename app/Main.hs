{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import Lib
import Data.Maybe (fromMaybe)
import System.Console.CmdArgs

defaultDictionary = "american-english-large"

data Config = Config
  { dict :: Maybe String
  , wordsoutput :: Bool
  , required :: String
  , others :: String
  } deriving (Show, Data, Typeable)

config = Config
  { dict = Nothing
  , wordsoutput = def &= help "Output matching words" -- &= opt True
  , required    = def &= typ "REQUIRED" &= argPos 0
  , others      = def &= typ "OTHERS"   &= argPos 1
  } &= help "Solve a Puzzle"


getConfig :: IO Config
getConfig = checkValid <$> cmdArgsRun mode
  where 
    mode = cmdArgsMode config
    checkValid m = 
      if invalid then error "args are invalid"
      else m
        where
        invalid = length (required m) /= 1 || length (others m) /= 6

main :: IO ()
main = do
  config <- getConfig
  print config
  let dictName = fromMaybe defaultDictionary (dict config)
  helloMessage
  d <- makeDict dictName
  let puzzle = makePuzzle (head $ required config) (others config)
  let results = solvePuzzle d puzzle
  print puzzle
  showResults config results

showResults :: Config -> [Result] -> IO ()
showResults cfg results = do
  putStrLn $ "Matching words " ++ (show $ length results) 
  if wordsoutput cfg then print $ map translate results
  else return ()
    where
      translate (Valid word True) = word ++ " 🍳"
      translate (Valid word False) = word
      translate (Invalid _ ) = error "shouldn't get here"

helloMessage :: IO ()
helloMessage = do
  putStrLn "🐝"
  putStrLn "Hello and welcome to Spelling Bee Solver"
  putStrLn "🐝🐝"

  putStrLn "🐝🐝🐝"
  -- putStrLn "Required Letter:  ", string(required)
  -- putStrLn "Other Letters:    ", others
  -- putStrLn "Dictionary:       ", dictionaryName
  -- putStrLn "Dictionary words: ", len(dictionary.Words())
  putStrLn "Solving now"
  putStrLn "🐝🐝🐝🐝"

  -- solver := Solver{dictionary, puzzle}
  -- start = time.Now()
  -- solutions := solver.Solve()
  -- elapsedSolve := time.Since(start)

  putStrLn "🐝🐝🐝🐝🐝"
  putStrLn "Solved!"
  putStrLn ""
  -- putStrLn "  Words:", len(solutions)

